(**************************************************************************)
(*  Copyright (C) 2012-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for     *)
(*  any purpose with or without fee is hereby granted, provided that the  *)
(*  above copyright notice and this permission notice appear in all       *)
(*  copies.                                                               *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL         *)
(*  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED         *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE      *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL  *)
(*  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA    *)
(*  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER     *)
(*  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR      *)
(*  PERFORMANCE OF THIS SOFTWARE.                                         *)
(**************************************************************************)

open Cohttp
open Headers
open Sexplib.Std

(* HTTP caching rules implemented as close as possible to the
   specification in RFC7234 *)

let is_status_understood = function
  | `Code _ -> false
  | `OK | `Not_modified | `Not_found -> true
  | #Code.status_code -> false (* TODO:support more codes *)

type etag = string [@@deriving sexp]
type age = Age of float

type cache_req = {
  uri: Uri.t;
  target: string;
  canonical: Uri.t;
  headers: Header.t;
  meth: Code.meth;
  control: CacheControl.t;
  if_none_match: etag list;
} [@@deriving sexp]

let cache_req r =
  let headers = Request.headers r in
  let uri = Request.uri r in
  {
    uri; target = Request.resource r; canonical = Uri.canonicalize uri;
    headers; meth = Request.meth r;
    control = CacheControl.of_header headers;
    if_none_match = Headers.get_all headers "if-none-match"
  }

type cached_response = {
  cache_uri: Uri.t;
  cache_headers: Header.t;
  cache_control: CacheControl.t;
  cache_status: Code.status_code;
  response_time: float;
  corrected_initial_age: float;
  freshness_lifetime: float;
  etag: string option;
  validated: bool;
} [@@deriving sexp]

let has_nocache req =
  CacheControl.(has_field no_cache req.control) ||
  List.mem "no-cache" (Header.get_multi req.headers "pragma")

(* rfc7234#section-4.2.3 *)
let current_age reply =
  let now = Unix.gettimeofday () in
  let resident_time = now -. reply.response_time in
  Age (max 0. (reply.corrected_initial_age +. resident_time))

let is_fresh ~current_age req reply =
  Logs.debug (fun m -> m "corrected_initial_age: %f, age: %f, freshness_lifetime: %f"
        reply.corrected_initial_age
        current_age reply.freshness_lifetime
    );
  reply.freshness_lifetime > current_age &&
  (match CacheControl.(get_field max_age req.control) with
   | Some max_age ->
     let s = max_age in
     let max_age = CacheControl.delta_seconds_of_string max_age in
     Logs.debug (fun m -> m "checking max-age (%s) %d, %f" s max_age current_age);
     max_age >= int_of_float (current_age +. 0.5)
   | None -> true) &&
  (match CacheControl.(get_field min_fresh req.control) with
   | Some min_fresh -> float (CacheControl.delta_seconds_of_string min_fresh) +. current_age <= reply.freshness_lifetime
   | None -> true)

let get_control_freshness control () =
  match (match CacheControl.(get_field s_maxage control) with
      | Some _ as v -> v
      | None -> CacheControl.(get_field max_age control)) with
  | Some v ->
    Printf.eprintf "v: %s\n%!" v;
    Some (float (CacheControl.delta_seconds_of_string v))
  | None -> None

let get_expires_freshness h ~date_value () =
  match get_expires h with
  | Some expires ->
    Some (expires -. date_value)
  | None -> None

(* TODO: configurable *)
let heuristic_fraction = 10.
let max_heuristic_freshness_lifetime = 86400.

let range v (a, b) =
  if v < a then a
  else if v > b then b
  else v

(* rfc7234#section-4.2.2 *)
let get_heuristic_freshness ~has_auth ~code ~date_value ~last_modified () =
  if code <> `OK || has_auth then None
  else match last_modified with
    | Some last_modified ->
      Some (range ((date_value -. last_modified) *. heuristic_fraction /. 100.) (0., max_heuristic_freshness_lifetime))
    | None -> None

let (|||) a b () =
  match a () with
  | Some _ as v -> v
  | None -> b ()

(* rfc7234#page-6 *)
let can_store req status resp_headers =
  let resp_control = CacheControl.of_header resp_headers in
  (* POST would be cachable too, but we don't implement it *)
  let meth_cachable = (req.meth = `GET || req.meth = `HEAD) in
  let understood = is_status_understood status in
  let has_no_store = CacheControl.(has_field no_store req.control) ||
                     CacheControl.(has_field no_store resp_control) in
  let has_private = CacheControl.(has_field private_ resp_control) in
  let auth_storable = can_store_auth req.headers resp_control in
  let has_expires = get_expires resp_headers <> None in
  let has_max_age = CacheControl.(has_field max_age resp_control) in
  let has_s_maxage = CacheControl.(has_field s_maxage resp_control) in
  let status_cachable = is_status_cacheable status in
  let has_public = CacheControl.(has_field public resp_control) in
  let result =
    meth_cachable && understood && not has_no_store && not has_private &&
    auth_storable &&
    (has_expires || has_max_age || has_s_maxage || status_cachable || has_public)
  in
  Logs.debug (fun m -> m "can_store: %b; meth:%b,understood:%b,has_nostore:%b,has_private:%b;auth_storable:%b;has_expires:%b;has_maxage:%b;has_s_maxage:%b; status_cachable:%b;has_public:%b" result
            meth_cachable understood has_no_store has_private auth_storable has_expires has_max_age has_s_maxage status_cachable has_public
        );
  result

let want_store req status resp_headers =
  req.meth <> `HEAD &&
  can_store req status resp_headers &&
  status <> `Not_modified &&
  (get_etag resp_headers <> None || get_last_modified resp_headers <> None)

let cached_response ~request_time ~has_auth uri reply =
  let h = Response.headers reply in
  let control = CacheControl.of_header h in
  let code = Response.status reply in
  (* rfc7234#section-4.2.3 *)
  let age_value = float (get_age h) in
  let date_value = try get_date h with Failure _ -> Unix.gettimeofday () in
  let response_time = Unix.gettimeofday () in
  let apparent_age = max 0. (response_time -. date_value) in
  let response_delay = response_time -. request_time in
  let corrected_age_value = age_value +. response_delay in
  Logs.debug (fun m -> m "apparent age: %f, corrected_age_value: %f (%f+%f)" apparent_age corrected_age_value age_value response_delay);
  let corrected_initial_age = max apparent_age corrected_age_value in
  let last_modified = get_last_modified h in
  prerr_endline "Z1";
  (* TODO: no-cache/no-store, etc. *)
  let freshness_lifetime = match ((get_control_freshness control) |||
                                  (get_expires_freshness h ~date_value) |||
                                  (get_heuristic_freshness ~has_auth ~code ~date_value ~last_modified)) () with
  | Some v -> v
  | None -> 0. in
  prerr_endline "Z2";
  let etag = get_etag h in
  {
    cache_uri = uri;
    cache_control = control;
    cache_status = code;
    cache_headers = h;
    response_time; corrected_initial_age; freshness_lifetime;
    etag; validated = true
  }

let allow_stale ~current_age req resp =
  let open CacheControl in
  let max_stale = match CacheControl.(get_field max_stale req.control) with
    | Some v -> CacheControl.delta_seconds_of_string v
    | None -> if CacheControl.(has_field max_age req.control) then 0 else max_int
  in
  not (List.exists (fun f -> has_field f resp.cache_control) [
      no_store;
      no_cache;
      must_revalidate;
      s_maxage;
      proxy_revalidate
    ]) &&
  resp.freshness_lifetime -. current_age <= float max_stale

let can_use ~current_age req cached_response =
  let Age current_age = current_age in
  let vary_match header =
    let v = Header.get req.headers header in
    v = Header.get cached_response.cache_headers header && v != Some "*"
  in
  let uri_match = Uri.compare (Uri.with_scheme req.canonical None) (Uri.with_scheme cached_response.cache_uri None) = 0 in
  let vary_match = List.for_all vary_match (get_vary req.headers) in
  let cachable_req = (not (has_nocache req) || cached_response.validated) in
  let cachable_resp = (not CacheControl.(has_field no_cache cached_response.cache_control) || cached_response.validated) in
  let fresh = is_fresh ~current_age req cached_response
  and stale_ok = allow_stale ~current_age req cached_response
  and validated = cached_response.validated in
  Logs.debug (fun m -> m "uri match: %b, vary match: %b, cachable req: %b, cachable resp: %b, fresh: %b, stale_ok: %b, validated: %b"
        uri_match vary_match cachable_req cachable_resp fresh stale_ok validated
    );
  if not uri_match then
    Logs.debug (fun m -> m "URI: %S != %S" (Uri.to_string req.canonical) (Uri.to_string cached_response.cache_uri));
  uri_match && vary_match && cachable_req &&  cachable_resp && (fresh || stale_ok || validated)


(* rfc7234#section-4.3.4  *)
let freshen ~old ~latest =
  match old with
  | None -> Some latest
  | Some old ->
    if latest.etag <> None && old.etag = latest.etag then
      (* TODO: delete 1xx Warn headers and retain 2xx Warn headers *)
      let old_headers = old.cache_headers in
      let replace k v h = Header.replace h k v in
      Some { old with cache_headers = Header.fold replace latest.cache_headers old_headers; validated = true }
    else
      None

let cache_conditional_notmodified req cresp =
  match req.if_none_match with
  | ["*"] -> true
  | (_ :: _) as etags ->
    (match cresp.etag with
    | None -> false
    | Some etag -> List.mem etag etags)
  | [] ->
    match Header.get req.headers "if-modified-since" with
    | None -> false
    | Some ims ->
      let ims = date_of_string ims in
      let date = try Some (Headers.get_date cresp.cache_headers) with _ -> None in
      match Headers.get_last_modified cresp.cache_headers, date with
      | Some lm, _ -> lm <= ims
      | None, Some date -> date <= ims
      | None, None -> cresp.response_time <= ims
