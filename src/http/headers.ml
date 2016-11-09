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
open CalendarLib

let fold_split accum v =
  List.rev_append (Stringext.split v ~on:',' |> List.rev_map String.trim) accum

let get_all h k =
  Header.get_multi h k |>
  List.fold_left fold_split []

let get_vary h = match Header.get h "vary" with Some v -> [v] | None -> []

let imf_fixdate = "%a, %d %b %Y %H:%M:%S GMT"
let rfc850 = "%A, %d-%b-%y %H:%M:%S GMT"
let asctime = "%c"

let date_of_string s =
  let date =
    try Printer.Calendar.from_fstring imf_fixdate  s
    with Invalid_argument _ ->
      try Printer.Calendar.from_fstring rfc850 s
      with Invalid_argument _ -> Printer.Calendar.from_fstring asctime s in
  Calendar.to_unixfloat date

let string_of_date d =
  Printer.Calendar.sprint imf_fixdate d

let get_age h = match Header.get h "age" with
  | Some v -> CacheControl.delta_seconds_of_string v
  | None -> 0

let get_date h = match Header.get h "date" with
  | Some date -> date_of_string date
  | None -> failwith "Date header is missing"

let get_last_modified h = match Header.get h "last-modified" with
  | Some date -> Some (date_of_string date)
  | None -> None

let get_expires h = match Header.get h "expires" with
  | Some date -> Some (try date_of_string date with _ -> 0.)
  | None -> None

let get_etag h = Header.get h "etag"

let hop_by_hop = [
  "Connection"; "Keep-Alive"; "Proxy-Authenticate";
  "Proxy-Authorization"; "TE"; "Trailers"; "Transfer-Encoding"; "Upgrade"
]

(* TODO: remove headers nominated by Connection: too *)
let remove_hop_by_hop h =
  List.fold_left Header.remove h hop_by_hop

let add_age h age =
  Header.replace h "Age" (string_of_int (int_of_float (age +. 0.5)))

let now () = string_of_date (Calendar.to_gmt (Calendar.now ()))

let add_date h date =
  Header.add h "Date" date

let add_server h v =
  Header.replace h "Server" v

let append_via h via =
  match Header.get h "via" with
  | Some old -> Header.replace h "Via" (old ^ ", " ^ via)
  | None -> Header.add h "Via" via

(* rfc7231#section-6.1 *)
type cacheable_2xx = [ `OK | `Non_authoritative_information | `No_content |
                       `Partial_content ]
type cacheable_3xx = [ `Found | `Moved_permanently ]
type cacheable_4xx = [ `Not_found | `Method_not_allowed | `Gone |
                       `Request_uri_too_long ]
type cacheable_5xx = [ `Not_implemented ]

let is_status_cacheable = function
  | #cacheable_2xx | #cacheable_3xx | #cacheable_4xx | #cacheable_5xx -> true
  | #Code.status_code -> false

(* rfc7234#section-3.2 *)
let can_store_auth req_headers resp_control =
  match Header.get_authorization req_headers with
  | None -> true
  | Some _ ->
    Logs.debug (fun m -> m "response cache control: %s" (Sexplib.Sexp.to_string_hum (CacheControl.sexp_of_t resp_control)));
    CacheControl.(has_field must_revalidate resp_control) ||
    CacheControl.(has_field public resp_control) ||
    CacheControl.(has_field s_maxage resp_control)

let asterisk_form = Uri.of_string "*"

let resolve_proxy_uri meth uri =
  let connect = Uri.make ?scheme:(Uri.scheme uri) ?host:(Uri.host uri) ?port:(Uri.port uri) () in
  let origin_form =
    if (meth = `OPTIONS && Uri.path uri = "" && Uri.query uri = []) then
      asterisk_form
    else
      Uri.make ~path:(Uri.path uri) ~query:(Uri.query uri) ()
  in
  connect, origin_form
