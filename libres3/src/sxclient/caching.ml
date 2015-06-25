(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

open EventLog
module MCache = LRUCacheMonad.Make(Lwt)
let cache = MCache.create

type 'a cached = {
  response_time: float;
  etag: Nethttp.etag option;
  result: 'a
}

let cache n = MCache.create n

let cached_result ~request_time reply =
  let h = reply.Http.headers in
  (* rfc7234#section-4.2.3 *)
  let response_time = Unix.gettimeofday () in
  let etag = try Some (Nethttp.Header.get_etag h) with Not_found -> None in
  {
    response_time; etag; result = ()
  }

let is_fresh ?(heuristic_freshness=0.) reply =
  if reply.etag = None then
    let now = Unix.gettimeofday () in
    let delta = now -. reply.response_time in
    debug (fun () -> Printf.sprintf "age: %f" delta);
    delta < heuristic_freshness
  else
    false (* always revalidate replies with ETag *)

open Lwt

exception UnCachable of Http.reply

let print_etag () = function
  | Some (`Strong etag | `Weak etag) -> Printf.sprintf "(Etag: %s)" etag
  | None -> ""

let make_cached_request cache ?heuristic_freshness ~fetch ~parse key =
  Lwt.catch (fun () ->
      with_label key (fun () ->
          MCache.lookup_exn ~is_fresh:(is_fresh ?heuristic_freshness) cache key ~revalidate:(fun (_, old) ->
              let etag = match old with Some { etag; _ } -> etag | None -> None in
              let request_time = Unix.gettimeofday () in
              fetch ?etag key >>= fun reply ->
              let cached = cached_result ~request_time reply in
              match reply.Http.code, old with
              | (200 | 203 | 204 | 206 | 300 | 301 | 404 | 405 | 410 | 414 | 501), _ ->
                with_label "parsing" (fun () ->
                    parse reply) >|= fun result -> { cached with result = result }
              | 304, Some old ->
                debug (fun () -> "<- HTTP 304 (cached)");
                return { cached with result = old.result}
              | 304, None ->
                error "Unexpected 304";
                Lwt.fail (Invalid_argument "unexpected 304")
              | n, _ ->
                debug (fun () -> Printf.sprintf "<- HTTP %d NOT cachable" n);
                Lwt.fail (UnCachable reply)
            ) >|= fun reply ->
          debug (fun () ->
              Printf.sprintf "result%a" print_etag reply.etag;);
          reply.result
        )) (function UnCachable reply -> parse reply | e -> Lwt.fail e)


(* TODO: config *)
let max_global_freshness = 3600.
let make_global_cached_request cache ~fetch ~parse key =
  make_cached_request cache ~heuristic_freshness:max_global_freshness ~fetch ~parse key

let make_private_cached_request cache ~fetch ~parse url =
  (* user is part of URL, so one user can't access the other ones cache *)
  make_cached_request cache ~fetch ~parse (Neturl.string_of_url url)

let invalid = MCache.Result.fail Not_found

let invalidate_cached cache key =
  MCache.set cache key invalid
