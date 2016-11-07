(**************************************************************************)
(*  HTTP header helpers                                                   *)
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
