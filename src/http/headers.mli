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

val get_all : Cohttp.Header.t -> string -> string list
val get_vary : Cohttp.Header.t -> string list
val imf_fixdate : string
val rfc850 : string
val asctime : string
val date_of_string : string -> float
val string_of_date : CalendarLib.Printer.Calendar.t -> string
val get_age : Cohttp.Header.t -> CacheControl.delta_seconds
val get_date : Cohttp.Header.t -> float
val get_last_modified : Cohttp.Header.t -> float option
val get_expires : Cohttp.Header.t -> float option
val get_etag : Cohttp.Header.t -> string option
val hop_by_hop : string list
val remove_hop_by_hop : Cohttp.Header.t -> Cohttp.Header.t
val add_age : Cohttp.Header.t -> float -> Cohttp.Header.t
val now : unit -> string
val add_date : Cohttp.Header.t -> string -> Cohttp.Header.t
val add_server : Cohttp.Header.t -> string -> Cohttp.Header.t
val append_via : Cohttp.Header.t -> string -> Cohttp.Header.t
type cacheable_2xx =
  [ `No_content | `Non_authoritative_information | `OK | `Partial_content ]
type cacheable_3xx = [ `Found | `Moved_permanently ]
type cacheable_4xx =
  [ `Gone | `Method_not_allowed | `Not_found | `Request_uri_too_long ]
type cacheable_5xx = [ `Not_implemented ]
val is_status_cacheable : [< Cohttp.Code.status_code ] -> bool
val can_store_auth : Cohttp.Header.t -> CacheControl.t -> bool

val resolve_proxy_uri : Cohttp.Code.meth -> Uri.t -> Uri.t * Uri.t
