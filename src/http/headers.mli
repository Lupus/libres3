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
