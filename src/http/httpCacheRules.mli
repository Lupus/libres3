(**************************************************************************)
(*  RFC7234 HTTP cache rules                                              *)
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

(** RFC7234 rules *)

type etag = string
type age = Age of float
type cache_req = {
  uri : Uri.t;
  target: string;
  canonical: Uri.t;
  headers : Cohttp.Header.t;
  meth : Cohttp.Code.meth;
  control : CacheControl.t;
  if_none_match : etag list;
} [@@deriving sexp]

(** build cache request from Cohttp *)
val cache_req : Cohttp.Request.t -> cache_req

type cached_response = {
  cache_uri : Uri.t;
  cache_headers : Cohttp.Header.t;
  cache_control : CacheControl.t;
  cache_status : Cohttp.Code.status_code;
  response_time : float;
  corrected_initial_age : float;
  freshness_lifetime : float;
  etag : string option;
  validated : bool;
} [@@deriving sexp]

(** [want_store cache_req status resp_headers] determines whether it is useful
   to store this reply in the cache *)
val want_store : cache_req -> Cohttp.Code.status_code -> Cohttp.Header.t -> bool

(** [can_store cache_req status resp_headers] determines whether RFC7234 rules
   would allow this reply to be stored *)
val can_store : cache_req -> Cohttp.Code.status_code -> Cohttp.Header.t -> bool

(** [cached_response ~request_time ~has_auth uri response] builds a cachable
   response from Cohttp response *)
val cached_response :
  request_time:float -> has_auth:bool -> Uri.t -> Cohttp.Response.t -> cached_response

(** [current_age cached_response] determines the current age of the cached entry *)
val current_age : cached_response -> age

(** [can_use ~current_age cache_req cached_response] determines whether
   RFC7234 rules allow this cached response to be used *)
val can_use : current_age:age -> cache_req -> cached_response -> bool

(** [freshen ~old ~latest] updates a cached response after a new origin query *)
val freshen : old:cached_response option -> latest:cached_response -> cached_response option

(** [cache_conditional_notmodified cache_req cached_response] determines whether
   we can answer with 304 Not modified *)
val cache_conditional_notmodified : cache_req -> cached_response -> bool
