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
