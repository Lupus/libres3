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

type t [@@deriving sexp]
val of_header : Cohttp.Header.t -> t

type field
val max_age : field
val max_stale : field
val min_fresh : field
val no_cache : field
val no_store : field
val no_transform : field
val only_if_cached : field
val must_revalidate : field
val public : field
val private_ : field
val proxy_revalidate : field
val s_maxage : field

val has_field : field -> t -> bool
val get_field : field -> t -> string option

val add_field_opt: Cohttp.Header.t -> field -> string option -> Cohttp.Header.t

type delta_seconds = int
val delta_seconds_of_string : string -> delta_seconds

type field_name = string
val field_name_of_string : string option -> field_name list
