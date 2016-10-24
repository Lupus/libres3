(**************************************************************************)
(*  Copyright (C) 2014-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
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

type xml = 'a Markup.node as 'a

val element : ?ns:string -> string -> ?attrs:(Markup.name * string) list ->
  xml list -> xml

val add_opt_element : ?ns:string -> string -> ?attrs:(Markup.name * string) list ->
  ('a -> xml list) -> 'a option -> xml list -> xml list

val opt: ('a -> xml list) -> 'a option -> xml list

val text : string -> xml list

val text_of_int : int -> xml list

val root : string -> ?attrs:(Markup.name * string) list -> xml list -> xml

val to_string : xml -> string

val expect_root : string -> xml option -> (xml list, Rresult.R.msg) result

val is_tag : tag:string -> xml -> bool
