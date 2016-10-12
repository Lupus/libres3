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

type +'a t

type generated
type parsed
type json = Json_repr.ezjsonm

(* Errors *)
module Error : sig
  type range = (int * int) * (int * int)
  type t = Syntax of Jsonm.error | Expected of string * Jsonm.lexeme
  exception Json of range * t
  val pp : t Fmt.t
end

(* parse JSON *)
val of_strings : ?encoding:[< Jsonm.encoding] -> string Lwt_stream.t -> parsed t

(* extract an object substream, you have to consume the stream before using [t] again *)
val expect_object: parsed t -> (string * parsed t) Lwt_stream.t Boundedio.t

(* extract an array substream, you have to consume the stream before using [t] again *)
val expect_array: parsed t -> parsed t Boundedio.t

(* expect EOF *)
val expect_eof : parsed t -> unit Boundedio.t

(* consume the (sub)stream *)
val drain : parsed t -> unit Boundedio.t

(* build JSON tree *)
val to_json : 'a t -> json Boundedio.t

(* serialize JSON *)
val of_json : json -> generated t

(* encode JSON *)
val to_strings : ?minify:bool -> 'a t -> string Lwt_stream.t
