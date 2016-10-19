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

type json_primitive = [`String of string | `Float of float | `Bool of bool |
                       `Null]
type json = [json_primitive | `O of (string * json) list | `A of json list]
type lexeme = Jsonm.lexeme
type field_stream = (string * lexeme t) Lwt_stream.t

(* Errors *)
module Error : sig
  type range = (int * int) * (int * int)
  type t = Syntax of Jsonm.error | Expected of string * Jsonm.lexeme
  exception Json of range option * t
  val pp : t Fmt.t
end

(* parse JSON from string *)
val of_string : ?encoding:[< Jsonm.encoding] -> string -> [> `Os | `As] t

(* parse JSON from stream *)
val of_strings : ?encoding:[< Jsonm.encoding] -> string Lwt_stream.t -> [> `Os | `As] t

(* extract an object substream,
   you have to consume the stream before using [t] again *)
val expect_object: lexeme t -> [> `Os] t Boundedio.t

val fields : [< `Os] t -> field_stream

val build_array : [< lexeme] t Lwt_stream.t -> [> `As] t
val build_object : (string * [< lexeme] t) Lwt_stream.t -> [> `Os] t

(* [extract_fields fields streamingfield stream]
   Builds a JSON tree from [fields], and returns the value stream for
   [streamingfield].
   An optimized implementation is used if [streamingfield] is last.
*)
val extract_fields : string list -> string -> field_stream ->
  (json * lexeme t option) Boundedio.t

(* extract an array substream,
   you have to consume the stream before using [t] again *)
val expect_array: lexeme t -> [> `As] t Boundedio.t

val elements : [< `As] t -> lexeme t Lwt_stream.t

(* extract a non-nested value *)
val expect_primitive: lexeme t -> [> json_primitive] Boundedio.t

(* expect EOF *)
val expect_eof : lexeme t -> unit Boundedio.t

val observe : prefix:string -> 'a t -> 'a t

(* consume the (sub)stream *)
val drain : 'a t -> unit Boundedio.t

(* build JSON tree *)
val to_json : 'a t -> json Boundedio.t

(* serialize JSON *)
val of_json : json -> lexeme t

(* encode JSON *)
val to_strings : ?minify:bool -> [< `Os | `As] t -> string Lwt_stream.t

val to_string : ?minify:bool -> [< `Os | `As] t -> string Boundedio.t

val append : 'a t -> 'a t -> 'a t

val equal : json -> json -> bool
