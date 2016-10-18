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

type 'a encoding = 'a Json_encoding.encoding

module Int53 : sig
  type t = private int64
  val max_int : t
  val min_int : t
  val of_int64_exn : int64 -> t
  val to_int64 : t -> int64
  val of_float_exn : float -> t
  val to_float : t -> float
  val to_string : t -> string
  val of_string_exn : string -> t
  val pp : t Fmt.t
  val encoding : t encoding
end

val http_date : Http_date.t encoding

val ipaddr : Ipaddr.t encoding

(* an object with a single key *)
val singleton : ('a -> string) -> (string -> 'a) -> 'b encoding -> ('a * 'b) encoding

(* accepts additional fields for non-strict parsing *)
val obj_opt : 'a encoding -> 'a encoding

type ('a, 'b) streaming
val obj_streaming : 'a encoding -> string -> 'b encoding -> ('a, 'b) streaming

open Jsonio
val decode : ('a, 'b) streaming -> lexeme t -> ('a * (string * 'b) Lwt_stream.t) Boundedio.t
val encode : ('a, 'b) streaming -> ('a * (string * 'b) Lwt_stream.t) -> [> `Os] t
