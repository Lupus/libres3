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

open Result
type +'a t = 'a Lwt.t

val return : 'a -> 'a t

val return_unit : unit t
val return_some : 'a -> 'a option t
val return_none : 'a option t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (>>>) : 'a t -> (('a, exn) result -> 'b t) -> 'b t

val ignore : _ t -> unit t
val fail : exn -> _ t
val try_with : (unit -> 'a t) -> 'a t

val raise : [> `Use_fail_instead]

val yield : unit -> unit t

val run : 'a t -> 'a

val delay : float -> unit t
