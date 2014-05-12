(**************************************************************************)
(*  Copyright (C) 2014, Skylable Ltd. <info-copyright@skylable.com>       *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above            *)
(*  copyright notice, this list of conditions and the following           *)
(*  disclaimer in the documentation and/or other materials provided       *)
(*  with the distribution.                                                *)
(*                                                                        *)
(*  3. Neither the name of the copyright holder nor the names of its      *)
(*  contributors may be used to endorse or promote products derived       *)
(*  from this software without specific prior written permission.         *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,            *)
(*  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    *)
(*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR    *)
(*  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)    *)
(*  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,   *)
(*  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)         *)
(*  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED   *)
(*  OF THE POSSIBILITY OF SUCH DAMAGE.                                    *)
(**************************************************************************)

open LRUCacheTypes
module ResultT :
  functor (M : TryMonad) ->
  sig
    type ('ok, 'err) result = OK of 'ok | Error of 'err
    type ('ok, 'err) t = ('ok, 'err) result M.t
    val return : 'a -> ('a, 'b) result M.t
    val fail : 'a -> ('b, 'a) result M.t
    val ( >>= ) :
      ('a, 'b) result M.t ->
      ('a -> ('c, 'b) result M.t) -> ('c, 'b) result M.t
    val lift : ('a -> 'b M.t) -> 'a -> ('b, exn) result M.t
    val unwrap : ('a, exn) result -> 'a M.t
    val catch :
      ('a, 'b) result M.t ->
      ('b -> ('a, 'c) result M.t) -> ('a, 'c) result M.t
  end
module Make :
  functor (M : TryMonad) ->
  sig
    module Result :
    sig
      type ('ok, 'err) result =
        ('ok, 'err) ResultT(M).result =
          OK of 'ok
        | Error of 'err
      type ('ok, 'err) t = ('ok, 'err) result M.t
      val return : 'a -> ('a, 'b) result M.t
      val fail : 'a -> ('b, 'a) result M.t
      val ( >>= ) :
        ('a, 'b) result M.t ->
        ('a -> ('c, 'b) result M.t) -> ('c, 'b) result M.t
      val lift : ('a -> 'b M.t) -> 'a -> ('b, exn) result M.t
      val unwrap : ('a, exn) result -> 'a M.t
      val catch :
        ('a, 'b) result M.t ->
        ('b -> ('a, 'c) result M.t) -> ('a, 'c) result M.t
    end
    type ('a, 'b) t
    val create : int -> ('a, 'b) t
    val get : ('a, 'b) t -> notfound:'b -> string -> ('a, 'b) Result.t
    val set : ('a, 'b) t -> string -> ('a, 'b) Result.t -> unit
    val lookup :
      ('a, exn) t ->
      string -> (string -> 'a M.t) -> ('a, exn) Result.t
    val lookup_exn :
      ('a, exn) t -> string -> (string -> 'a M.t) -> 'a M.t
  end
