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
module ResultT(M: TryMonad) = struct
  type ('ok, 'err) result = OK of 'ok | Error of 'err
  type ('ok, 'err) t = ('ok, 'err) result M.t
  let return v = M.return (OK v)
  let fail v = M.return (Error v)
  let (>>=) m f =
    M.(>>=) m (function OK v -> f v | Error e -> fail e)
  let lift f v =
    M.try_bind (fun () -> f v) return fail
  let unwrap = function
    | OK v -> M.return v
    | Error e -> M.fail e

  let catch m g =
    M.(>>=) m (function OK _ as r -> M.return r | Error e -> g e)
end

module Make(Monad : TryMonad) = struct
  module Result = ResultT(Monad)
  module M = LRUCacheResult.Make(Result)
  type ('ok, 'err) t = ('ok, 'err) M.t
  let create = M.create
  let get = M.get
  let set = M.set
  let lookup cache key f =
    M.lookup cache key (Result.lift f)
  let lookup_exn (cache:('a,exn) t) key f =
    Monad.(>>=) (lookup cache key f) Result.unwrap
end
