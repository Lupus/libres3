(**************************************************************************)
(*  Copyright (C) 2014-2015, Skylable Ltd. <info-copyright@skylable.com>  *)
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

module type Result = sig include LRUCacheTypes.Result end
module Make(R : Result) = struct
  module LRUMap = LRU.Make(String)
  type ('ok, 'err) t = {
    map: ('ok, 'err) R.t LRUMap.cache;
    return_true: (bool, 'err) R.t;
    return_none: ('ok option, 'err) R.t
  }
  type ('ok, 'err) validator = ((string * 'ok option) -> ('ok, 'err) R.t)

  let create n =
    let return_true = R.return true
    and return_none = R.return None in
    { map = LRUMap.create n; return_true; return_none }

  open R
  let return_some v = return (Some v)

  let get cache ~notfound key =
    try LRUMap.find cache.map key
    with _ -> R.fail notfound

  let get_opt cache key : ('a option, 'b) R.t =
    try LRUMap.find cache.map key >>= return_some
    with _ -> cache.return_none

  let set cache = LRUMap.replace cache.map

  let compute_value cache (key, old) f =
    (*  start computing of the value:
        key was not found in cache, the previous computation failed or validation failed *)
    let pending = f (key, old) in
    (* store pending value in cache *)
    set cache key pending;
    (* return pending value *)
    pending >>= fun final ->
    (* put final value into cache *)
    let result = return final in
    set cache key result;
    result

  let always _ = true
  let lookup cache key ?(is_fresh=always) ~revalidate =
    let return_none _ = cache.return_none in
    (* return value from cache if it exists *)
    R.catch (get_opt cache key) return_none >>= (function
        | Some data ->
          if is_fresh data then return data
          else compute_value cache (key, Some data) revalidate
        | None ->
          compute_value cache (key, None) revalidate
      )
end
