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

(* 2Q: A Low Overhead High Performance Buffer Management Replacement Algorithm
 * Theodore Johnson, Dennis Shasha
 * 1994
*)

module Make(Key:Map.OrderedType) = struct
  module KMap = Map.Make(Key)
  type 'a kind =
    | Amain of 'a * Key.t DLinkedList.node
    | A1in of (Key.t * 'a)
    | A1out of (Key.t, 'a) WeakBuffer.id

  (* TODO: use bytes for accounting! *)
  type 'a cache = {
    amain: Key.t DLinkedList.t;
    a1in: (Key.t * 'a) Queue.t;
    a1out: Key.t Queue.t;
    buf: (Key.t, 'a) WeakBuffer.t;
    mutable map: 'a kind KMap.t;
    mutable a1in_size: int;
    mutable a1out_size: int;
    mutable amain_size: int;
    kin: int;
    kout: int;
    total_size: int;
  }

  let create n =
    let kout = n / 2 in {
    amain = DLinkedList.create ();
    a1in = Queue.create ();
    a1out = Queue.create ();
    buf = WeakBuffer.create kout;
    map = KMap.empty;
    a1in_size = 0;
    a1out_size = 0;
    amain_size = 0;
    kin = n / 4;
    kout = kout;
    total_size = n
  }

  open DLinkedList

  let add_a1out cache key value =
    let element = A1out (WeakBuffer.add cache.buf key value) in
    Queue.push key cache.a1out;
    cache.a1out_size <- cache.a1out_size + 1;
    if cache.a1out_size > cache.kout then begin
      cache.map <- KMap.remove (Queue.pop cache.a1out) cache.map;
      cache.a1out_size <- cache.a1out_size - 1;
    end;
    cache.map <- KMap.add key element cache.map;;

  let add_a1in cache key value =
    Queue.push (key,value) cache.a1in;
    let element = A1in (key,value) in
    cache.map <- KMap.add key element cache.map;
    cache.a1in_size <- cache.a1in_size + 1
  ;;

  let add_main cache key value =
    let node = add_head cache.amain key in
    let element = Amain (value, node) in
    cache.map <- KMap.add key element cache.map;
    cache.amain_size <- cache.amain_size + 1
  ;;

  let reclaim cache =
    if (cache.a1in_size + cache.a1out_size + cache.amain_size < cache.total_size) then
      ()
    else if cache.a1in_size > cache.kin then begin
      let ykey,yval = Queue.pop cache.a1in in
      cache.a1in_size <- cache.a1in_size - 1;
      cache.map <- KMap.remove ykey cache.map;
      add_a1out cache ykey yval
    end else begin
      let y = remove (tail cache.amain) in
      cache.map <- KMap.remove y cache.map;
      cache.amain_size <- cache.amain_size - 1
      (* do not put it on A1out, it hasn't been accessed for a while *)
    end
  ;;

  let find cache key =
    match KMap.find key cache.map with
    | Amain (data, node) ->
        cache.map <- KMap.remove key cache.map;
        ignore (remove node);
        add_main cache key data;
        data
    | A1in (_, data) ->
        data
    | A1out id ->
        begin match WeakBuffer.get cache.buf id with
        | Some data ->
            reclaim cache;
            add_main cache key data;
            data
        | None ->
            raise Not_found
        end
    ;;

  let replace cache key data =
    try
      match KMap.find key cache.map with
      | Amain (_,node) ->
        cache.map <- KMap.add key (Amain (data, node)) cache.map
      | A1in _ ->
        cache.map <- KMap.add key (A1in (key, data)) cache.map
      | A1out _ ->
          reclaim cache;
          add_main cache key data
    with Not_found ->
      reclaim cache;
      add_a1in cache key data;
  ;;
end
