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

type 'a node = {
  v: 'a;
  parent: 'a t;
  mutable prev: 'a node option;
  mutable next: 'a node option;
}
and 'a t = {
  mutable head: 'a node option;
  mutable tail: 'a node option;
}

let create () = {
  head = None;
  tail = None;
}

let tail l = match l.tail with
  | None -> invalid_arg "list is empty"
  | Some tail -> tail

let add_head l v =
  let node = { v = v; prev = None; next = l.head; parent = l } in
  begin match l.head with
    | None -> l.tail <- Some node
    | Some old_head ->
      old_head.prev <- Some node;
  end;
  l.head <- Some node;
  node;;

let add_tail l v =
  let node = { v = v; prev = l.tail; next = None; parent = l } in
  begin match l.tail with
    | None -> ()
    | Some old_tail ->
      old_tail.next <- Some node
  end;
  l.tail <- Some node;
  node;;

let remove v =
  (* remove from double-linked list *)
  begin match v.prev with
    | None ->
      (* this was head *)
      v.parent.head <- v.next;
    | Some prev ->
      prev.next <- v.next
  end;
  begin match v.next with
    | None ->
      (* this was tail *)
      v.parent.tail <- v.prev;
    | Some next ->
      next.prev <- v.prev
  end;
  v.v
;;
