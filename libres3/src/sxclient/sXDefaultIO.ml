(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

open Lwt
exception Detail of exn * (string * string) list
type output_data = string * int * int
type input_stream = unit -> output_data Lwt.t
type output_stream = output_data -> unit Lwt.t

type entry = {
  name: string;
  size: int64;
  mtime: float;
  etag: string;
}

type source = {
  meta: entry;
  seek: ?len:int64 -> int64 -> input_stream Lwt.t
}

type acl = [`Grant | `Revoke] * [`UserName of string] * [`Owner | `Manager | `Read | `Write] list
let read_string str ?len pos =
  let eof = ref false in
  return (fun () ->
      if !eof then return ("",0,0)
      else begin
        eof := true;
        let len = match len with
          | None -> Int64.sub (Int64.of_int (String.length str)) pos
          | Some len -> len in
        if len < 0L then return ("",0,0)
        else return (str, Int64.to_int pos, Int64.to_int len)
      end
    );;
let of_string str = `Source {
    meta = {
      name = "";
      size = Int64.of_int (String.length str);
      mtime = Unix.gettimeofday ();
      etag = Digest.to_hex (Digest.string str)
    };
    seek = read_string str;
  }

let rec iter stream f =
  stream () >>= fun (str,pos,len) ->
  f (str,pos,len) >>= fun () ->
  if len = 0 then return ()
  else iter stream f
;;
