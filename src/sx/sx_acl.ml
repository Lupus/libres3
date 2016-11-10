(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2016 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open Json_encoding

module Priv = struct
  type t = [`Read | `Write | `Owner | `Manager]
  let encoding = string_enum [
      "read", `Read;
      "write", `Write;
      "owner", `Owner;
      "manager", `Manager;
    ]
  let compare (a:t) (b:t) = Pervasives.compare a b

  let pp _ = failwith "TODO"

  let example = "manager"
end

module RW = struct
  type t = { read: bool; write: bool }
  let of_string_exn p : t =
    if String.length p <> 2 then
      invalid_arg ("Invalid privs: " ^ p);
    {
      read = (p.[0] = 'r');
      write = (p.[1] = 'w');
    }

  let to_string {read;write} =
    (if read then "r" else "-") ^
    (if write then "w" else "-")

  let encoding = conv to_string of_string_exn string

  let pp = Fmt.(using to_string string)

  let example = "r-"
end

include Set.Make(Priv)

let of_rw (rw:RW.t) =
  let open! RW in
  let set = empty in
  let set = if rw.read then add `Read set else set in
  if rw.write then add `Write set else set

let to_rw set =
  {
    RW.read = mem `Read set;
    RW.write = mem `Write set;
  }

let rev_elements s = elements s |> List.rev
let encoding = list Priv.encoding |> conv rev_elements of_list

let example = "[\"manager\",\"read\"]"

let pp _ = failwith "TODO"
