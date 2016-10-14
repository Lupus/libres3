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
open Jsonenc

module Meta = struct
  type binary = string (* TODO *)
  let binary = Fmt.string

  type t = (string * binary) list

  let encoding = assoc string

  let pp = Fmt.(pair string binary |> list)
end

module Locate = struct
  type t = {
    node_list : Ipaddr.t list;
    block_size : int option;
    volume_meta : Meta.t option;
    custom_volume_meta : Meta.t option;
    files_size : Int53.t;
    files_count: Int53.t;
    size_bytes : Int53.t;
    used_size : Int53.t;
    replica_count : int;
    max_revisions : int;
    privs: string;
    owner: string;
    global_id: string option;
  }

  let ipaddr = conv Ipaddr.to_string Ipaddr.of_string_exn string

  let to_obj t = (
    (t.node_list, t.block_size, t.volume_meta, t.custom_volume_meta,
     t.files_size, t.files_count, t.size_bytes, t.used_size, t.replica_count,
     t.max_revisions),
    (t.privs, t.owner, t.global_id))

  let of_obj ((node_list,block_size,volume_meta,custom_volume_meta,
               files_size, files_count, size_bytes, used_size, replica_count,
               max_revisions),(privs,owner,global_id)) =
    {node_list;block_size;volume_meta;custom_volume_meta;
     files_size;files_count;size_bytes;used_size;replica_count;
     max_revisions;privs;owner;global_id}

  let encoding = merge_objs
      (obj10
         (req "nodeList" (list ipaddr))
         (opt "blockSize" int)
         (opt "volumeMeta" Meta.encoding)
         (opt "customVolumeMeta" Meta.encoding)
         (req "filesSize" Int53.encoding)
         (req "filesCount" Int53.encoding)
         (req "sizeBytes" Int53.encoding)
         (req "usedSize" Int53.encoding)
         (req "replicaCount" int)
         (req "maxRevisions" int))
      (obj3
         (req "privs" string)
         (req "owner" string)
         (opt "globalID" string)) |>
                 obj_opt |>
                 conv to_obj of_obj

  let pp _ = failwith "TODO"
  let example = "{\"nodeList\":[\"127.0.0.2\",\"127.0.0.4\",\"127.0.0.1\"],\"blockSize\":16384,\"owner\":\"user\",\"replicaCount\":2,\"maxRevisions\":6,\"privs\":\"rw\",\"usedSize\":142685712,\"sizeBytes\":63542,\"filesSize\":61012,\"filesCount\":89,\"volumeMeta\":{\"two\":\"2222\",\"three\":\"333333\",\"one\":\"01\"}}"
end
