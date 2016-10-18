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
open Sx_types

module Revision = struct
  type t = string
  let encoding = string
  let pp = Fmt.string
end

module Get = struct
  type header = {
    file_size : Int53.t;
    block_size: int;
    created_at: Http_date.t;
    file_revision: Revision.t;
  }
  let of_header h = h.file_size, h.block_size, h.created_at, h.file_revision
  let to_header (file_size, block_size, created_at, file_revision) =
    { file_size; block_size; created_at; file_revision }

  let header_obj = obj4 (req "fileSize" Int53.encoding)
      (req "blockSize" int)
      (req "createdAt" http_date)
      (req "fileRevision" Revision.encoding)

  let header_encoding = header_obj |> conv of_header to_header

  type t = Sx_block.t * Ipaddr.t list

  type element = t

  let encoding = singleton Sx_block.to_string Sx_block.of_string (list ipaddr)

  let all_encoding = merge_objs header_obj (obj1 (req "fileData" (list encoding)))

  let streaming = arr_streaming header_encoding "fileData" encoding

  let get (Sx_volume.T.Volume vol) path =
    Uri.make ~path:("/" ^ (Filename.concat vol path)) ()

  let example = "{\"blockSize\":4096,\"createdAt\":1399634734,\"fileData\":[{\"46c899e54e096a2cf5ab937c6db4a3cd574b610d\":[\"127.0.0.4\",\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.2\"]},{\"5307c51ba36363ddfd1cb310a0fc16050d942e8b\":[\"127.0.0.2\",\"127.0.0.1\",\"127.0.0.4\",\"127.0.0.3\"]},{\"46e82e208d9fd75c085ca9cf82351bab9aa0b42e\":[\"127.0.0.4\",\"127.0.0.2\",\"127.0.0.3\",\"127.0.0.1\"]},{\"dec11072d085efd0dfd047efe0a9a8375096e769\":[\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.4\",\"127.0.0.2\"]},{\"271fe0055f750a31bb3943ba99f0d55715e0050e\":[\"127.0.0.4\",\"127.0.0.3\",\"127.0.0.2\",\"127.0.0.1\"]}],\"fileRevision\":\"2014-05-09 11:25:34.687:abc10225a67838c32006e94be597c5b1\",\"fileSize\":18092}"

  let example = "{\"blockSize\":4096,\"createdAt\":1399634734,\"fileRevision\":\"2014-05-09 11:25:34.687:abc10225a67838c32006e94be597c5b1\",\"fileSize\":18092,\"fileData\":[{\"46c899e54e096a2cf5ab937c6db4a3cd574b610d\":[\"127.0.0.4\",\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.2\"]},{\"5307c51ba36363ddfd1cb310a0fc16050d942e8b\":[\"127.0.0.2\",\"127.0.0.1\",\"127.0.0.4\",\"127.0.0.3\"]},{\"46e82e208d9fd75c085ca9cf82351bab9aa0b42e\":[\"127.0.0.4\",\"127.0.0.2\",\"127.0.0.3\",\"127.0.0.1\"]},{\"dec11072d085efd0dfd047efe0a9a8375096e769\":[\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.4\",\"127.0.0.2\"]},{\"271fe0055f750a31bb3943ba99f0d55715e0050e\":[\"127.0.0.4\",\"127.0.0.3\",\"127.0.0.2\",\"127.0.0.1\"]}]}"

  let pp _ = failwith "TODO"
end

module Meta = struct
  type t = { file_meta : Meta.t }
  let of_v t = t.file_meta
  let v file_meta = { file_meta }
  let encoding = obj1 (req "fileMeta" Meta.encoding) |> obj_opt |> conv of_v v
  let pp = Fmt.using of_v Meta.pp

  let target = Volume
  let example = "{\"fileMeta\":{\"OSI-Approved\":\"796573\"}}"

  let get vol path =
    Uri.with_query (Get.get vol path) ["fileMeta",[]]
end
