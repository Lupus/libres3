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

open Jsonenc
open Json_encoding

type header = {
  volume_size: Int53.t;
}
type attributes = {
  file_size : Int53.t;
  block_size : int;
  created_at : Http_date.t;
  file_revision : string;
}
type t = File of attributes | Directory

let pp ppf = function
| Directory -> Fmt.pf ppf "directory"
| File f ->
    Fmt.pf ppf "@[file {size=%a; blocksize=%d; created_at=%a; revision=%s}@]"
      Int53.pp f.file_size f.block_size Http_date.pp f.created_at f.file_revision

let of_file (file_size, block_size, created_at, file_revision) = File {
    file_size; block_size; created_at; file_revision
  }

let to_file = function
| File f -> Some (f.file_size, f.block_size, f.created_at, f.file_revision)
| Directory -> None

let to_directory = function
| File _ -> None
| Directory -> Some ()

let of_directory _ = Directory

let encoding =
  let open Json_encoding in
  let file = obj4 (req "fileSize" Int53.encoding) (req "blockSize" int)
      (req "createdAt" http_date)
      (req "fileRevision" string) |> obj_opt in
  let directory = empty |> obj_opt in
  union [
    case file to_file of_file;
    case directory to_directory of_directory
  ]

let example = "{\"volumeSize\":10737418240,\"fileList\":{\"/BSD_2-clause.txt\":{\"fileSize\":1511,\"blockSize\":4096,\"createdAt\":1407864164,\"fileRevision\":\"2014-08-12 17:22:44.192:18681b2989a2aaa51bffad12555fdb23\"},\"/BSD_3-clause.txt\":{\"fileSize\":1507,\"blockSize\":4096,\"createdAt\":1407864164,\"fileRevision\":\"2014-08-12 17:22:44.574:f5d8e79d20d8d8c6f7202f8d22efe7b5\"},\"/documentation/\":{},\"/gpl2.txt\":{\"fileSize\":18092,\"blockSize\":4096,\"createdAt\":1407863898,\"fileRevision\":\"2014-08-12 17:18:18.573:bfd129b3155254d987bdd4532351fc30\"},\"/gpl3.txt\":{\"fileSize\":35147,\"blockSize\":4096,\"createdAt\":1407863899,\"fileRevision\":\"2014-08-12 17:18:19.091:cbfac4ed6ec0f0715660387cdf715f78\"},\"/library/\":{}}}"
let to_header volume_size = {volume_size}
let of_header h = h.volume_size

let header_obj = obj1 (req "volumeSize" Int53.encoding)

let header = header_obj |> conv of_header to_header

let streaming = obj_streaming header "fileList" encoding

let all_encoding = merge_objs header_obj (obj1 (req "fileList" (assoc encoding))) |> obj_opt
