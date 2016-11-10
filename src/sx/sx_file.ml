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
  let example = "\"2014-08-12 17:22:44.192:18681b2989a2aaa51bffad12555fdb23\""
end

let uri (Sx_volume.T.Volume vol) path =
  Uri.make ~path:("/" ^ (Filename.concat vol path)) ()

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

  let get = uri

  let example = "{\"blockSize\":4096,\"createdAt\":1399634734,\"fileData\":[{\"46c899e54e096a2cf5ab937c6db4a3cd574b610d\":[\"127.0.0.4\",\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.2\"]},{\"5307c51ba36363ddfd1cb310a0fc16050d942e8b\":[\"127.0.0.2\",\"127.0.0.1\",\"127.0.0.4\",\"127.0.0.3\"]},{\"46e82e208d9fd75c085ca9cf82351bab9aa0b42e\":[\"127.0.0.4\",\"127.0.0.2\",\"127.0.0.3\",\"127.0.0.1\"]},{\"dec11072d085efd0dfd047efe0a9a8375096e769\":[\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.4\",\"127.0.0.2\"]},{\"271fe0055f750a31bb3943ba99f0d55715e0050e\":[\"127.0.0.4\",\"127.0.0.3\",\"127.0.0.2\",\"127.0.0.1\"]}],\"fileRevision\":\"2014-05-09 11:25:34.687:abc10225a67838c32006e94be597c5b1\",\"fileSize\":18092}"

  let example = "{\"blockSize\":4096,\"createdAt\":1399634734,\"fileRevision\":\"2014-05-09 11:25:34.687:abc10225a67838c32006e94be597c5b1\",\"fileSize\":18092,\"fileData\":[{\"46c899e54e096a2cf5ab937c6db4a3cd574b610d\":[\"127.0.0.4\",\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.2\"]},{\"5307c51ba36363ddfd1cb310a0fc16050d942e8b\":[\"127.0.0.2\",\"127.0.0.1\",\"127.0.0.4\",\"127.0.0.3\"]},{\"46e82e208d9fd75c085ca9cf82351bab9aa0b42e\":[\"127.0.0.4\",\"127.0.0.2\",\"127.0.0.3\",\"127.0.0.1\"]},{\"dec11072d085efd0dfd047efe0a9a8375096e769\":[\"127.0.0.1\",\"127.0.0.3\",\"127.0.0.4\",\"127.0.0.2\"]},{\"271fe0055f750a31bb3943ba99f0d55715e0050e\":[\"127.0.0.4\",\"127.0.0.3\",\"127.0.0.2\",\"127.0.0.1\"]}]}"

  let pp _ = failwith "TODO"
end

module Meta = struct
  type t = { file_meta : Meta.t }
  let of_v t = t.file_meta
  let v file_meta = { file_meta }

  let field_encoding = Meta.encoding |> conv of_v v
  let encoding = obj1 (req "fileMeta" Meta.encoding) |> obj_opt |> conv of_v v
  let pp = Fmt.using of_v Meta.pp

  let target = Volume
  let example = "{\"fileMeta\":{\"OSI-Approved\":\"796573\"}}"

  let get vol path =
    Uri.with_query (Get.get vol path) ["fileMeta",[]]
end

module Initialize = struct
  module Request = struct
    type header = Int53.t * Meta.t

    let header_encoding = obj2 (req "fileSize" Int53.encoding)
        (req "fileMeta" Meta.field_encoding)

    type t = Sx_block.t
    type element = t

    let all_encoding = merge_objs header_encoding
        (obj1 (req "fileData" (list Sx_block.encoding)))

    let streaming = arr_streaming header_encoding "fileData" Sx_block.encoding

    let put = uri
    let example = "{\"fileSize\":7651,\"fileData\":[\"d623df8f4425635bf30bf3d526e8a585cf03048c\",\"1ee9296225132bc3a167d735d418d1fe026114d1\"],\"fileMeta\":{\"OSI-Approved\":\"796573\"}}"

    let pp _ = failwith "TODO"
  end
  module Reply = struct
    type header = UploadToken.t

    let header_encoding : header encoding = obj1 (req "uploadToken" UploadToken.encoding)

    type t = Ipaddr.t list
    type element = Sx_block.t * t

    let to_block lst =
      List.rev_map (fun (k,v) -> Sx_block.of_string k, v) lst

    let of_block lst = 
      List.rev_map (fun (k,v) -> Sx_block.to_string k, v) lst

    let entry_encoding = assoc (list ipaddr) |>
                         conv Sx_block.unsafe_to_assoc Sx_block.unsafe_of_assoc

    let all_encoding = merge_objs header_encoding
        (obj1 (req "uploadData" entry_encoding))
    let streaming = obj_streaming header_encoding "uploadData" (list ipaddr) |>
                    Sx_block.unsafe_of_streaming

    let example = "{\"uploadToken\":\"e4c09c7e-48ec-4940-92d9-518ed88d6d3f:7a9bb44da0e6ec17a4716ac8c50b80be:00000002:0000000053721bb7:e7e0916a90d4500bc1b6c98628fb9f950a748b2e\",\"uploadData\":{\"1ee9296225132bc3a167d735d418d1fe026114d1\":[\"127.0.0.4\",\"127.0.0.2\"],\"d623df8f4425635bf30bf3d526e8a585cf03048c\":[\"127.0.0.2\",\"127.0.0.1\"]}}"

    let pp _ = failwith "TODO"
  end

  module AddChunk = struct
    type t = {
      extend_seq: Int53.t;
      file_size : Int53.t option; (* 2.2, see Sx_volume.Locate.growableSize *)
      file_data: Sx_block.t list;
      file_meta: (string * Hex.t option) list;
    }

    let meta_encoding = assoc (option hex_encoding)

    let of_v t = t.extend_seq, t.file_size, t.file_data, t.file_meta
    let v (extend_seq, file_size, file_data, file_meta) =
      { extend_seq; file_size; file_data; file_meta }

    let encoding = obj4 (req "extendSeq" Int53.encoding)
        (opt "fileSize" Int53.encoding)
        (req "fileData" (list Sx_block.encoding))
        (dft "fileMeta" meta_encoding []) |> conv of_v v

    let pp _ = failwith "TODO"

    let target = SingleHost

    let put = UploadToken.file_uri

    let example = "{\"extendSeq\":1,\"fileData\":[\"1ee9296225132bc3a167d735d418d1fe026114d1\"],\"fileMeta\":{\"May change later\":\"42\",\"May be removed later\":null,\"Added in step 2\":\"1337\"}}"
  end
end

module Flush = struct
  let put = UploadToken.file_uri
  let target = SingleHost
end

module Delete = struct
  let delete vol path = uri vol path
  let target = Volume
end

module ListRevisions = struct
  type attr = {
    block_size: int;
    file_size: Int53.t;
    created_at: Http_date.t;
  }

  let of_v a = a.block_size, a.file_size, a.created_at
  let v (block_size, file_size, created_at) =
    {block_size; file_size; created_at}


  let attr_encoding = obj3 (req "blockSize" int)
      (req "fileSize" Int53.encoding)
      (req "createdAt" http_date) |> obj_opt |> conv of_v v

  type t = (Revision.t * attr) list

  let encoding = obj1 (req "fileRevisions" (assoc attr_encoding))

  let pp _ = failwith "TODO"

  let target = Volume

  let example = "{\"fileRevisions\":{\"2015-05-01 10:30:00.009:ba3c495ebf9e10cd62489bbf6806ee62\":{\"blockSize\":4096,\"fileSize\":83755,\"createdAt\":1430476200},\"2015-05-15 10:30:00.183:b12a9e5b5b68920fc183d849911a28ec\":{\"blockSize\":4096,\"fileSize\":75423,\"createdAt\":1431685800},\"2015-06-01 10:30:00.210:1135f377f172633ef8bf3bc83639bc85\":{\"blockSize\":16384,\"fileSize\":1316830,\"createdAt\":1433154600}}}"

  let get vol path =
    Uri.with_query (uri vol path) ["fileRevisions",[]]
end
