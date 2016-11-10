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
open Sx_types

module Meta = struct
  type t = VolumeMeta of Sx_types.Meta.t
  let v meta = VolumeMeta meta
  let of_v (VolumeMeta meta) = meta
  let encoding = conv of_v v Sx_types.Meta.encoding
  let pp = Fmt.(always "volume meta" |> prefix)
             Fmt.(using of_v Sx_types.Meta.pp |> braces)

  let example = "{\"a\":\"bb00cc\"}"
end

module Attr = struct
  type t = {
    size_bytes: Int53.t;
    used_size : Int53.t;
    replica_count: int;
    max_revisions: int;
    privs: Sx_acl.RW.t;
    owner: User.t;
    files_size: Int53.t; (* 2.1 *)
    files_count: Int53.t; (* 2.1 *)
    volume_meta: Meta.t option;
    custom_volume_meta: Meta.t option;
    global_id: string option; (* 2.1 *)
    (* undocumented: effectiveReplicaCount *)
  }

  let of_v t = (t.size_bytes,t.used_size,t.replica_count,t.max_revisions,
                t.privs, t.owner,t.files_size,t.files_count,t.volume_meta,
                t.custom_volume_meta),t.global_id

  let v ((size_bytes,used_size,replica_count,max_revisions,privs,owner,
         files_size,files_count,volume_meta,custom_volume_meta),global_id) =
    {size_bytes;used_size;replica_count;max_revisions;privs;owner;files_size;
    files_count;volume_meta;custom_volume_meta;global_id}

  let obj = merge_objs (obj10
      (req "sizeBytes" Int53.encoding)
      (req "usedSize" Int53.encoding)
      (req "replicaCount" int)
      (req "maxRevisions" int)
      (req "privs" Sx_acl.RW.encoding)
      (req "owner" User.encoding)
      (req "filesSize" Int53.encoding)
      (req "filesCount" Int53.encoding)
      (opt "volumeMeta" Meta.encoding)
      (opt "customVolumeMeta" Meta.encoding))
      (opt "globalID" string |> obj1)

  let encoding = obj |> obj_opt |> conv of_v v

  let example = "{\"owner\":\"user\",\"replicaCount\":2,\"maxRevisions\":6,\"privs\":\"rw\",\"usedSize\":142685712,\"sizeBytes\":102005473280,\"filesSize\":102001213000,\"filesCount\":2345}"

  let pp _ = failwith "TODO"
end

module T = struct
  type t = Volume of string
  let v s =
    if String.contains s '/' then
      invalid_arg "Volume cannot contain /";
    Volume s
  let of_v (Volume v) = v
  let encoding = conv of_v v string
  let example = "vol1"
  let pp = Fmt.(using of_v string)

  let uri (Volume v) =
    Uri.make ~path:("/" ^ v) ()
end

module ListVolumes = struct
  type t = (T.t * Attr.t) list

  let to_volume lst =
    List.rev_map (fun (k,v) -> T.v k, v) lst

  let of_volume lst =
    List.rev_map (fun (k,v) -> T.of_v k, v) lst

  let encoding = obj1 (req "volumeList" (assoc Attr.encoding)) |> conv of_volume to_volume
  let pp = Fmt.(pair T.pp Attr.pp |> list)

  let target = Cluster

  let example = "{\"volumeList\":{\"MyOtherVolume\":{\"owner\":\"user\",\"replicaCount\":2,\"maxRevisions\":6,\"privs\":\"rw\",\"usedSize\":142685712,\"sizeBytes\":102005473280,\"filesSize\":102001213000,\"filesCount\":2345},\"volume\":{\"owner\":\"admin\",\"replicaCount\":1,\"maxRevisions\":3,\"privs\":\"r-\",\"usedSize\":2871431,\"sizeBytes\":10737418240,\"filesSize\":10735131100,\"filesCount\":1311}}}"

  let get ?(volume_meta=false) ?(custom_volume_meta=false) () =
    let query =  ["volumeList",[]] |>
                 query_opt_bool volume_meta "volumeMeta" |>
                 query_opt_bool custom_volume_meta "customVolumeMeta"
    in
    Uri.make ~path:"/" ~query ()
end


module Locate = struct
  type volnodes = Ipaddr.t list
  type size_info = {
    growable_size : Int53.t; (* 2.2 *)
    block_size : int;
  }

  let volnodes_obj = obj1 (req "nodeList" (list ipaddr))

  let size_info_obj = obj2
      (opt "growableSize" Int53.encoding)
      (opt "blockSize" int)

  let of_v = function
  | Some t -> Some t.growable_size, Some t.block_size
  | None -> None, None

  let v = function
  | Some growable_size, Some block_size -> Some {growable_size;block_size}
  | None, None -> None
  | None, Some _ -> invalid_arg "Missing field growableSize"
  | Some _, None -> invalid_arg "Missing field blockSize"

  let size_info_encoding = size_info_obj |> conv of_v v

  type t = volnodes * (size_info option * Attr.t)
  (* undocumented: effectiveReplicaCount, growableSize *)

  let attr_encoding = Attr.obj |> conv Attr.of_v Attr.v

  let encoding : t encoding =
    merge_objs volnodes_obj
      (merge_objs size_info_encoding attr_encoding) |> obj_opt

  let pp _ = failwith "TODO"

  let target = Cluster

  let example = "{\"nodeList\":[\"127.0.0.2\",\"127.0.0.4\",\"127.0.0.1\"],\"blockSize\":16384,\"owner\":\"user\",\"replicaCount\":2,\"maxRevisions\":6,\"privs\":\"rw\",\"usedSize\":142685712,\"sizeBytes\":63542,\"filesSize\":61012,\"filesCount\":89,\"volumeMeta\":{\"two\":\"2222\",\"three\":\"333333\",\"one\":\"01\"}}"

  let get ?(volume_meta=false) ?(custom_volume_meta=false) ?size volume =
    ["o",["locate"]] |>
    query_opt_bool volume_meta "volumeMeta" |>
    query_opt_bool custom_volume_meta "customVolumeMeta" |>
    query_opt size (fun siz -> ["size",[Int53.to_string siz]]) |>
    Uri.with_query (T.uri volume)
end

module Create = struct
  type t = {
    volume_size : Int53.t;
    owner: User.t;
    replica_count: int;
    max_revisions: int;
    volume_meta: Meta.t option;
  }

  let of_v t = t.volume_size, t.owner, t.replica_count, t.max_revisions,
               t.volume_meta

  let v (volume_size, owner, replica_count, max_revisions, volume_meta) =
    { volume_size; owner; replica_count; max_revisions; volume_meta }

  let encoding = obj5
      (req "volumeSize" Int53.encoding)
      (req "owner" User.encoding)
      (req "replicaCount" int)
      (dft "maxRevisions" int 1)
      (opt "volumeMeta" Meta.encoding) |> conv of_v v

  let put = T.uri

  let target = Cluster

  let pp _ = failwith "TODO"

  let target = Cluster

  let pp _ = failwith "TODO"

  let example = "{\"volumeSize\":107374182400,\"owner\":\"testuser\",\"replicaCount\":1,\"volumeMeta\":{\"VolumeDescription\":\"736861726564206f666669636520646f63756d656e7473\",\"ApplicationID\":\"ff03\"}}"
end

module Modify = struct
  type t = {
    size: Int53.t option;
    owner: User.t option;
    max_revisions: int option;
    custom_volume_meta: Meta.t option;
    name : T.t option
  }

  let v t = t.size, t.owner, t.max_revisions, t.custom_volume_meta, t.name

  let of_v (size,owner,max_revisions,custom_volume_meta,name) =
    {size;owner;max_revisions;custom_volume_meta;name}

  let encoding = obj5
      (opt "size" Int53.encoding)
      (opt "owner" User.encoding)
      (opt "maxRevisions" int)
      (opt "customVolumeMeta" Meta.encoding)
      (opt "name" T.encoding) |> conv v of_v

  let target = Cluster

  let pp _ = failwith "TODO"
  
  let put volume =
    Uri.with_query' (T.uri volume) ["o","mod"]

  let example = "{\"size\":21474836480,\"maxRevisions\":4}"
end

module ModifyReplica = struct
  type t = {
    next_replica: int
  }

  let v next_replica = {next_replica}
  let of_v {next_replica} = next_replica

  let encoding = obj1 (req "next_replica" int) |> conv of_v v

  let example = "{\"next_replica\":3}"
  let pp = Fmt.(using of_v int)

  let target = Cluster

  let put vol =
    Uri.with_query' (T.uri vol) ["o", "replica"]
end

module Delete = struct
  let delete vol = T.uri vol
  let target = Cluster
end

module Acl = struct
  module Get = struct
    type t = (User.t * Sx_acl.t) list

    let of_user lst =
      List.rev_map (fun (u,v) -> User.of_v u, v) lst

    let to_user lst =
      List.rev_map (fun (u,v) -> User.v u, v) lst

    let encoding = assoc Sx_acl.encoding |> conv of_user to_user

    let target = Cluster

    let get vol =
      Uri.with_query (T.uri vol) ["o",["acl"]; "manager",[]]

    let pp = Fmt.(pair User.pp Sx_acl.pp |> list)

    let example = "{\"testuser\":[\"read\",\"write\",\"manager\",\"owner\"],\"admin\":[\"read\",\"write\",\"manager\"]}"
  end
  module Update = struct
    type t = {
      grant_read : User.t list;
      grant_write: User.t list;
      grant_manager: User.t list;
      revoke_read : User.t list;
      revoke_write: User.t list;
      revoke_manager: User.t list;
    }

    let of_v v = v.grant_read,v.grant_write,v.grant_manager,v.revoke_read,v.revoke_write,v.revoke_manager
    let v (grant_read,grant_write,grant_manager,revoke_read,revoke_write,revoke_manager) =
      {grant_read;grant_write;grant_manager;revoke_read;revoke_write;revoke_manager}

    let users = list User.encoding

    let encoding = obj6
        (dft "grant-read" users [])
        (dft "grant-write" users [])
        (dft "grant-manager" users [])
        (dft "revoke-read" users [])
        (dft "revoke-write" users [])
        (dft "revoke-manager" users [])
                   |> conv of_v v

    let pp _ = failwith "TODO"
    let target = Cluster

    let example = "{\"grant-read\":[\"readonly\",\"readwrite\"],\"grant-write\":[\"writer\",\"readwrite\"],\"revoke-read\":[\"writer\"]}"

    let put vol =
      Uri.with_query' (T.uri vol) ["o","acl"]
  end
end

module ListFiles = struct
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

  type element = string * t
  
  let streaming = obj_streaming header "fileList" encoding

  let all_encoding = merge_objs header_obj (obj1 (req "fileList" (assoc encoding))) |> obj_opt

  let get ?filter ?(recursive=false)  ?limit ?after volume =
    ["o", ["list"]] |>
    Pattern.add_query_opt filter |>
    query_opt_bool recursive "recursive" |>
    query_opt limit (fun limit -> ["limit",[string_of_int limit]]) |>
    query_opt after (fun start -> ["after",[start]]) |>
    Uri.with_query (T.uri volume)

end

module Mass = struct
  module Delete = struct
    let delete vol pattern =
      Pattern.add_query_opt (Some pattern) [] |>
      Uri.with_query (T.uri vol)
    let target = Volume
  end
  module Rename = struct
    type t = {
      source: string;
      dest: string;
      recursive: bool;
    }
    let rename vol t =
      ["source", [t.source]; "dest", [t.dest]] |>
      query_opt_bool t.recursive "recursive" |>
      Uri.with_query (T.uri vol)
    let target = Volume
  end
end
