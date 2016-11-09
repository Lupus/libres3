(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2016 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License version 2 as     *)
(*  published by the Free Software Foundation.                            *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA 02110-1301 USA.                                                    *)
(*                                                                        *)
(*  Special exception for linking this software with OpenSSL:             *)
(*                                                                        *)
(*  In addition, as a special exception, Skylable Ltd. gives permission   *)
(*  to link the code of this program with the OpenSSL library and         *)
(*  distribute linked combinations including the two. You must obey the   *)
(*  GNU General Public License in all respects for all of the code used   *)
(*  other than OpenSSL. You may extend this exception to your version     *)
(*  of the program, but you are not obligated to do so. If you do not     *)
(*  wish to do so, delete this exception statement from your version.     *)
(**************************************************************************)

open Acl
open Policy

module AclMap = Map.Make(T)

module BuildMap(S : Set.S) = struct
  let of_acl of_acl_map grants =
    GrantMap.map (fun set ->
        AclSet.fold (fun e accum ->
            AclMap.find e of_acl_map |> S.union accum) set S.empty
    ) grants

  let to_acl to_acl_map grants =
    GrantMap.map (fun set ->
        List.fold_left (fun accum (acl, subset) ->
            if S.subset subset set then AclSet.add acl accum
            else accum
        ) AclSet.empty to_acl_map
    ) grants

  let build lst =
    let of_acl_map =
      List.fold_left (fun accum (k, v) -> AclMap.add k v accum) AclMap.empty lst in
    let to_acl_map = lst in
    of_acl of_acl_map, to_acl to_acl_map
end

module BS = BuildMap(BucketSet)
module OS = BuildMap(ObjSet)

let bucket_of_acl, acl_of_bucket =
  let open T in
  BS.build [
    Read, BucketSet.of_list [Bucket.ListObjects.Reply.policy;
                             Bucket.GetObjectVersions.policy;
                             Bucket.ListMultipartUploads.policy];
    Write, BucketSet.of_list [Object.Put.policy; Object.Delete.policy];
    Read_acp, BucketSet.singleton GetBucket.policy;
    Write_acp, BucketSet.singleton PutBucket.policy
  ]

let obj_of_acl, acl_of_obj =
  let open T in
  OS.build [
    Read, ObjSet.of_list [Object.Get.policy;
                          Object.GetVersion.policy;
                          Unimpl.GetObjectTorrent.policy];
    Write, ObjSet.empty;
    Read_acp, ObjSet.of_list [Object.Acl.Get.policy; Object.Acl.GetVersion.policy];
    Write_acp, ObjSet.of_list [Object.Acl.Put.policy; Object.Acl.PutVersion.policy]
  ]

module VolAcl = struct
  type t = [`Read | `Write | `Manager]
  let compare = compare
end

module VolSet = Set.Make(VolAcl)
module VS = BuildMap(VolSet)

let vol_of_acl, acl_of_vol =
  let open T in
  VS.build [
    Read, VolSet.singleton `Read;
    Write, VolSet.singleton `Write;
    Read_acp, VolSet.singleton `Manager;
    Write_acp, VolSet.singleton `Manager;
  ]
