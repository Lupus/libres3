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

(* DELETE Bucket policy
   GET Bucket policy
   PUT Bucket policy *)

type obj
type bucket
type subresource

module Permission = struct
  type 'a t = string (* TODO *)


  let obj perm = perm
  let bucket perm = perm
  let subresource perm = perm

  let all = "*"
end

module GetBucket = struct
  let policy = Permission.subresource "s3:GetBucketPolicy"
end

module DeleteBucket = struct
  let policy = Permission.subresource "s3:DeleteBucketPolicy"
end

module PutBucket = struct
  let policy = Permission.subresource "s3:PutBucketPolicy"
end

module ObjSet = Set.Make(struct type t = obj Permission.t let compare = compare end)
module BucketSet = Set.Make(struct type t = bucket Permission.t let compare = compare end)
module SubresourceSet = Set.Make(struct type t = subresource Permission.t let compare = compare end)

(*
   READ: Get*
   WRITE: Put*
   MANAGER: Put*Acl

   store in customVolumeMeta (requires manager)

   check if you have it -> escalate to volume owner
   add libres3-policy-<user> user user to all S3 buckets,
   and escalate to that

   SX ACL mapped to S3 ACL mapped to S3 policy
   flag to specify which one is in effect in customVolumeMeta

   SX ACL needs to be conservative: if you don't grant all s3:list* policies for e.g.
    then user is not granted access at SX level

   try regular op, fetch/cache policy/ apply policy

   bucket owner vs object owner

   already existing volumes
*)
