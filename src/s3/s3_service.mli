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

type context = unit
type bucket = string
type subresource = string
type key = string
type version = string
type stream_header
type stream

type 'a req =
  | GetService : context -> Bucket.Service.t req
  | DeleteBucket : (context * bucket) -> unit req
  | DeleteBucketSubresource: (context * bucket * subresource) -> unit req
  | GetBucketList : (context * bucket * Bucket.ListObjects.Req.t) -> Bucket.ListObjects.Reply.t req
  | GetBucketSubresource : (context * bucket * subresource) -> Xmlio.xml req
  | HeadBucket : (context * bucket) -> unit req
  | ListMultipartUploads : (context * bucket) -> Bucket.ListMultipartUploads.t req
  | CreateBucket : (context * bucket * Bucket.Create.t) -> unit req
  | PutBucketSubresource : (context * bucket * subresource * Xmlio.xml) -> unit req
  | DeleteObject: (context * bucket * key) -> unit req
  | DeleteMultipleObjects: (context * bucket * Object.DeleteMulti.t) -> unit req
  | GetObject : (context * bucket * key * version option) -> stream req
  | GetObjectACL : (context * bucket * key * version option) -> Object.Acl.t req
  | GetObjectTorrent : (context * bucket * key) -> unit req
  | HeadObject : (context * bucket * key * version option) -> stream_header req
  | OptionsObject : (context * bucket * key) -> Cohttp.Header.t req
  | PostObject : (context * bucket * key * stream) -> unit req
  | PostObjectRestore: (context * bucket * key) -> unit req
  | PutObject : (context * bucket * key * version option * stream) -> unit req
  | PutObjectACL : (context * bucket * key * version option * Object.Acl.t) -> unit req
  | PutObjectCopy : (context * bucket * key * version option * Object.Copy.t) -> unit req
  | InitiateMultipart : (context * bucket * key * Object.InitiateMultipart.t) -> unit req
  | UploadPart : (context * bucket * key * Object.UploadPart.t) -> unit req
  | UploadPartCopy : (context * bucket * key * Object.UploadPartCopy.t) -> unit req
  | CompleteMultipart: (context * bucket * key * Object.CompleteMultipart.t) -> unit req
  | AbortMultipart : (context * bucket * key * Object.AbortMultipart.t) -> unit req
  | ListParts: (context * bucket * key) -> Object.ListParts.t req

module type S = sig
  val service : 'a req -> 'a Boundedio.t
end
