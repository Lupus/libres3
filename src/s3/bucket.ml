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

(* GET Service
   DELETE Bucket
   HEAD Bucket
   GET Bucket (List Objects) v1, v2
   PUT Bucket
*)

open Xmlio
open CalendarLib
open Cohttp
open Rresult

let timestamp_unix_to_iso t =
  Printer.Calendar.sprint "%FT%T.000Z" t

module Region = struct
  type t = string
  let pp = Fmt.string

  let of_xml = function
  (* TODO: validate region *)
  | [ `Element (_,_,[`Text loc])] -> Ok (Some loc)
  | _ -> Ok None
end

module StorageClass = struct
  type t = string

  let to_string s = s
end

module Service = struct
  module Bucket = struct
    type t = {
      name: string;
      creation_date: Calendar.t;
    }

    let to_xml t = element "Bucket" [
        element "Name" [text t.name];
        element "CreationDate" [timestamp_unix_to_iso t.creation_date |> text]
    ]
  end
  type t = {
    owner : Acl.CanonicalUser.t;
    buckets: Bucket.t list;
  }

  let policy = Policy.Permission.bucket "s3:ListAllMyBuckets"

  let to_xml t =
    root "ListAllMyBucketsResult" [
      element "Owner" (Acl.CanonicalUser.to_xml t.owner);
      element "Buckets" (List.rev_map Bucket.to_xml t.buckets)
    ]

  let to_reply t = (Header.init (), Some (to_xml t))
end

module Create = struct
  type t = {
    bucket : string;
    acl : Acl.t;
    location_constraint: Region.t option;
  }

  let of_request (header, bucket, xml) =
    R.(
      Acl.of_header header >>= fun acl ->
      Xmlio.expect_root "CreateBucketConfiguration" xml >>= fun lst ->
      let loc, _ = List.partition (Xmlio.is_tag ~tag:"LocationConstraint") lst in
      (* TODO: if strict check that there are no more tags here *)
      Region.of_xml loc >>= fun location_constraint ->
      return { bucket; acl; location_constraint }
    )

  let to_reply t =
    (Header.init_with "Location" ("/" ^ t.bucket), None)

  let policy = Policy.Permission.bucket "s3:CreateBucket"
end

module Delete = struct
  let policy = Policy.Permission.bucket "s3:DeleteBucket"
end

module Head = struct
  let policy = Policy.Permission.bucket "s3:ListBucket"
end

module ListObjects = struct
  type version = [`V1 | `V2]
  module Req = struct
    type t = {
      delimiter : string;
      encoding_type: Uri.t option;
      marker: string option;
      max_keys: int;
      prefix: string;
      list_type : version;
      continuation_token : string option;(* V2 *)
      fetch_owner: bool;(* V2 *)
      start_after: string option; (* V2 *)
    }
  end
  module Reply = struct
    type contents = {
      owner: Acl.CanonicalUser.t option;(* V2: optional *)
      etag: string;
      key: string;
      last_modified: Http_date.t;
      size: Int64.t;
      storage_class: StorageClass.t;
    }
    type t = {
      contents: contents list;
      common_prefixes : string list;
      delimiter: string option;
      encoding_type: string option;
      is_truncated: bool;
      marker: string option;
      max_keys: int;
      name: string;
      next_marker: string option;
      owner: Acl.CanonicalUser.t;
      prefix: string option;
      continuation_token: string option;(* V2 *)
      key_count: int option; (* V2 *)
      next_continuation_token: string option;(* V2 *)
      start_after: string option; (* V2 *)
    }
    let policy = Head.policy
  end
end

module GetObjectVersions = struct
  let policy = Policy.Permission.bucket "s3:ListBucketVersions"
end

module ListMultipartUploads = struct
  let policy = Policy.Permission.bucket "s3:ListBucketMultipartUploads"
end
