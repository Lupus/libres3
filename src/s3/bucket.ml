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
        element "Name" (text t.name);
        element "CreationDate" (timestamp_unix_to_iso t.creation_date |> text)
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

  let examples = [{
    owner = {
      id = "e0c1437646a118276693ab4c625667";
      display_name = "myuser";
    };
    buckets = [
      { name = "other"; creation_date = Calendar.make 2016 10 24 17 05 07  };
      { name = "bucket1"; creation_date = Calendar.make 2016 10 24 17 01 05 };
    ]
    }, "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ListAllMyBucketsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01\"><Owner><ID>e0c1437646a118276693ab4c625667</ID><DisplayName>myuser</DisplayName></Owner><Buckets><Bucket><Name>bucket1</Name><CreationDate>2016-10-24T17:01:05.000Z</CreationDate></Bucket><Bucket><Name>other</Name><CreationDate>2016-10-24T17:05:07.000Z</CreationDate></Bucket></Buckets></ListAllMyBucketsResult>"
    ]

  type kind = Policy.bucket
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
      last_modified: Calendar.t;
      size: Int64.t;
      storage_class: StorageClass.t;
    }
    type t = {
      contents: contents list;
      common_prefixes : string list; (* counts as 1 towards max_keys *)
      delimiter: string option;
      encoding_type: string option;
      is_truncated: bool;
      marker: string option; (* V1 *)
      max_keys: int;
      name: string;
      next_marker: string option; (* V1 *)
      owner: Acl.CanonicalUser.t option; (* V2: optional *)
      prefix: string option;
      continuation_token: string option;(* V2 *)
      key_count: int option; (* V2 *)
      next_continuation_token: string option;(* V2 *)
      start_after: string option; (* V2 *)
    }

    type kind = Policy.bucket
    let policy = Head.policy

    let empty = {
      contents = [];
      common_prefixes = [];
      delimiter = None;
      encoding_type = None;
      is_truncated = true;
      marker = None;
      max_keys = 0;
      name = "";
      next_marker = None;
      owner = None;
      prefix = None;
      continuation_token = None;
      key_count = None;
      next_continuation_token = None;
      start_after = None;
    }

    let xml_of_contents (c:contents) =
      element "Contents" (
        add_opt_element "Owner" Acl.CanonicalUser.to_xml c.owner
          [
            element "Key" (text c.key);
            element "LastModified" (timestamp_unix_to_iso c.last_modified |> text);
            element "ETag" (text c.etag);
            element "Size" (Int64.to_string c.size |> text);
            element "StorageClass" (text c.storage_class)
          ])

    let add_opt f lst init =
      List.rev_append (List.rev_map f lst) init

    let prefix str = element "CommonPrefixes" [ element "Prefix" (text str) ]

    let to_xml t =
      root "ListBucketResult" (
        (element "Name" (text t.name) ::
         element "Prefix" (opt text t.prefix) ::
         element "KeyCount" (opt text_of_int t.key_count) ::
         element "MaxKeys" (text_of_int t.max_keys) ::
         element "IsTruncated" (text (string_of_bool t.is_truncated)) ::
         add_opt xml_of_contents t.contents (
           add_opt prefix t.common_prefixes [])
        ) |>
        add_opt_element "NextContinuationToken" text t.next_continuation_token |>
        add_opt_element "StartAfter" text t.start_after |>
        add_opt_element "Delimiter" text t.delimiter |>
        add_opt_element "Encoding-Type" text t.encoding_type |>
        add_opt_element "ContinuationToken" text t.continuation_token |>
        add_opt_element "Marker" text t.marker |>
        add_opt_element "NextMarker" text t.marker
      )

    let to_reply t =
      (Header.init (), Some (to_xml t))

    let example_contents2 = {
      key = "image.jpg";
      last_modified = Calendar.make 2015 09 07 11 34 59;
      etag = "\"044592b81a09b47a9e1d665e615a50aa\"";
      size = 1516L;
      storage_class = "STANDARD";
      owner = None;
    }

    let example_contents1 = {
      key = "Example.txt";
      last_modified = Calendar.make 2016 10 24 19 00 09;
      etag = "\"044592b81a09b47a9e1d665e615a502f\"";
      size = 1457L;
      storage_class = "REDUCED_REDUNDANCY";
      owner = None;
    }

    let examples = [{
        empty with
        name = "bucket1";
        prefix = None;
        key_count = Some 2;
        max_keys = 1000;
        is_truncated = false;
        contents = [ example_contents1; example_contents2 ];
      },
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01\"><Name>bucket1</Name><Prefix/><KeyCount>2</KeyCount><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated><Contents><Key>Example.txt</Key><LastModified>2016-10-24T19:00:09.000Z</LastModified><ETag>&quot;044592b81a09b47a9e1d665e615a502f&quot;</ETag><Size>1457</Size><StorageClass>REDUCED_REDUNDANCY</StorageClass></Contents><Contents><Key>image.jpg</Key><LastModified>2015-09-07T11:34:59.000Z</LastModified><ETag>&quot;044592b81a09b47a9e1d665e615a50aa&quot;</ETag><Size>1516</Size><StorageClass>STANDARD</StorageClass></Contents></ListBucketResult>";
       {
         empty with
         name = "example";
         prefix = Some "pics/2009/";
         key_count = Some 3;
         max_keys = 1000;
         is_truncated = false;
         delimiter = Some "/";
         contents = [ {
             key = "pics/2009/";
             last_modified = Calendar.make 2016 10 20 17 01 16;
             etag = "\"d41d8cd98f00b204e9800998ecf8427e\"";
             size = 0L;
             storage_class = "STANDARD";
             owner = None;
           }
         ];
         common_prefixes = [ "pics/2009/Feb/"; "pics/2009/Jan/" ]
       }, "<?xml version=\"1.0\" encoding=\"UTF-8\"?><ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01\"><Delimiter>/</Delimiter><Name>example</Name><Prefix>pics/2009/</Prefix><KeyCount>3</KeyCount><MaxKeys>1000</MaxKeys><IsTruncated>false</IsTruncated><Contents><Key>pics/2009/</Key><LastModified>2016-10-20T17:01:16.000Z</LastModified><ETag>&quot;d41d8cd98f00b204e9800998ecf8427e&quot;</ETag><Size>0</Size><StorageClass>STANDARD</StorageClass></Contents><CommonPrefixes><Prefix>pics/2009/Feb/</Prefix></CommonPrefixes><CommonPrefixes><Prefix>pics/2009/Jan/</Prefix></CommonPrefixes></ListBucketResult>"

      ]

  end

end

module GetObjectVersions = struct
  let policy = Policy.Permission.bucket "s3:ListBucketVersions"
end

module ListMultipartUploads = struct
  type t
  type kind = Policy.bucket
  let policy = Policy.Permission.bucket "s3:ListBucketMultipartUploads"

  let to_reply _ = failwith "TODO"
end
