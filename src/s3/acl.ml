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

(* GET Bucket acl
   PUT Bucket acl
   GET Object acl
   PUT Object acl *)

open Xmlio
open Rresult
open Cohttp
open Astring

module Id = struct
  type t = string
end

module CanonicalUser = struct
  type t = {
    id : Id.t;
    display_name: string;
  }
  let to_xml t = [
        element "ID" [text t.id];
        element "DisplayName" [text t.display_name];
  ]
end

module Groups = struct
  type t = [`AuthenticatedUsers | `AllUsers | `LogDelivery]
  let authenticated_users =
    Uri.of_string "http://acs.amazonaws.com/groups/global/AuthenticatedUsers"
  let all_users =
    Uri.of_string "http://acs.amazonaws.com/groups/global/AllUsers"
  let log_delivery =
    Uri.of_string "http://acs.amazonaws.com/groups/s3/LogDelivery"

  let of_uri uri =
    if Uri.equal uri authenticated_users then Ok `AuthenticatedUsers
    else if Uri.equal uri all_users then Ok `AllUsers
    else if Uri.equal uri log_delivery then Ok `LogDelivery
    else R.error_msgf "%s is not a known predefined group" (Uri.to_string uri)
end

module Grantee = struct
  type t =
    | Owner | ObjectOwner
    | EmailAddress of string
    | ID of Id.t
    | Uri of Groups.t

  let is_quote c = c = '"'

  let of_header_field str =
    let unquote v = String.trim ~drop:is_quote v in
    match String.cut ~sep:"=" str with
    | Some ("emailAddress", v) -> Ok (EmailAddress (unquote v))
    | Some ("id", v) -> Ok (ID (unquote v))
    | Some ("uri", v) ->
        R.map (fun u -> Uri u) (v |> unquote |> Uri.of_string |> Groups.of_uri)
    | Some _ | None -> R.error_msgf "Unknown grantee type in %s" str
end

type t = {
  grant_read : Grantee.t list;
  grant_write : Grantee.t list;
  grant_read_acp : Grantee.t list;
  grant_write_acp : Grantee.t list;
}

let empty = {
  grant_read = [];
  grant_write = [];
  grant_read_acp = [];
  grant_write_acp = [];
}

let full_control grantee = {
  grant_read = [grantee];
  grant_write = [grantee];
  grant_read_acp = [grantee];
  grant_write_acp = [grantee];
}

let (@+) a b = {
  grant_read = List.rev_append a.grant_read b.grant_read;
  grant_write = List.rev_append a.grant_write b.grant_write;
  grant_read_acp = List.rev_append a.grant_read_acp b.grant_read_acp;
  grant_write_acp = List.rev_append a.grant_write_acp b.grant_write_acp;
}

let of_canned = function
| "private" -> full_control Owner
| "public-read" -> full_control Owner @+ {
    empty with
    grant_read = [Grantee.Uri `AllUsers]
  }
| "public-read-write" -> full_control Owner @+ {
    empty with
    grant_read = [Grantee.Uri `AllUsers];
    grant_write = [Grantee.Uri `AllUsers]
  }
| "aws-exec-read" -> full_control Owner (* @+ EC2 *)
| "authenticated-read" -> full_control Owner @+ {
    empty with grant_read = [Grantee.Uri `AuthenticatedUsers]
  }
| "bucket-owner-read" -> full_control ObjectOwner @+
                         {empty with grant_read=[Owner]}
| "bucket-owner-full-control" -> full_control ObjectOwner @+ full_control Owner
| "log-delivery-write" -> {
    empty with grant_write = [Grantee.Uri `LogDelivery];
    grant_read_acp = [Grantee.Uri `LogDelivery]
}
| _ -> failwith "TODO"

let values_of_header field h =
  let fold_split accum v =
    List.rev_append (String.cuts ~sep:"," v |> List.rev_map String.trim) accum
  in
  Header.get_multi h field |>
  List.fold_left fold_split []

let get_grants field h =
  values_of_header field h |>
  List.fold_left (fun accum v ->
      R.(
        accum >>= fun lst ->
        Grantee.of_header_field v >>= fun r ->
        return (r :: lst)
      )
    ) (Ok [])

let of_header h =
  R.(
    get_grants "x-amz-grant-read" h >>= fun grant_read ->
    get_grants "x-amz-grant-write" h >>= fun grant_write ->
    get_grants "x-amz-grant-read-acp" h >>= fun grant_read_acp ->
    get_grants "x-amz-grant-write-acp" h >>= fun grant_write_acp ->
    get_grants "x-amz-grant-full-control" h >>= fun grant_full_control ->
    return {
      grant_read = List.rev_append grant_full_control grant_read;
      grant_write = List.rev_append grant_full_control grant_write;
      grant_read_acp = List.rev_append grant_full_control grant_read_acp;
      grant_write_acp = List.rev_append grant_full_control grant_write_acp;
    })

let to_bucket_policy grants = failwith "TODO"
let to_obj_policy grants = failwith "TODO"
let to_subresource_policy grants = failwith "TODO"

module GetBucket = struct
  let policy = Policy.Permission.subresource "s3:GetBucketAcl"
end

module PutBucket = struct
  let policy = Policy.Permission.subresource "s3:PutBucketAcl"
end
