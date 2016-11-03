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

open Rresult
open Cohttp_lwt_unix
open Cohttp
open Astring
open Lwt
open S3_to_sx
open S3_headers

let of_xml (headers, xml) =
  let status, body = match xml with
  | Some xml -> `OK, Xmlio.to_string xml
  | None -> `No_content, "" in
  Server.respond_string ~status ~headers ~body ()

let subresources = ["accelerate"; "acl"; "cors"; "lifecycle"; "policy"; "logging"; "notification"; "replication"; "tagging"; "website"]

let get_subresource = function
| [res, []] when List.mem res subresources -> Some res
| _ -> None

let dispatch r body =
  let handle_service () =
    service (GetService ()) >|= Bucket.Service.to_reply
  in
  let handle_bucket bucket =
    match get_subresource (Uri.query r.uri) with
    | None -> begin match r.meth with
      | `PUT ->
          let attr = R.get_ok (Bucket.Create.of_request (r.headers, bucket, None)) in
          service (CreateBucket ((),bucket,attr))
      | `DELETE ->
          service (DeleteBucket ((), bucket))
      end >>= Server.respond_string ~status:`OK ~body:""
    | Some sub -> begin match r.meth with
      | `PUT ->
          Cohttp_lwt_body.to_string body >>= fun body ->
          begin match Xmlio.of_string body with
          | Some xml ->
              service (PutBucketSubresource ((), bucket, sub, xml)) >>=
              Server.respond_string ~status:`OK ~body:""
          | None -> invalid_arg "xml"
          end
      | `DELETE ->
          service (DeleteBucketSubresource ((), bucket, sub)) >>=
          Server.respond_string ~status:`OK ~body:""
      | `GET ->
          service (GetBucketSubresource ((), bucket, sub)) >>= fun xml ->
          let body = Xmlio.to_string xml in
          Server.respond_string ~status:`OK ~body ()
      end
  in
  let handle_object bucket key =
    Server.respond_string ~status:`Not_implemented ~body:"" ()
  in
  begin match r.bucket, r.key with
  | None, None -> handle_service () >>= of_xml
  | Some bucket, None -> handle_bucket bucket
  | Some bucket, Some key -> handle_object bucket key
  | None, Some _ -> assert false
  end >>= fun (resp, body) ->
  begin if Logs.level () = Some Logs.Debug &&
           Response.status resp = `Not_implemented then
      Cohttp_lwt_body.to_string body >>= fun body ->
      Logs.debug (fun m -> m "Body(%d):@,%s" (String.length body) body);
      return ()
    else return ()
  end >>= fun () ->
  return (resp, body)

let handle _conn =
  dispatch |> wrapper |> fallback_handler
