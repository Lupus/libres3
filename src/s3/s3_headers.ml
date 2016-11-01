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

open Cohttp
open Astring
open Rresult

type t = {
  meth : Code.meth;
  uri : Uri.t;
  version : Code.version;
  authorization: string option;
  content_length : int64 option;
  content_type: string option;
  content_md5: Digest.t option;
  date: Http_date.t option;
  expect: string option;
  host: string option;
  x_amz_content_sha256: string option;
  x_amz_date : Http_date.t option;
  x_amz_security_token : string option;
  id: string;
  id2: string;
  bucket: string option;
  key: string option;
}

let get_other_auth expected h =
  match Header.get_authorization h with
  | None | Some (`Basic _) -> None
  | Some (`Other s) ->
      match String.cut ~sep:" " s with
      | Some (actual, auth) when actual = expected -> Some auth
      | Some (unknown, _) ->
          Logs.debug (fun m -> m "Unknown auth type %S" unknown);
          None
      | _ -> None

let parse_header field f h =
  match Header.get h field with
  | None -> None
  | Some s ->
      try Some (f s)
      with e ->
        Logs.debug (fun m -> m "Cannot parse header %S: %a" field Fmt.exn e);
        None

let generate_request_id () =
  Printf.sprintf "%04Lx" (Random.int64 Int64.max_int)

let digest_opt s =
  Digest.string (match s with | Some s -> s | None -> "")

let validate r =
  if r.meth = `PUT && r.content_length = None then
    R.error_msg "Content-length missing, but PUT requires it"
  else if r.host = None && r.version = `HTTP_1_1 then
    R.error_msg "HTTP/1.1 requires Host header"
  else if r.authorization <> None && r.date = None && r.x_amz_date = None then
    R.error_msg "Missing Date and x-amz-date headers"
  else
    R.ok r

let of_request req =
  let h = Request.headers req in
  let uri = Request.uri req in
  let path = Uri.path uri in
  let rpath = String.drop ~max:1 path in
  let bucket, key = match String.cut ~sep:"/" rpath, rpath with
  | None, "" -> None, None
  | None, bucket -> Some bucket, None
  | Some (bucket, key), _ -> Some bucket, Some key
  in
  let authorization = get_other_auth "AWS" h in
  validate {
    meth = Request.meth req;
    uri = uri;
    version = Request.version req;
    authorization;
    content_length = Header.get_content_range h;
    content_type = Header.get_media_type h;
    content_md5 = parse_header "Content-MD5" B64.decode h;
    date = Http_date.of_header h;
    expect = Header.get h "Expect";
    host = Uri.host uri;
    x_amz_content_sha256 = Header.get h "x-amz-content-sha256";
    x_amz_date = Http_date.of_header ~field:"x-amz-date" h;
    x_amz_security_token = Header.get h "x-amz-security-token";
    bucket; key;
    id = generate_request_id ();
    id2 =
      [digest_opt bucket; digest_opt key; digest_opt authorization] |>
      String.concat ~sep:"" |> B64.encode
  }

module ETag = struct
  type t = string

  let of_md5 d = "\"" ^ (Digest.to_hex d) ^ "\""
  let of_md5_list lst =
    Printf.sprintf "\"%s-%d\""
      (String.concat ~sep:"" lst |> Digest.string)
      (List.length lst)

  let pp = Fmt.string
end

let add_header_opt field v lst = match v with
| Some v -> (field, v) :: lst
| None -> lst

let server_name = "LibreS3-%%VERSION%%"

let respond req ?x_amz_delete_marker ?x_amz_version_id ?etag ?content_type
    ~content_length (status:[< Cohttp.Code.status_code]) =
  (* keepalive only on well-behaved clients *)
  let connection = match status with
  | #Code.informational_status | #Code.success_status | `Not_found -> "open"
  | #Code.redirection_status | #Code.client_error_status
  | #Code.server_error_status  | `Code _ -> "close"
  in
  let date = Unix.gettimeofday () |> Http_date.of_unix_timestamp in
  let delete_marker = match x_amz_delete_marker with
  | Some b -> Some (string_of_bool b)
  | None -> None in
  let headers =
    ["Connection", connection;
     "Server", server_name;
     "x-amz-id-2", req.id2;
     "x-amz-request-id", req.id;
    ] |>
    add_header_opt "x-amz-delete-marker" delete_marker |>
    add_header_opt "x-amz-version-id" x_amz_version_id |>
    add_header_opt "Content-Type" content_type |>
    add_header_opt "ETag" etag |>
    Header.of_list |>
    Http_date.add_header date
  in
  let encoding = Transfer.Fixed content_length in
  Response.make ~encoding ~headers ~status:(status :> Cohttp.Code.status_code) ()

let respond_xml req status xml =
  let body = xml |> Xmlio.to_string in
  let content_length = body |> String.length |> Int64.of_int in
  respond req ~content_type:"application/xml" ~content_length status,
  Body.of_string body

let respond_error req err =
  let code, message, status = S3_error.info err in
  let open Xmlio in
  element "Error" [
    element "Code" (text code);
    element "Message" (text message);
    element "Resource" (text (Uri.path req.uri));
    element "RequestId" (text req.id)
  ] |> respond_xml req status
