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

type t = {
  meth : Cohttp.Code.meth;
  uri : Uri.t;
  version : Cohttp.Code.version;
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

module ETag : sig
  type t = private string
  val of_md5 : Digest.t -> t
  val of_md5_list : Digest.t list -> t
  val pp : t Fmt.t
end

open Cohttp
val of_request : Request.t -> (t, R.msg) result

val respond : t -> ?x_amz_delete_marker:bool -> ?x_amz_version_id:string ->
  ?etag:ETag.t -> ?content_type:string -> content_length:int64 ->
  [< Code.status_code] -> Response.t
val respond_xml : t -> [< Code.status_code] -> Xmlio.xml -> Response.t * Body.t
val respond_error : t -> S3_error.t -> Response.t * Body.t

