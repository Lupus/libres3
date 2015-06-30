(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open OUnit
module IO = EventIO
module Server = struct
  type t = unit
  type u = t

  let log _ _ = ()
  open Dispatch
  let transform_headers h =
    let headers = h.reply_headers in
    let headers = begin match h.content_length with
      | Some len -> ("Content-Length",Int64.to_string len) :: headers
      | None -> headers
    end in
    let headers = begin match h.content_type with
      | None -> headers
      | Some c -> ("Content-Type", c) :: headers
    end in
    let headers = begin match h.last_modified with
      | None -> headers
      | Some c -> ("Last-Modified", Util.format_date c) :: headers
    end in
    let headers = begin match h.etag_header with
      | None -> headers
      | Some c -> ("ETag", c) :: headers
    end in
    headers

  let set_user _ _ = ()
end


module D = Dispatch.Make(Server)
let test_init () =
  Lwt_main.run (D.init ());;

open HttpTest
open Lwt

let map_method meth source = match meth with
  | (`GET | `HEAD | `DELETE | `TRACE | `OPTIONS) as m -> m
  | `POST ->
    `POST source
  | `PUT ->
    `PUT source
;;

let perform_queries dispatcher lst =
  Lwt_main.run (
    let server = () in
    Lwt_list.rev_map_p (fun req ->
        let `Source source = SXIO.of_string req.req_body in
        let host = if req.port = 80 then req.host else
            Printf.sprintf "%s:%d" req.host req.port in
        D.handle_request dispatcher {
          server = server;
          meth = map_method req.meth source;
          D.body_file = None;
          info = {
            req_headers = ("Host",host) :: req.req_headers;
            CanonRequest.undecoded_url = req.relative_url;(* TODO: encode it! *)
          };
        }
      ) lst >>= Lwt_list.rev_map_p (fun (headers, body) ->
        body () >>= Lwt_stream.to_list >>= fun lst ->
        return {
          headers = Server.transform_headers headers;
          code = Nethttp.int_of_http_status headers.Dispatch.status;
          body = String.concat "" lst
        }
      ) >>= fun r -> return (List.rev r)
  );;

let build_tests dispatcher name =
  HttpTest.generate_tests name (perform_queries dispatcher) (S3TestData.suite false);;

let tests =
  let dispatcher = test_init () in
  "server">:::[
    build_tests dispatcher "queries";
  ]
