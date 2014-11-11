(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
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
module Make(SXIO:Sigs.SXIOSig)(IO: Sigs.EventIOSig with type 'a t = 'a SXIO.M.t)
: sig
  val tests : test
end = struct
  module Server = struct
    type t = {
      mutable hstatus: Nethttp.http_status;
      mutable headers: (string * string) list;
      buf: Buffer.t
    }
    type u = t
    type 'a monad = 'a IO.t
    module M = SXIO.M

    let log _ _ = ()
    open Dispatch
    let send_data s (str, pos, len) =
      Buffer.add_substring s.buf str pos len;
      M.return ()
    let send_headers s ?body_header h =
      s.hstatus <- h.status;
      begin match h.content_length with
      | Some len ->
        s.headers <-
          ("Content-Length",Int64.to_string len) ::
          h.reply_headers;
      | None ->
        s.headers <- h.reply_headers
      end;
      begin match h.content_type with
      | None -> ()
      | Some c ->
          s.headers <- ("Content-Type", c) :: s.headers
      end;
      begin match h.last_modified with
      | None -> ()
      | Some c ->
          s.headers <- ("Last-Modified", Util.format_date c) :: s.headers
      end;
      begin match h.etag with
      | None -> ()
      | Some c ->
          s.headers <- ("ETag", c) :: s.headers
      end;
      match body_header with
      | Some b ->
        M.(>>=) (send_data s (b, 0, String.length b))
          (fun () -> M.return s)
      | None -> M.return s
  end


  module D = Dispatch.Make(SXIO)(IO)(Server)
  let test_init () =
    Configfile.buckets_dir := Filename.temp_file "libres3test" ".tmpdir";
    Sys.remove !Configfile.buckets_dir;
    SXIO.M.run (D.init ());;

  open HttpTest
  open SXIO.M

  let map_method meth source = match meth with
  | (`GET | `HEAD | `DELETE | `TRACE | `OPTIONS) as m -> m
  | `POST ->
      `POST source
  | `PUT ->
      `PUT source
  ;;

  let perform_queries dispatcher lst =
    run (
      let server = { Server.hstatus = `Service_unavailable; Server.headers = []; Server.buf = Buffer.create 4096 } in
      IO.rev_map_p (fun req ->
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
          } >>= fun () ->
          return server
      ) lst >>= IO.rev_map_p (fun server ->
        let body = Buffer.contents server.Server.buf in
        return {
          headers = server.Server.headers;
          code = Nethttp.int_of_http_status server.Server.hstatus;
          body = body;
        }
      ) >>= fun r -> return (List.rev r)
    );;

  let build_tests dispatcher name =
    HttpTest.generate_tests name (perform_queries dispatcher) (S3TestData.suite false);;

  let tests =
    let dispatcher = test_init () in
    "server">:::[
      build_tests dispatcher "queries";
      "cleanup">::(fun () ->
        (* volume deletes are disabled, so rmdir them here *)
        let dir = !Configfile.buckets_dir in
        if dir <> "" && dir <> "/" then
          TestUtil.rmdirs dir
      )
    ]
end
