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

open Ocsigen_extensions
open Ocsigen_http_frame
open Lwt
open SXDefaultIO

type access = {
  user: string option;
  undecoded_url: string;
  ri  : Ocsigen_request_info.request_info;
  body: int64 option;
  code: int
}

module Log = Accesslog.Make(struct
    type t = access

    let body_bytes_sent i =
      i.body

    let string_opt = function
      | "" -> None
      | s -> Some s

    let http_referer i =
      Ocsigen_headers.get_referer (Ocsigen_request_info.http_frame i.ri)

    let http_user_agent i =
      string_opt (Ocsigen_request_info.user_agent i.ri)

    let remote_addr i =
      Ocsigen_request_info.remote_ip i.ri

    let remote_user i = i.user

    let request i =
      Printf.sprintf "%s %s %s"
        (Framepp.string_of_method (Ocsigen_request_info.meth i.ri))
        i.undecoded_url
        (Framepp.string_of_proto (Ocsigen_request_info.protocol i.ri))

    let status i = i.code

    let clf = "%d/%b/%Y:%H:%M:%S %z"
    let time_local () =
      Netdate.mk_date ~localzone:true ~fmt:clf (Unix.gettimeofday ())
end)

module Server = struct
  type t = {
    mutable headers: Dispatch.headers option;
    mutable woken: bool;
    mutable woken_body: bool;
    mutable stream_error : bool;
    mutable auth_user : string option;
    mutable body_sent: int64;
    mutable info : access option;
    headers_wait: unit Lwt.t;
    headers_wake: unit Lwt.u;
    body_stream : string Lwt_stream.t;
    body_stream_push : string Lwt_stream.bounded_push;
  }
  type u = t
  let send_data s (str, pos, len) =
    if s.stream_error then return_unit
    else
      let data =
        if len = String.length str then str
        else String.sub str pos len in
      s.body_stream_push#push data

  let send_headers s ?body_header h =
    s.headers <- Some h;
    if not s.woken then begin
      (* send headers and body xml header only once *)
      s.woken <- true;
      Lwt.wakeup s.headers_wake ();
      match body_header with
      | Some b -> send_data s (b, 0, String.length b) >>= fun () ->
        return s
      | None -> return s
    end else begin
      s.stream_error <- true;
      return s
    end

  let set_user s user = s.auth_user <- Some user
  let log _ str = Ocsigen_messages.warning str
end

open Ocsigen_http_frame.Http_header
open EventIO
module OcsigenServer =
  Dispatch.Make(Server)
module D = Dispatch

let conv_method m src = match m with
  |  Http_header.GET -> `GET
  |  Http_header.POST -> `POST src
  |  Http_header.HEAD -> `HEAD
  |  Http_header.PUT -> `PUT src
  |  Http_header.DELETE -> `DELETE
  |  Http_header.TRACE -> `TRACE
  |  Http_header.OPTIONS -> `OPTIONS
  |  Http_header.CONNECT -> `CONNECT
  |  Http_header.LINK -> `LINK
  |  Http_header.UNLINK -> `UNLINK
  |  Http_header.PATCH -> `PATCH;;

open OcsigenServer

let convert_headers h =
  List.fold_left (fun a (name, value) ->
      Http_headers.add (Http_headers.name name) value a
    ) Http_headers.empty h;;

let empty_read () =
  Ocsigen_stream.empty None

let return_eof server () =
  if not server.Server.woken then
    Lwt.wakeup server.Server.headers_wake ();
  server.Server.body_stream_push#close;
  return_unit

let stream_of_reply wait_eof server =
  ignore_result (wait_eof >>= return_eof server);
  let rec read () =
    Lwt_stream.get server.Server.body_stream >>= function
    | None ->
      if server.Server.stream_error then begin
        Server.log () "error encountered after headers already sent: truncating reply";
        Lwt.fail Lwt.Canceled
      end else
        Ocsigen_stream.empty None
    | Some substr ->
      let len = String.length substr in
      EventLog.debug (fun () ->  Printf.sprintf "send: %d" len);
      server.Server.body_sent <- Int64.add server.Server.body_sent (Int64.of_int len);
      Ocsigen_stream.cont substr read
  in
  Ocsigen_stream.make ~finalize:(fun _ ->
      Lwt.cancel wait_eof;
      begin match server.Server.info with
      | Some info ->
        Log.log ~template:Accesslog.combined { info with body = Some server.Server.body_sent }
      | None -> ()
      end;
      return_unit) read
;;

let max_input_mem = 65536L

let stream_of arg pos =
  if pos <> 0L then
    fail (Failure "position is not 0 in input")
  else
    let s = ref (Ocsigen_stream.get arg) in
    return (fun () ->
        Ocsigen_stream.next !s >>= function
        | Ocsigen_stream.Finished _ -> return ("",0,0)
        | Ocsigen_stream.Cont (buf, next_stream) ->
          s := next_stream;
          return (buf, 0, String.length buf)
      );;

let source_of arg size =
  return (`Source {
      meta = { name = ""; size = size; mtime = 0.; etag = "" };
      seek = stream_of arg
    })

let empty_stream () = Ocsigen_stream.of_string ""

let stream_of_request ri =
  match Ocsigen_request_info.meth ri with
  | Http_header.POST | Http_header.PUT ->
    begin match (Ocsigen_request_info.http_frame ri).frame_content with
      | None -> empty_stream ()
      | Some input -> input
    end
  | _ -> empty_stream ();;

let process_request dispatcher ri () =
  let headers = Http_headers.fold (fun name values accum ->
      let namestr = Http_headers.name_to_string name in
      List.rev_append (List.map (fun v -> namestr, v) values) accum
    ) (Ocsigen_request_info.http_frame ri).frame_header.headers [] in
  let stream = stream_of_request ri in
  let body_stream, body_stream_push = Lwt_stream.create_bounded 16 in
  let w, u = Lwt.wait () in
  let server = {
    Server.headers = None; stream_error = false;
    auth_user = None; body_sent = 0L; info = None;
    headers_wait = w; headers_wake = u; woken=false;
    woken_body=false;
    body_stream = body_stream; body_stream_push = body_stream_push } in
  let cl = match Ocsigen_request_info.content_length ri with
    | Some l -> l
    | None -> 0L in
  source_of stream cl >>= fun (`Source source) ->
  let req_method = conv_method (Ocsigen_request_info.meth ri) source in
  let undecoded_url = match (Ocsigen_request_info.http_frame ri).frame_header.mode with
    | Query (_, url) -> url
    | _ -> "/" ^ (Ocsigen_request_info.url_string ri) in
  let wait_eof =
    handle_request dispatcher {
      server = server;
      meth = req_method;
      body_file = None;(* TODO: write to tmpfile *)
      info = {
        CanonRequest.req_headers = headers;
        undecoded_url = undecoded_url;
      };
    } in
  Ocsigen_senders.Stream_content.result_of_content
    (stream_of_reply wait_eof server) >>= fun res ->
  server.Server.headers_wait >|= fun () ->
  match server.Server.headers with
  | None -> res
  | Some h ->
    let code = Nethttp.int_of_http_status h.D.status in
    server.Server.info <- Some {
      user = server.Server.auth_user;
      undecoded_url = undecoded_url;
      ri = ri;
      body = h.D.content_length;
      code = code
    };
    Ocsigen_http_frame.Result.update res
      ~code
      ~content_length:(h.D.content_length)
      ~content_type:h.D.content_type
      ~headers:(convert_headers h.D.reply_headers)
      ~lastmodified:h.D.last_modified
      ~etag:h.D.etag_header ()
;;

open Dns.Packet

let rec get_txt = function
  | [] -> []
  | { Dns.Packet.rdata = TXT txt_list; _ } :: _ -> txt_list
  | _ :: tl -> get_txt tl

let query_txt resolver name =
  Dns_resolver_unix.resolve resolver Q_IN Q_TXT
    (Dns.Name.string_to_domain_name name)
  >>= fun packet ->
  return (get_txt packet.answers)

type version_status =
  | SecurityUpdate
  | Update
  | Noop

let check_version (vmaj,vmin,sec) (srcmaj,srcmin) =
  if vmaj > srcmaj then
    SecurityUpdate
  else if vmaj = srcmaj && vmin > srcmin then
    if sec || vmin > srcmin + 1 then SecurityUpdate
    else Update
  else
    Noop

let upgrade_msg security (maj,min) (srcmaj,srcmin) =
  Ocsigen_messages.warning (
    Printf.sprintf "%sLibreS3 %d.%d is available "
      (if security then "CRITICAL update found!" else "")
      maj min ^
    Printf.sprintf "(this node is running version %d.%d). " srcmaj srcmin ^
    Printf.sprintf
      "See http://www.skylable.com/products/libres3/release/%d.%d for upgrade instructions"
      maj min
  )

let dns_check resolver dns =
  query_txt resolver dns >>= function
  | [] ->
    Ocsigen_messages.console (fun () ->  (Printf.sprintf "Cannot check version: no TXT record for '%s'" dns));
    return ()
  | ver :: [] ->
    begin try
        Scanf.sscanf ver "%d.%d.%d" (fun maj min sec ->
            Scanf.sscanf Version.version "%d.%d" (fun srcmaj srcmin ->
                match check_version (maj,min,sec > 0) (srcmaj, srcmin) with
                | SecurityUpdate -> upgrade_msg true (maj,min) (srcmaj,srcmin)
                | Update -> upgrade_msg false (maj,min) (srcmaj,srcmin)
                | Noop -> ()
              )
          );
        return ()
      with _ ->
        Ocsigen_messages.console (fun () ->  "Cannot check version: bad version received");
        return ()
    end
  | _ ->
    Ocsigen_messages.console (fun () ->
        Printf.sprintf "Cannot check version: too many TXT records for '%s'" dns);
    return ()

let hdist_seed = 0x1337l (* must match SX *)
let self_id = Murmur.murmurhash64b
    (Anonymize.anonymize_item "SELF" (Unix.gethostname ()))
    hdist_seed

let check_url resolver url =
  SXIO.check url >>= function
  | Some uuid ->
    let buf = Buffer.create 32 in
    String.iter (fun c ->
        if c <> '-' then
          Buffer.add_char buf c
      ) uuid;
    let uuidhex = Buffer.contents buf in
    let uuidbin = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) uuidhex
    in
    let id = Murmur.murmurhash64b uuidbin hdist_seed in
    dns_check resolver (Printf.sprintf "%d.%016Lx%016Lx.s3ver.skylable.com" (Random.bits ()) self_id id)
  | None ->
    return ()

let noop e =
  Ocsigen_messages.warning (Printf.sprintf "version check error: %s" (Printexc.to_string e));
  return ()

let test_resolve resolver name =
  Dns_resolver_unix.gethostbyname resolver name >>= fun lst ->
  if lst = [] then begin
    Ocsigen_messages.warning
      (Printf.sprintf "Failed to lookup hostname %s via DNS" name);
    Ocsigen_messages.warning "S3 clients may not be able to access this server properly!"
  end else begin
    let ips = String.concat ", " (List.rev_map Ipaddr.to_string lst) in
    Ocsigen_messages.console (fun () ->  (Printf.sprintf "%s resolves to %s" name ips));
  end;
  return ()

let dns_check_wildcard resolver =
  ignore (test_resolve resolver !Configfile.base_hostname);
  ignore (test_resolve resolver (Printf.sprintf "test%d.%s"
                                   (Random.bits ()) !Configfile.base_hostname))

let rec dns_check_loop resolver url =
  Lwt.catch (fun () -> check_url resolver url) noop >>= fun () ->
  dns_check_wildcard resolver;
  OS.sleep !Configfile.check_interval >>= fun () ->
  dns_check_loop resolver url

let periodic_check () =
  match !Configfile.sx_host with
  | None -> return ()
  | Some host ->
    let url = SXIO.of_neturl (Neturl.make_url ~encoded:false
                                ~scheme:"sx" ~user:!Config.key_id ~port:!Config.sx_port
                                ~host ~path:[""] SXC.syntax) in
    Dns_resolver_unix.create () >>= fun resolver->
    dns_check_wildcard resolver;
    OS.sleep !Configfile.initial_interval >>= fun () ->
    dns_check_loop resolver url

let debug_logger =
  Lwt_log.channel ~template:"$(date).$(milliseconds): $(message)" ~close_mode:`Keep ~channel:Lwt_io.stderr ()

let fun_site _ config_info _ _ _ _ =
  Configfile.base_hostname := config_info.default_hostname;
  if Lwt_log.Section.level EventLog.section = Lwt_log.Debug then
    Lwt_log.default := Lwt_log.broadcast [ !Lwt_log.default; debug_logger ];
  let default = !Lwt_log.default in
  (* ignore default access.log messages *)
  Lwt_log.default := Lwt_log.dispatch (fun sect level ->
      if String.compare (Lwt_log.Section.name sect) "access" = 0 then
        Lwt_log.null
      else default
    );
  Ocsigen_messages.console (fun () ->
      Printf.sprintf "LibreS3 default hostname: %s"  !Configfile.base_hostname
    );
  begin try
      Ssl.set_cipher_list !Ocsigen_http_client.sslcontext
        "HIGH+AES128:HIGH:!DH:!SSLv2:!aNULL:!eNULL:!MD5";
    with _ -> () end;
  let dispatcher = Lwt_main.run (OcsigenServer.init ()) in
  Ocsigen_messages.console (fun () ->  "Startup complete");
  let _ = periodic_check () in
  function
  | Req_not_found (_, request) ->
    Lwt.return (Ext_found (process_request dispatcher request.request_info))
  | Req_found _ ->
    Lwt.return Ext_do_nothing
;;

let ping pipe =
  let chan = Lwt_chan.out_channel_of_descr pipe in
  Lwt_chan.output_char chan  'X' >>= fun () ->
  Lwt_chan.flush chan

let register_all pipe =
  Ocsigen_extensions.register_extension ~fun_site ~name:"libres3" ();
  Ocsigen_extensions.register_command_function ~prefix:"libres3"
    (fun s c -> match c with
       | ["ping"] ->
         ping (Lwt_unix.of_unix_file_descr pipe)
       | _ -> raise Ocsigen_extensions.Unknown_command)
