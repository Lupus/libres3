(**************************************************************************)
(*  Copyright (C) 2014-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for     *)
(*  any purpose with or without fee is hereby granted, provided that the  *)
(*  above copyright notice and this permission notice appear in all       *)
(*  copies.                                                               *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL         *)
(*  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED         *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE      *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL  *)
(*  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA    *)
(*  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER     *)
(*  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR      *)
(*  PERFORMANCE OF THIS SOFTWARE.                                         *)
(**************************************************************************)

open Live_config
open Boundedio
open Cohttp
open Jsonenc

let none = Ipaddr.V4 Ipaddr.V4.unspecified

let resolve_opt = function
| None -> return []
| Some host ->
    Lwt_unix.getaddrinfo host "0" [AI_SOCKTYPE SOCK_STREAM] >>= fun addrs ->
    List.rev_map (function
      | {Lwt_unix.ai_addr=ADDR_INET (addr,_);_} -> Ipaddr_unix.of_inet_addr addr
      | _ -> Ipaddr.V4 Ipaddr.V4.unspecified
      ) addrs |> List.filter (fun ip -> Ipaddr.compare ip none != 0) |>
    Lwt.return

let sx_service ((),req,(body:Body.t)) =
  Logs.debug (fun m -> m "SX req: %a" Request.pp_hum req);
  Http_client.service (req, (body :> Cohttp_lwt_body.t))

let main uri recurse =
  load_sx uri >>= fun sx ->
  let open Sx_config in
  resolve_opt sx.hostname >>= fun nodes ->
  let nodes = List.rev_append sx.nodes nodes in
  let host = List.hd nodes |> Ipaddr.to_string in
  let base_uri = Uri.make ~scheme:"https" ~host () in

  let vol, filter = match Astring.String.cuts ~empty:false ~sep:"/" (Uri.path uri) with
  | vol :: filter -> vol, filter
  | [] -> invalid_arg "No volume specified" in

  let vol = Sx_volume.T.v vol in

  let filter = Sx_types.Pattern.of_literal (Astring.String.concat ~sep:"/" filter) in

  let uri' = Uri.resolve "" base_uri (Sx_volume.Locate.get vol) in
  let req = { (Request.make_for_client `GET uri') with
              resource = Uri.to_string uri' } in
  Logs.debug (fun m -> m "request: %a" Request.pp_hum req);
  Sky.filter sx_service (sx.token,req,Body.empty) >>= fun (resp, body) ->
  Logs.debug (fun m -> m "Response: %a" Response.pp_hum resp);
  body |> Cohttp_lwt_body.to_stream |> Jsonio.of_strings |>
  Jsonio.to_json >>= fun json ->
  let locate = Json_encoding.destruct Sx_volume.Locate.encoding json in

  let host = List.hd locate.Sx_volume.Locate.node_list |> Ipaddr.to_string in
  let base_uri = Uri.make ~scheme:"https" ~host () in

  let uri' = Sx_volume.ListFiles.get ~filter ~recursive:recurse vol in
  let uri' = Uri.resolve "" base_uri uri' in

  let req = { (Request.make_for_client `GET uri') with
              resource = Uri.to_string uri' } in
  Logs.debug (fun m -> m "request: %a" Request.pp_hum req);
  Sky.filter sx_service (sx.token,req,Body.empty) >>= fun (resp, body) ->
  Logs.debug (fun m -> m "Response: %a" Response.pp_hum resp);
  body |> Cohttp_lwt_body.to_stream |> Jsonio.of_strings |>
  Jsonio.expect_object >>= fun s ->
  Jsonio.to_string s >>= fun str ->
  print_endline str;
  return_unit

let run uri recurse () =
  try Lwt_main.run (main uri recurse)
  with
  | Invalid_argument s ->
      Logs.err (fun m -> m "Invalid argument: %s" s)
  |  e ->
      let bt = Printexc.get_raw_backtrace () in
      Logs.err (fun m -> m "ERROR: %a" Fmt.exn_backtrace (e,bt))

open Cmdliner

let setup style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ())

let init ?docs () =
  Term.(pure setup $ Fmt_cli.style_renderer ?docs () $ Logs_cli.level ?docs ())

let uri : Uri.t Arg.converter =
  (fun str -> `Ok (Uri.of_string str)),
  Fmt.(using Uri.to_string string)

let required_pos ?docv ?doc n conv = Arg.(required & pos n (some conv) None & info [] ?docv ?doc)

let app =
  let doc = "ls test" in
  let recursive = Arg.(value & flag & info ["r";"recursive"]) in
  Term.(const run $ required_pos 0 uri ~docv:"URI" $ recursive $ init ()),
  Term.info "ls" ~version:"%%VERSION%%" ~doc

let () =
  match Term.eval app with
  | `Ok () | `Version | `Help -> ()
  | `Error _ -> exit 1
