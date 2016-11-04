(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2016 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

open Rresult
open Cohttp_lwt_unix
open Cohttp
open Sx_services
open Sx_types
open Live_config
open Lwt

(* TODO: chain *)
exception Backoff of (float * float) option
exception JobError of string
exception SxError of Sx_types.Error.t
exception RetryFailed of exn list


let () = Printexc.register_printer (function
  | JobError msg -> Some ("SX Job error: " ^ msg)
  | SxError e -> Some ("SX Error: " ^ e.Error.error_message)
  | RetryFailed lst ->
      Some (Fmt.strf "Retries failed: @[%a@]" Fmt.(list exn) lst)
  | _ -> None)

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

let sx =
  Lwt_main.run(
    let uri = Uri.of_string "sx://admin@localhost" in
    load_sx uri)

let base_uri, cluster_nodes =
  Lwt_main.run(
  let uri = Uri.of_string "sx://admin@localhost" in
  load_sx uri >>= fun sx ->
  let open Sx_config in
  resolve_opt sx.hostname >|= fun nodes ->
  let scheme = if sx.ssl then "https" else "http" in
  let base_uri = Uri.make ~scheme ?port:sx.port () in
  let uri_of_node node =
    Uri.with_host base_uri (Some (Ipaddr.to_string node))
  in
  base_uri, List.rev_append sx.nodes nodes |>
            List.rev_map uri_of_node)

let uri_of_node node =
  Uri.with_host base_uri (Some (Ipaddr.to_string node))

let sx_service ((),req,(body:Body.t)) =
  Logs.debug (fun m -> m "SX req: %a" Request.pp_hum req);
  Http_client.service (req, (body :> Cohttp_lwt_body.t))



let rec sx_retry_loop next cmd interval () =
  let open Boundedio in
  let t0 = Unix.gettimeofday () in
  next cmd >>> function
  | Ok result ->
      Logs.debug (fun m -> m "got OK result");
      return result
  | Error (Backoff backoff) ->
      Logs.debug (fun m -> m "got backoff result");
      let min_interval, max_interval = match backoff with
      | Some v -> v
      | None -> 0.1, 10. in
      let interval = interval *. 2. in
      let wake_at = t0 +. (interval |> max min_interval |> min max_interval) in
      let t1 = Unix.gettimeofday () in
      let sleep_amount = wake_at -. t1 in
      (if sleep_amount > 0. then begin
          Logs.debug (fun m -> m "[@%.3f] backoff, sleeping for %f s" t1 sleep_amount);
          delay sleep_amount
        end
       else yield ()) >>= sx_retry_loop next cmd interval
  | Error e ->
      Logs.debug (fun m -> m "got error %a" Fmt.exn e);
      fail e
      
let sx_retry next cmd =
  sx_retry_loop next cmd 0.001 ()

let sx_make_job_request meth relative_uri ?input reply_encoding targets =
  let encoding = Json_encoding.(union [
      case reply_encoding R.to_option R.ok;
      case Sx_types.Error.encoding (fun x -> None) R.error
    ]) in
  begin match input with
  | None -> return Body.empty
  | Some (encoding, data) ->
      Json_encoding.construct encoding data |>
      Jsonio.of_json |>
      Jsonio.expect_object >>=
      Jsonio.to_string >|= Body.of_string
  end >>= fun body ->
  Logs.debug (fun m -> m "Making SX request: %s" (Body.to_string body));
  let rec loop errors = function
  | [] -> fail (RetryFailed errors)
  | node :: tl ->
      let uri = Uri.resolve "" node relative_uri in
      let req = { (Request.make_for_client meth uri) with
                  resource = Uri.to_string uri } in
      Logs.debug (fun m -> m "request: %a" Request.pp_hum req);
      Sky.filter sx_service (sx.token, req, body) >>= fun (resp, body) ->
      Logs.debug (fun m -> m "response: %a" Response.pp_hum resp);
      body |> Cohttp_lwt_body.to_stream |> Jsonio.of_strings |>
      (*Jsonio.observe ~prefix:"reply" |> *)
      Jsonio.to_json >>= fun json ->
      match Json_encoding.destruct encoding json with
      | Ok result ->
          Logs.debug (fun m -> m "got result");
          return (node, result)
      | Error e -> loop (SxError e :: errors) tl
  in loop [] targets

let sx_make_request meth relative_uri ?input reply_encoding targets =
  sx_make_job_request meth relative_uri ?input reply_encoding targets >>= fun (_,r) ->
  Logs.debug (fun m -> m "got non-job result");
  return r

let rec service : type a. a req -> a Boundedio.t = function
| WaitJob (node, job) ->
    let open Sx_types.Job in
    let uri' = Poll.get job.request_id in
    sx_make_request `GET uri' Poll.encoding [node] >>= fun r ->
    begin match r.request_status with
    | `Pending ->
        let backoff = job.min_poll_interval /. 1000., job.max_poll_interval /. 1000. in
        fail (Backoff (Some backoff))
    | `Ok -> return_unit
    | `Error msg -> fail (JobError msg)
    end
| ListVolumes () ->
    let open Sx_volume.ListVolumes in
    let uri' = get ~volume_meta:true ~custom_volume_meta:true () in
    sx_make_request `GET uri' encoding cluster_nodes
| CreateVolume (vol, attr) ->
    let open Sx_volume.Create in
    let uri' = put vol in
    sx_make_job_request `PUT uri' ~input:(encoding, attr) Job.encoding cluster_nodes
| DeleteVolume vol ->
    service (Locate vol) >>= fun (volnodes, _) ->
    Logs.debug (fun m -> m "deleting volume from %d volnodes" (List.length volnodes));
    let open Sx_volume.Delete in
    let uri' = delete vol in
    sx_make_job_request `DELETE  uri' Job.encoding (List.rev_map uri_of_node volnodes)
| Locate vol ->
    let open Sx_volume.Locate in
    let uri' = get ~volume_meta:true ~custom_volume_meta:true vol in
    sx_make_request `GET uri' encoding cluster_nodes >>= fun ((l, _) as r) ->
    Logs.debug (fun m -> m "got locate %d nodes" (List.length l));
    return r
| GetFileMeta (vol, key) ->
    service (Locate vol) >>= fun (volnodes, _) ->
    Logs.debug (fun m -> m "deleting volume from %d volnodes" (List.length volnodes));
    let open Sx_file.Meta in
    let uri = get vol key in
    sx_make_request `GET uri encoding (List.rev_map uri_of_node volnodes)
(*| InitializeFile (vol, key, lst) ->*)
| _ -> failwith "TODO"

