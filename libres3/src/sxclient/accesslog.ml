(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open Lwt

module type Info = sig
  type t
  val body_bytes_sent : t -> int64 option
  val http_referer : t -> string option
  val http_user_agent : t -> string option
  val remote_addr : t -> string
  val remote_user : t -> string option
  val request : t -> string
  val status : t -> int
  val time_local : unit -> string
  (*
  val bytes_sent: t -> int64
  val connection: t -> int64
  val connection_requests: t -> int64
  val content_length : t -> int64 option
  val content_type : t -> string
  val host : t -> string
  val https: t -> bool
  val pid : t -> int
  val query_string: t -> string
  val remote_port : t -> int
  val request_length : t -> int
  val request_time : t -> float
  val scheme : t -> string
  val server_addr : t -> string
  val server_name : t -> string
  val server_port : t -> int64
  val msec : t -> float
  val pipe : t -> bool
  val uri : t -> string
  *)

end

let log = ref (None, Lwt_io.null)
let reopen ?path () =
  match path, !log with
  | Some path, (_, old) | None, (Some path, old) ->
    Lwt_io.open_file ~mode:Lwt_io.output path >>= fun fd ->
    log := Some path, fd;
    Lwt_io.close old
  | _ -> return_unit

let combined = "$remote_addr - $remote_user [$time_local] \"$request\" $status $body_bytes_sent \"$http_referer\" \"$http_user_agent\""

module Make(I:Info) = struct

  let opt = function
    | None -> "-"
    | Some v -> v

  let substitute i = function
    |"body_bytes_sent" ->
      begin match I.body_bytes_sent i with
        | None | Some 0L -> "-"
        | Some n -> Int64.to_string n
      end
    |"http_referer" -> opt (I.http_referer i)
    |"http_user_agent" -> opt (I.http_user_agent i)
    |"remote_addr" -> I.remote_addr i
    |"remote_user" -> opt (I.remote_user i)
    |"request" -> I.request i
    |"status" -> string_of_int (I.status i)
    |"time_local" -> I.time_local ()
    | var -> failwith ("Unsupported log variable: " ^ var)

  let stdout = Lwt_log.channel ~template:"$(message)" ~close_mode:`Keep ~channel:Lwt_io.stdout ()

  let log ~template i =
    let b = Buffer.create 256 in
    Buffer.add_substitute b (substitute i) template;
    let line = Buffer.contents b in
    ignore_result (Lwt_io.write_line (snd !log) line);
    if Lwt_log.Section.level EventLog.section <= Lwt_log.Notice then
      Lwt_log.ign_notice ~logger:stdout line
end
