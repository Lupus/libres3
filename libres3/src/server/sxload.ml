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
open Lwt
let (|>) x f = f x

let eq = Re_str.regexp " *= *"
let split_kv str =
  if String.length str > 0 && str.[0] = '#' then None
  else match Re_str.bounded_split eq str 2 with
  | [k; v] -> Some (k, v)
  | _ -> failwith (Printf.sprintf "Expected 'key=value' format, but got '%s'" str)

let parse_config configfile =
  Lwt_io.with_file ~mode:Lwt_io.input configfile (fun ch ->
      Lwt_io.read_lines ch |>
      Lwt_stream.filter_map split_kv |>
      Lwt_stream.to_list
    )

let load_nodes sxdir =
  Lwt_unix.files_of_directory (Filename.concat sxdir "nodes") |>
  Lwt_stream.filter_map (function
      |"." | ".." -> None
      | entry -> Ipaddr.of_string entry
    ) |>
  Lwt_stream.to_list

type t = {
  nodes : Ipaddr.t list;
  use_ssl : bool;
  port : int option;
  uuid : string;
  user : string;
  token : string;
}

let load ?configdir ?(user="default") ~host =
  let sxdir = match configdir with
    | None -> List.fold_left Filename.concat (Sys.getenv "HOME") [".sx"; host]
    | Some dir -> dir in
  let tokenfile = Filename.concat sxdir (Filename.concat "auth" user) in
  let configfile = Filename.concat sxdir "config" in
  parse_config configfile >>= fun config ->
  Lwt_io.with_file ~mode:Lwt_io.input tokenfile (Lwt_io.read ?count:None) >>= fun token ->
  load_nodes sxdir >>= fun nodes ->
  let use_ssl = try List.assoc "UseSSL" config = "Yes" with Not_found -> false in
  let port = try Some (int_of_string (List.assoc "HttpPort" config)) with Not_found -> None in
  let uuid = try List.assoc "ClusterUUID" config
    with Not_found -> invalid_arg ("Cluster UUID is missing in " ^ configfile) in
  Lwt.return {
    nodes = nodes;
    use_ssl = use_ssl;
    port = port;
    uuid = uuid;
    user = user;
    token = token;
  }


