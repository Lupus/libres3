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

open Astring
open Rresult

module Token = struct
  type t = string
  let of_string str =
    let n = String.length str in
    if n <> 56 then
      R.error_msgf "Invalid token length: %d (expected 56)" n
    else
      try R.ok (B64.decode str)
      with _ ->
        R.error_msgf "Base64 decoding failed on %S" str
  let binary t = t
end

type t = {
  token : Token.t;
  uuid: Uuidm.t option;
  hostname : string option;
  port: int option;
  ssl: bool;
  nodes: Ipaddr.t list;
}

module Location = struct
  type path = string
  type t = {
    config: path;
    auth: path;
    nodes: path
  }

  let home () =
    try Sys.getenv "HOME"
    with Not_found -> "/root"

  let home_sx = home () ^ "/.sx"

  let expect_scheme scheme uri =
    if Uri.scheme uri <> Some scheme then
      R.error_msg "Scheme must be sx"
    else R.ok ()

  let profile uri = match Uri.user uri with
  | Some user -> user
  | None -> "default"

  let sx_cluster uri =
    uri |> Uri.host |> R.of_option ~none:(fun () -> R.error_msg "SX cluster must be specified") >>= function
    | "" -> R.error_msg "SX cluster cannot be empty"
    | s -> R.ok s

  let of_uri ?(dir=home_sx) uri =
    let reword error =
      R.msgf "%s: %s" error (Uri.to_string uri)
    in
    expect_scheme "sx" uri >>= fun () ->
    sx_cluster uri >>= fun cluster ->
    let dir = Filename.concat dir cluster in
    R.ok {
      auth = List.fold_left Filename.concat dir ["auth"; profile uri];
      config = Filename.concat dir "config";
      nodes = Filename.concat dir "nodes"
    } |> R.reword_error_msg ~replace:true reword
end

let uuid str =
  let none () = R.error_msgf "Cannot parse UUID %S" str in
  Uuidm.of_string str |> R.of_option ~none

let re_matches_full re str =
  match Re.exec_opt re str with
  | None -> None
  | Some group ->
      if Re.Group.stop group 0 = String.length str then Some str
      else None

let hostname str = re_matches_full Uri_re.host str

let parse_bool str = match (String.Ascii.lowercase str) with
| "yes" -> Some true
| "no" -> Some false
| _ -> None

let parse_nodes nodes =
  let fold accum s =
    accum >>= fun ips -> match Ipaddr.of_string s with
    | Some ip -> R.ok (ip :: ips)
    | None -> R.error_msgf "Cannot parse node address %S" s
  in
  List.fold_left fold (R.ok []) nodes

let of_config ~config ~auth ~nodes =
  let lines = String.cuts ~sep:"\n" in
  let fields s = match String.cut ~sep:"=" s with
  | Some kv -> kv
  | None -> s, String.empty
  in
  let map = config |> lines |> List.rev_map fields |> String.Map.of_list in
  let get field f =
    match String.Map.find field map with
    | None -> R.ok None
    | Some v ->
        match f v with
        | None -> R.error_msgf "Cannot parse field %S" field
        | Some _ as v -> R.ok v
  in
  Token.of_string auth >>= fun token ->
  get "ClusterUUID" Uuidm.of_string >>= fun uuid ->
  get "Hostname" hostname >>= fun hostname ->
  get "HttpPort" String.to_int >>= fun port ->
  get "UseSSL" parse_bool >>= fun use_ssl ->
  let ssl = match use_ssl with None -> true | Some v -> v in
  parse_nodes nodes >>= fun nodes ->
  if nodes = [] && hostname = None then
    R.error_msg "No nodes and hostname not configured"
  else
    R.ok {token; uuid; hostname; port; ssl; nodes}
