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

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let parse_value v =
  try
    Scanf.sscanf v "%S" (fun v -> v)
  with Scanf.Scan_failure _ | End_of_file ->
    v

let parse spec line =
  if String.length line = 0 then None
  else if line.[0] = '#' then None
  else
    try Some (Scanf.sscanf line "%[^= ] = %s@\n" (fun k v ->
      let k = String.lowercase k in
      if not (StringSet.mem k spec) then
        raise (Failure ("Unknown configuration key: " ^ k));
      k, parse_value v
    ))
    with End_of_file -> None;;

let parse_configuration_line spec ch accum =
  match parse spec (input_line ch) with
  | Some (key, v) -> StringMap.add key v accum
  | None -> accum;;

let print_config key v =
  if key <> "secret_key" then
    Printf.printf "\t%s=%s\n" key v;;

let load_configuration spec =
  let keys = List.fold_left (fun accum (key, _, _) ->
    StringSet.add (String.lowercase key) accum) StringSet.empty spec in
  let config = Paths.process_configuration ~parse:(parse_configuration_line keys)
    in
  List.iter (fun (key, fn, _) ->
    try
      fn (StringMap.find key config)
    with
    | Not_found -> ()
    | Failure msg ->
        raise (Failure (Printf.sprintf "Failed to parse configuration key '%s': %s"
          key msg))
  ) spec;
  config;;

let print_version () =
  Printf.printf "libres3 version %s\n%!" Version.version;
  exit 0
;;

let noop () = ()

let print_conf show () =
  if show then
    let confspec = List.append (List.map (fun (key, _, desc) ->
      (key, Arg.Unit noop, desc)) Configfile.entries)
    ["-help",Arg.Unit noop, "";
     "--help",Arg.Unit noop, ""] in
    Argcompat.usage_hide confspec "\nlibres3.conf entries:"

let parse_cmdline ?(print_conf_help=true) additional_args =
  let usage = (Printf.sprintf "Usage: %s [options]\n" Sys.argv.(0)) in
  let spec = [
    "--config-file", Arg.Set_string Paths.config_file,
      " Path to configuration file (default: " ^ !Paths.config_file ^ ")";
    "--version", Arg.Unit print_version, " Print version";
    "-V", Arg.Unit print_version, " Print version";
  ] @ additional_args in
  Argcompat.parse_align ~extra:(print_conf print_conf_help) spec (fun anon ->
    raise (Arg.Bad ("invalid option " ^ anon))
  ) usage

let validate_configuration config =
  if !Configfile.pidfile = "" then begin
    Printf.eprintf "pidfile is not set!\n";
    raise Exit
  end;
  Printf.printf "Configuration:\n";
  StringMap.iter print_config config;
  try
    if !Configfile.base_hostname = "" then
      raise (Arg.Bad "s3_host must be set");
    let mode_sx = !Configfile.sx_host <> None
    and mode_fs = !Configfile.buckets_dir <> "/" in
    if mode_sx && mode_fs then
      raise (Arg.Bad "You cannot use both SX and FS mode");
    if not mode_sx && not mode_fs then
      raise (Arg.Bad "SX host is not set");
    if !Configfile.ssl_certificate_file = Some "" then
      Configfile.ssl_certificate_file := None;
    if !Configfile.ssl_privatekey_file = Some "" then
      Configfile.ssl_privatekey_file := None;
    begin match !Configfile.ssl_certificate_file, !Configfile.ssl_privatekey_file with
    | Some _, Some _ -> ()
    | None, None -> ()
    | _ -> raise (Arg.Bad "You must specifiy both s3_ssl_certificate_file and \
    s3_ssl_privatekey_file")
    end;
    if Unix.geteuid () = 0 then begin
      if !Configfile.user = None then
        raise (Arg.Bad "When running as root you must set user");
      if Unix.getegid () = 0 && !Configfile.group = None then
        raise (Arg.Bad "When running as root you must set group");
    end else if Unix.getgid() = 0 then
      raise (Arg.Bad "Running as root group, but not as root user: cannot change groups")
    else begin
      begin match !Configfile.user with
      | Some u ->
          Printf.eprintf "Ignoring user %s directive: not running as root\n" u;
      | None -> ()
      end;
      begin match !Configfile.group with
      | Some g ->
          Printf.eprintf "Ignoring group %s directive: not running as root\n" g;
      | None -> ();
      end;
      Configfile.user := None;
      Configfile.group := None;
    end;
    Printf.printf "Starting an HTTP S3 server on %s:%d\n"
      !Configfile.base_hostname !Configfile.base_port;
    if !Configfile.ssl_privatekey_file <> None then
      Printf.printf "Starting an HTTPS S3 server on %s:%d\n"
        !Configfile.base_hostname !Configfile.base_ssl_port;
    match !Configfile.sx_host with
    | Some sx ->
      Printf.printf "Connecting to SX backend at %s\n"
        sx
    | None ->
      Printf.printf "Using the FS backend at %s\n"
        !Configfile.buckets_dir
  with Arg.Bad msg ->
    Printf.eprintf "Command-line error: %s!\n" msg;
    raise Exit

