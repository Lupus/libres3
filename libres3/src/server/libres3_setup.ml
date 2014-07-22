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

let print_version () =
  Printf.printf "libres3 setup version %s\n%!" Version.version;
  exit 0

let sxsetup_conf = ref ""
let open_errmsg = ref false
let s3_host = ref ""
let default_replica = ref ""
let ssl = ref true
let s3_port = ref ""
let batch_mode = ref false

let spec = [
  "--s3-host", Arg.Set_string s3_host,
    " Base hostname to use (equivalent to; s3.amazonaws.com, host_base in .s3cfg)";
  "--s3-port", Arg.Set_string s3_port,
    " Port to use for LibreS3";
  "--default-replica", Arg.Set_string default_replica,
    " Default volume replica count"
  ;
  "--sxsetup-conf", Arg.Set_string sxsetup_conf, " Path to sxsetup.conf";
  "--batch", Arg.Set batch_mode, "  Turn off interactive confirmations and assume safe defaults";
  "--version", Arg.Unit print_version, " Print version";
  "-V", Arg.Unit print_version, " Print version";
  "--no-ssl", Arg.Clear ssl, ""
]

let anon_fail flag =
  raise (Arg.Bad ("invalid option: " ^ flag))

let read_line () =
  if !batch_mode then begin
    prerr_endline "Default value invalid, and running in batch mode!";
    raise End_of_file
  end
  else
    input_line stdin

let read_value msg () =
  flush stderr;
  Printf.printf "\n%s: %!" msg;
  read_line ()

let ask_arg (_, spec, doc) =
  match spec with
  | Arg.Set_string str ->
      if !str = "" then str := read_value doc ()
  | _ -> ()

(* cmdline parsing *)
let () =
  let usage = Printf.sprintf "Usage: %s [options]\n" Sys.argv.(0) in
  Argcompat.parse_align ~extra:(fun () -> ()) spec anon_fail usage;
  if !sxsetup_conf = "" then
    sxsetup_conf := Filename.concat Configure.sysconfdir "sxserver/sxsetup.conf"
  else
    open_errmsg := true
(*  List.iter ask_arg spec*)

let parse_value v =
  try
    Scanf.sscanf v "%S" (fun v -> v)
  with Scanf.Scan_failure _ ->
    v

let load_file name =
  let f = open_in name in
  let size = in_channel_length f in
  let str = String.make size ' ' in
  really_input f str 0 size;
  close_in f;
  str

type line = KV of string * string | Other of string

let load_config ?kind file =
  try
    let f = open_in file in
    let rec loop lst =
      try
        let line = input_line f in
        let entry =
          try
            Scanf.sscanf line "%[^= ] = %s@\n" (fun k v -> KV(k, parse_value v))
          with Scanf.Scan_failure _ | End_of_file ->
            Other line in
        loop (entry :: lst)
      with End_of_file ->
        close_in f;
        begin match kind with
        | Some k ->
          Printf.printf "Successfully loaded %s configuration from '%s'\n%!" k file;
        | None -> ()
        end;
        List.rev lst in
    loop []
  with Sys_error msg ->
    if !open_errmsg then
      Printf.eprintf "Cannot open configuration file: %s\n%!" msg;
    []

let rec find lst key () = match lst with
| [] -> raise Not_found
| KV (k, v) :: _ when k = key ->
    v
| _ :: tl ->
  find tl key ()

let rec replace add lst key value out_list = match lst with
| [] ->
    let out_list = List.rev out_list in
    if add then (KV (key, value)) :: out_list else out_list
| KV (k, _) :: tl when k = key ->
    replace false tl key value (KV (key, value) :: out_list)
| hd :: tl ->
    replace add tl key value (hd :: out_list)

let libres3_conf () =
  Filename.concat Configure.sysconfdir "libres3/libres3.conf"

let rec read_yes_no default msg =
  let choice = match default with
  | true -> "[Y/n]"
  | false -> "[y/N]" in
  Printf.eprintf "%s %s " msg choice;
  flush stdout;
  flush stderr;
  try
    match (String.lowercase (read_line ())) with
    | "y" -> true
    | "n" -> false
    | "" -> default
    | _ -> read_yes_no default msg
  with End_of_file -> false

let open_out_ask name =
  try
    open_out_gen [Open_wronly;Open_creat;Open_excl] 0o600 name
  with Sys_error _ ->
    if Sys.file_exists name then
      Printf.printf "File '%s' already exists, overwriting\n%!" name;
    open_out name


module StringMap = Map.Make(String)

let port_validate portstr =
  let port = int_of_string portstr in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let msg =
    try
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_any, port));
      None
    with Unix.Unix_error(e,_,_) ->
      Some (Printf.sprintf "cannot bind to port: %s"
              (Unix.error_message e));
  in
  Unix.close socket;
  match msg with
  | None -> ()
  | Some str -> failwith str

let validate_one ~key f ?validate m =
  let value = f () in
  try
    let _, validator, _ = List.find (fun (k,_,_) -> k = key) Configfile.entries in
    validator value;
    begin match validate with
    | Some extra_validator -> extra_validator value
    | None -> ()
    end;
    StringMap.add key value m
  with Failure msg as e ->
    Printf.eprintf "Invalid value for '%s=%s': %s\n" key value msg;
    raise e

let rec validate_loop ~key f ?validate m =
  try validate_one ~key f ?validate m
  with Failure _ -> validate_loop ~key f ?validate m

let validate_and_add ~key ?default f ?validate m =
  match default with
  | None -> validate_loop ~key f ?validate m
  | Some default_fn ->
      try validate_one ~key default_fn ?validate m
      with Not_found | Failure _ -> validate_loop ~key f ?validate m

let validate_and_add_opt opt ~key ?default f ?validate m =
  if opt then validate_and_add ~key ?default f ?validate m
  else m

let read_and_validate ~key msg f x ?validate m =
  validate_and_add ~key ~default:(f x) (read_value msg) ?validate m

let read_and_validate_opt opt ~key msg f x ?validate m =
  if opt then read_and_validate ~key msg f x ?validate m
  else m

let update_s3cfg is_https host port key name =
  Printf.printf "Updating '%s'\n" name;
  let lst = load_config name in
  let f = open_out (name ^ ".tmp") in
  (* restrict access to the file because it contains keys *)
  Unix.fchmod (Unix.descr_of_out_channel f) 0o600;
  let hostport = host ^ ":" ^ (string_of_int port) in
  let lst = replace true lst "access_key" "admin" [] in
  let lst = replace true lst "secret_key" key [] in
  let lst = replace true lst "host_base" hostport [] in
  let lst = replace true lst "host_bucket" ("%(bucket)s." ^ hostport) [] in

  let use_https = if is_https then "True" else "False" in
  let lst = replace true lst "use_https" use_https [] in

  let lst = replace false lst "cloudfront_host" host [] in
  let lst = replace false lst "simpledb_host" host [] in
  let scheme = if is_https then "https" else "http" in
  let lst = replace false lst "website_endpoint" (scheme ^ "://" ^ hostport) [] in
  List.iter (function
    | KV (k,v) ->
      Printf.fprintf f "%s = %s\n" k v
    | Other s ->
      Printf.fprintf f "%s\n" s
  ) lst;
  close_out f;
  Sys.rename (name ^ ".tmp") name

let generate_boto secure host port key name =
  Printf.printf "Generating '%s'\n" name;
  let f = open_out (name ^ ".tmp") in
  (* restrict access to the file because it contains keys *)
  Unix.fchmod (Unix.descr_of_out_channel f) 0o600;
  Printf.fprintf f "[Credentials]\ns3_host=%s:%d\n" host port;
  output_string f ("# For newer versions of python-boto you should\n" ^
    "# drop the port from s3_host and use s3_port instead\n");
  Printf.fprintf f "aws_secret_access_key=%s\n" key;
  Printf.fprintf f "aws_access_key_id=%s\n" !Config.key_id;
  Printf.fprintf f "\n[Boto]\nis_secure=%s\n"
    (if secure then "True" else "False");
  close_out f;
  Sys.rename (name ^ ".tmp") name

let ask_start () =
  let init_d_script = "/etc/init.d/libres3" in
  let sbin_script = Filename.concat Configure.sbindir "libres3" in
  let has_init_d_script = Sys.file_exists init_d_script
  and has_sbin_script = Sys.file_exists sbin_script in

  if has_init_d_script || has_sbin_script then
    Printf.printf "\n%!";
    if read_yes_no true "Do you want to start LibreS3 now?" then begin
      let exec = if has_init_d_script then init_d_script else sbin_script in
      let cmd = exec ^ " start" in
      match Unix.system cmd with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          Printf.eprintf "Script exited with code %d\n%!" n;
          exit 1
      | Unix.WSIGNALED n ->
          (* signal number aren't OS signal numbers *)
          Printf.eprintf "Script exited due to signal -%d\n%!" n;
          exit 1
      | Unix.WSTOPPED n ->
          Printf.eprintf "Script stopped due to signal -%d\n%!" n;
          exit 1
  end

let print_opt out key = function
  | Some value ->
      Printf.fprintf out "%s=%S\n" key value
  | None -> ()

let file_exists_opt = function
  | Some file -> Sys.file_exists file
  | None -> false

let run_as_of_user_group user group =
  try
    ignore (Unix.getpwnam user);
    try
      ignore (Unix.getgrnam group);
      user ^ ":" ^ group
    with Not_found ->
      Printf.eprintf "Group not found: %s\n" group;
      failwith "group not found"
  with Not_found ->
    Printf.eprintf "User not found: %s\n" user;
    failwith "user not found"

let maybe_load_file f () =
  let v = f () in
  if String.length v > 1 && v.[0] = '/' then
    let ch = open_in v in
    Paths.with_file ch ~close:close_in (fun ch ->
      input_line ch
    )
  else v

let read_and_validate_admin_key ~key msg f x m =
  validate_and_add ~key ~default:(maybe_load_file (f x))
    (maybe_load_file (read_value msg)) m

let generate_ssl_certificate m =
  if !ssl then begin
    let s3_host = StringMap.find "s3_host" m in
    let cmd = Printf.sprintf "%s/libres3_certgen '%s'" Configure.sbindir s3_host in
    if Sys.command cmd <> 0 then
      prerr_endline "Cannot generate SSL certificate";
  end;
  m

let (|>) x f = f x

let () =
  try
    let config = load_config ~kind:"SX" !sxsetup_conf in
    let load = find config in
    let sx_use_ssl =
      try
        let v = load "SX_USE_SSL" () = "yes" in
        if v <> !ssl then
          Printf.eprintf "Warning: SX and LibreS3 SSL mode mismatch!\n";
        v
      with _ -> true in
    let sx_server_port_msg =
      if sx_use_ssl then "SX server HTTPS port"
      else "SX server HTTP port" in
    let generated =
      begin StringMap.empty
      |> read_and_validate_admin_key ~key:"secret_key" "Admin key or path to key-file" load "SX_ADMIN_KEY"
      |> read_and_validate ~key:"sx_host" "SX server IP/DNS name" load "SX_NODE_IP"
      |> read_and_validate ~key:"sx_port" sx_server_port_msg load "SX_PORT"
      |> validate_and_add ~key:"pidfile" ~default:(fun () ->
          let rundir = Filename.concat Configure.localstatedir "run" in
          Filename.concat rundir "libres3.pid") (read_value "PID file path")
      |> validate_and_add ~key:"run-as" ~default:(fun () ->
          run_as_of_user_group
            (load "SX_SERVER_USER" ()) (load "SX_SERVER_GROUP" ())) (fun () ->
          let u = read_value "Run as user" ()
          and g = read_value "Run as group" () in
          run_as_of_user_group u g)
      |> validate_and_add ~key:"volume_size" ~default:(fun () -> "10G") (fun () -> invalid_arg "built-in value")
      |> validate_and_add ~key:"s3_host" ~default:(fun () ->
          if !s3_host <> "" then !s3_host
          else load "LIBRES3_HOST" ()) (read_value "S3 (DNS) name")
      |> generate_ssl_certificate
      |> validate_and_add_opt !ssl ~key:"s3_ssl_privatekey_file" ~default:(fun () ->
          Filename.concat Configure.sysconfdir "ssl/private/libres3.key")
          (read_value "SSL private key file")
      |> validate_and_add_opt !ssl ~key:"s3_ssl_certificate_file" ~default:(fun () ->
          Filename.concat Configure.sysconfdir "ssl/certs/libres3.pem")
          (read_value "SSL certificate file")
      |> (
        let key = if !ssl then "s3_ssl_port" else "s3_port" in
        let port_msg = if !ssl then "S3 SSL port" else "S3 port" in
        validate_and_add ~key ~default:(fun () ->
          if !s3_port <> "" then !s3_port
          else load "LIBRES3_PORT" ()) (read_value port_msg)
              ~validate:port_validate
        )
      |> validate_and_add ~key:"replica_count" ~default:(fun () ->
          if !default_replica <> "" then !default_replica
          else load "LIBRES3_REPLICA" ())
          (read_value "Default volume replica count")
      |> validate_and_add ~key:"allow_volume_create_any_user"
        ~default:(fun () -> "true") (fun () -> invalid_arg "built-in value")
      end
    in
    let name = libres3_conf () in
    Printf.printf "\nGenerating '%s'\n" name;
    let outfile = open_out_ask name in
    (* restrict access to the file because it contains keys *)
    Unix.fchmod (Unix.descr_of_out_channel outfile) 0o600;
    Printf.fprintf outfile "# LibreS3 configuration file\n";

    let active, inactive =
      List.partition (fun (key,_,_) -> StringMap.mem key generated) Configfile.entries in
    List.iter (fun (key,_,doc) ->
      let value = StringMap.find key generated in
      Printf.fprintf outfile "\n#%s\n%s=%S\n" doc key value
    ) active;

    List.iter (fun (key,_,doc) ->
      if doc <> "" then
        Printf.fprintf outfile "\n#%s\n#%s=\n" doc key) inactive;

    close_out outfile;
    let s3_host = StringMap.find "s3_host" generated in
    let admin_key = StringMap.find "secret_key" generated in
    if !ssl then begin
      let portstr = StringMap.find "s3_ssl_port" generated in
      let port = int_of_string portstr in
      update_s3cfg true s3_host port admin_key (Filename.concat Configure.sysconfdir "libres3/libres3.sample.s3cfg");
      generate_boto true s3_host port admin_key (Filename.concat Configure.sysconfdir "libres3/libres3.sample.boto")
    end else begin
      let s3_port = StringMap.find "s3_port" generated in
      update_s3cfg false s3_host (int_of_string s3_port) admin_key (Filename.concat Configure.sysconfdir "libres3/libres3-insecure.sample.s3cfg");
      generate_boto false s3_host (int_of_string s3_port) admin_key (Filename.concat Configure.sysconfdir "libres3/libres3.sample.boto")
    end;
    if not !batch_mode then
      ask_start ();
  with
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
  | End_of_file ->
    if not !batch_mode then
      prerr_endline "\nEOF encountered before reading all the answers!\n"
    else
      prerr_endline "\nError(s) encountered while generating libres3.conf";
    exit 1

