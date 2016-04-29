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
open SXIO
let print_version () =
  Printf.printf "libres3 setup version %s\n%!" Version.version;
  exit 0

let (|>) x f = f x

let sxsetup_conf = ref ""
let open_errmsg = ref false
let s3_host = ref ""
let default_replica = ref ""
let default_volume_size = ref ""
let ssl = ref true
let s3_http_port = ref ""
let s3_https_port = ref ""
let batch_mode = ref false

let is_space c = c = ' ' || (c >= '\t' && c <= '\r') 

let rec trim_eol s pos e =
  if pos == e || e == 0 then ""
  else if is_space s.[e-1] then
    trim_eol s pos (e-1)
  else
    String.sub s pos (e-pos)

let rec trim_bol s pos =
  if pos >= String.length s then ""
  else if is_space s.[pos] then
    trim_bol s (pos+1)
  else
    trim_eol s pos (String.length s)

let trim s =
  trim_bol s 0

let update = ref []

let sx_uri = ref None
let anon_fail uri =
  let open Neturl in
  try
    sx_uri := Some (Neturl.url_of_string {
        Neturl.null_url_syntax with
        url_enable_user = Url_part_allowed;
        url_enable_scheme = Url_part_required;
        url_enable_host = Url_part_required;
        url_accepts_8bits = false;
        url_is_valid = (fun url ->
            Neturl.url_scheme url = "sx");
        url_enable_relative = false;
      } uri)
  with Neturl.Malformed_URL ->
    raise (Arg.Bad ("invalid SX URL: " ^ uri))

let configdir = ref None
let ssl_key_file = ref (Filename.concat Configure.sysconfdir "ssl/private/libres3.key")
let ssl_cert_file = ref (Filename.concat Configure.sysconfdir "ssl/certs/libres3.pem")

let spec = [
  "--update", Arg.String (fun s -> update := s :: !update), "Update configuration in cluster metadata (KEY=VALUE)";
  "--s3-host", Arg.Set_string s3_host,
    " Base hostname to use (equivalent to; s3.amazonaws.com, host_base in .s3cfg)";
  "--s3-http-port", Arg.Set_string s3_http_port,
    " HTTP port to use for LibreS3";
  "--s3-https-port", Arg.Set_string s3_https_port,
    " HTTPS port to use for LibreS3";
  "--default-replica", Arg.Set_string default_replica,
    " Default volume replica count"
  ;
  "--default-volume-size", Arg.Set_string default_volume_size,
    " Default volume size"
  ;
  "--ssl-certificate-file", Arg.Set_string ssl_cert_file,
  " Path for SSL certificate file";
  "--ssl-key-file", Arg.Set_string ssl_key_file,
  " Path for SSL key file";
  "--sxsetup-conf", Arg.Set_string sxsetup_conf, " Path to sxsetup.conf";
  "--batch", Arg.Set batch_mode, " Turn off interactive confirmations and assume safe defaults";
  "--config-dir", Arg.String (fun s -> configdir := Some s),
   " Path to SX configuration directory";
  "--debug", Arg.Unit (fun () -> Lwt_log.Section.set_level EventLog.section Lwt_log.Debug), "enable debug messages";
  "--version", Arg.Unit print_version, " ";
  "-V", Arg.Unit print_version, " Print version";
  "--no-ssl", Arg.Clear ssl, ""
]

let load_initial_configuration () =
  match !sx_uri with
  | None -> Lwt.return_none
  | Some uri ->
    let host = Neturl.url_host uri in
    let user = try Some (Neturl.url_user ~encoded:false uri) with Not_found -> None in
    Sxload.load ?configdir:!configdir ?user ~host >>= fun sx ->
    begin match sx.Sxload.port with
      | None -> ()
      | Some p ->
        Config.sx_port := p
    end;
    Config.sx_ssl := sx.Sxload.use_ssl;
    Configfile.sx_host := Some host;
    Config.secret_access_key := sx.Sxload.token;
    SXC.last_nodes := List.rev_map Ipaddr.to_string sx.Sxload.nodes;
    Lwt.return (Some (Neturl.make_url SXC.syntax ~scheme:"sx" ~user:sx.Sxload.user
      ~host:(List.hd !SXC.last_nodes) ?port:sx.Sxload.port ~path:[""]))

let lwt_run f =
  Lwt_unix.on_signal Sys.sigint (fun _ -> raise Sys.Break) |> ignore;
  try
    Lwt_main.run (Lwt.finalize f Lwt_io.flush_all)
  with
  | Failure msg ->
    Printf.eprintf "Error:  %s\n%!" msg;
    exit 1
  |  e ->
    Printf.eprintf "Error: %s\n%!" (Printexc.to_string e);
    exit 2

let print_configuration settings =
  Lwt_list.iter_s (fun (k,v) ->
      Lwt_io.printlf "\t%s = %s" k v
    ) settings

let update_config base lst remove =
  let updates = List.rev_map (fun str ->
      let key, v =
        try match Sxload.split_kv str with
          | Some r -> r
          | None -> failwith "Key cannot start with #"
        with Failure msg as e ->
          output_string stderr msg;
          raise e
      in
      try
        let _, validator, _ = List.find (fun (k,_,_) -> k = key) Configfile.meta_entries in
        validator v;
        key, v
      with
      | Failure msg ->
        failwith (Printf.sprintf "Invalid value for '%s=%s': %s" key v msg)
      | Not_found ->
        failwith (Printf.sprintf "Unknown meta configuration key '%s'\n" key)
    ) lst in
  let updates = List.rev_append (List.rev_map (fun (k,_) -> (k,"")) remove) updates in
  SXC.update_settings ~max_wait:10. base updates >>= fun settings ->
  Lwt_io.printl "\nNew configuration:" >>= fun () ->
  print_configuration settings

let read_line () =
  if !batch_mode then begin
    prerr_endline "Default value invalid, and running in batch mode!";
    raise End_of_file
  end
  else
    lwt_run (fun () -> Lwt_io.read_line Lwt_io.stdin) |> trim

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

let is_meta_entry key =
  List.find_all (fun (k,_,_) -> k = key) Configfile.meta_entries <> []

let () =
  Printexc.register_printer (function
      | Http_client.Http_protocol e ->
        Some ("HTTP(S) error: " ^ (Printexc.to_string e))
      | SXDefaultIO.Detail (e, l) ->
        Some (Printf.sprintf "SX error: %s\n%s" (Printexc.to_string e)
                (String.concat "\n" (List.rev_map (fun (k,v) -> k ^"="^v) l))
             )
      | Unix.Unix_error(e,fn,arg) ->
        Some (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message e))
      | _ -> None
    );
  if lwt_run (fun () ->
      load_initial_configuration () >>= function
      | None -> Lwt.return_false
      | Some base ->
        Lwt_unix.with_timeout 10. (fun () -> SXC.get_settings base) >>= fun settings ->
        let remove = List.find_all (fun (key,_) -> not (is_meta_entry key)) settings in
        Lwt_io.printl "Previous configuration:" >>= fun () ->
        print_configuration settings >>= fun () ->
        Lwt_io.printl "" >>= fun () ->
        begin if !update <> [] then
          update_config base !update remove
        else
          Lwt.return_unit
        end >>= fun () ->
        Lwt.return_true
    ) then exit 0


let parse_value v =
  try
    Scanf.sscanf v "%S" (fun v -> trim v)
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

let validate_one ?validate ~key f m =
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

let rec validate_loop ?validate ~key f m =
  try validate_one ?validate ~key f m
  with Failure _ -> validate_loop ?validate ~key f m

let validate_and_add ?validate ~key ?default f m =
  match default with
  | None -> validate_loop ?validate ~key f m
  | Some default_fn ->
      try validate_one ?validate ~key default_fn m
      with Not_found | Failure _ -> validate_loop ?validate ~key f m

let validate_and_add_opt opt ~key ?default f m  =
  if opt then validate_and_add ~key ?default f m
  else m

let read_and_validate ~key msg f x m =
  validate_and_add ~key ~default:(f x) (read_value msg) m

let read_and_validate_opt opt ~key msg f x m =
  if opt then read_and_validate ~key msg f x m
  else m

let update_s3cfg cert_file host port key name =
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

  let use_https = if cert_file = None then "False" else "True" in
  let lst = replace true lst "use_https" use_https [] in

  let lst = replace false lst "cloudfront_host" host [] in
  let lst = replace false lst "simpledb_host" host [] in
  let scheme = if cert_file = None then "http" else "https" in
  let lst = replace false lst "website_endpoint" (scheme ^ "://" ^ hostport) [] in
  let lst = match cert_file with
    | Some cert -> replace true lst "ca_certs_file" cert []
    | None -> lst in
  List.iter (function
    | KV (k,v) ->
      Printf.fprintf f "%s = %s\n" k v
    | Other s ->
      Printf.fprintf f "%s\n" s
  ) lst;
  close_out f;
  Sys.rename (name ^ ".tmp") name

let generate_boto cert_file host port key name =
  Printf.printf "Generating '%s'\n" name;
  let f = open_out (name ^ ".tmp") in
  (* restrict access to the file because it contains keys *)
  Unix.fchmod (Unix.descr_of_out_channel f) 0o600;
  Printf.fprintf f "[Credentials]\ns3_host=%s\n" host;
  Printf.fprintf f "s3_port=%d\n" port;
  output_string f ("# For older versions of python-boto you should\n" ^
    "# put the port on s3_host instead\n");
  Printf.fprintf f "aws_secret_access_key=%s\n" key;
  Printf.fprintf f "aws_access_key_id=%s\n" !Config.key_id;
  Printf.fprintf f "\n[Boto]\nis_secure=%s\n"
    (if cert_file <> None then "True" else "False");
  begin match cert_file with
  | None -> ()
  | Some file ->
    (* Python 2.7.9 enabled SSL certificate verification, which causes it to fail with python-boto by default on self-signed certificates.
       Explicitly enabling certificate verification will use python-boto's certificate validation code with the custom CA file and succeed.
    *)
    Printf.fprintf f "ca_certificates_file=%s\nhttps_validate_certificates=True\n" file
  end;
  close_out f;
  Sys.rename (name ^ ".tmp") name

let ask_start () =
  let init_d_script = "/etc/init.d/libres3" in
  let sbin_script = Filename.concat Configure.sbindir "libres3" in
  let use_init_d_script = Unix.geteuid () = 0 && (Sys.file_exists init_d_script)
  and has_sbin_script = Sys.file_exists sbin_script in

  if use_init_d_script || has_sbin_script then
    Printf.printf "\n%!";
    if read_yes_no true "Do you want to start LibreS3 now?" then begin
      let exec = if use_init_d_script then init_d_script else sbin_script in
      let cmd = exec ^ " restart" in
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

let check_user user =
  try
    ignore (Unix.getpwnam user);
    user
  with Not_found ->
    Printf.eprintf "User not found: %s\n" user;
    failwith "user not found"

let check_group group =
  try
    ignore (Unix.getgrnam group);
    group
  with Not_found ->
    Printf.eprintf "Group not found: %s\n" group;
    failwith "group not found"

let run_as_of_user_group user group =
      user ^ ":" ^ group

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

let (|>) x f = f x

open Lwt
open Cryptokit

let split s =
  try
    let p = String.index s '\x00' in
    String.sub s 0 p, String.sub s (p+1) (String.length s - p -1)
  with Not_found ->
    failwith "Invalid libres3_private settings format"

let join a b = a ^ "\x00" ^ b

let save_to ~file data =
  Lwt_io.with_file ~mode:Lwt_io.output file (fun ch ->
      Lwt_io.write ch data >>= fun () ->
      Lwt_io.flush ch
  )

let load_from ~file =
  Lwt_io.with_file ~mode:Lwt_io.input file (fun ch -> Lwt_io.read ch)

let generate_ssl_certificate ~ssl_key_file ~ssl_cert_file m =
  if !ssl then begin
    let s3_host = StringMap.find "s3_host" m in
    let host = StringMap.find "sx_host" m in
    let url = Neturl.make_url ~encoded:false
        ~scheme:"sx" ~user:!Config.key_id ~port:!Config.sx_port ~host ~path:[""] SXC.syntax in
    Printexc.register_printer (function
        | Http_client.Http_protocol e ->
          Some ("HTTP(S) error: " ^ (Printexc.to_string e))
        | SXDefaultIO.Detail (e, l) ->
          Some (Printf.sprintf "SX error: %s\n%s" (Printexc.to_string e)
                  (String.concat "\n" (List.rev_map (fun (k,v) -> k ^"="^v) l))
               )
        | Unix.Unix_error(e,fn,arg) ->
          Some (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message e))
        | _ -> None
      );
    Sys.catch_break true;
    Printf.printf "Locking LibreS3 private settings on SX cluster: %s\n%!" (Neturl.string_of_url url);
    Lwt_main.run (Lwt.catch (fun () ->
        SXIO.with_settings ~max_wait:60. (SXIO.of_neturl url) (function
            | "" ->
              let cmd = Printf.sprintf "%s/libres3_certgen '%s'" Configure.sbindir s3_host in
              (* TODO: Lwt *)
              if Sys.command cmd <> 0 then
                prerr_endline "Cannot generate SSL certificate";
              load_from ~file:ssl_cert_file >>= fun cert ->
              load_from ~file:ssl_key_file >>= fun key ->
              let v = join cert key in
              Printf.printf "Uploaded generated SSL certificate and key to SX cluster\n%!";
              Lwt.return (Some (transform_string (Hexa.encode ()) v))
            | old ->
              let old = transform_string (Hexa.decode()) old in
              let cert, key = split old in
              save_to ssl_cert_file cert >>= fun () ->
              save_to ssl_key_file key >>= fun () ->
              Printf.printf "Downloaded SSL certificate and key from the cluster to %s and %s\n%!" ssl_key_file ssl_cert_file;
              Lwt.return_none
          ) "libres3_private"
      ) (function
        | e ->
          Printf.eprintf "\nError updating settings: %s\n" (Printexc.to_string e);
          Lwt.fail e) >|= fun () ->
    Printf.printf "Settings completed\n%!");
  end;
  m

let () =
  try
    let config = load_config ~kind:"SX" !sxsetup_conf in
    let load = find config in
    let sx_use_ssl =
      try
        let v = load "SX_USE_SSL" () = "yes" in
        if v <> !ssl then
          let is_secure = function true -> "secure" | false -> "insecure" in
          Printf.eprintf
            "ERROR: SX (%s) and LibreS3 (%s) SSL mode mismatch!\n"
            (is_secure v) (is_secure !ssl);
          if v then
            prerr_endline "Run libres3_setup without --no-ssl to enable SSL support"
          else
            prerr_endline "Run libres3_setup with --no-ssl to disable SSL support (NOT RECOMMENDED!)";
          exit 3
        else v
      with _ -> true in
    Config.sx_ssl := sx_use_ssl;
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
          Filename.concat rundir "libres3/libres3.pid") (read_value "PID file path")
      |> validate_and_add ~key:"run-as" ~default:(fun () ->
          run_as_of_user_group
            (check_user (load "SX_SERVER_USER" ()))
            (check_group (load "SX_SERVER_GROUP" ()))) (fun () ->
          let u = check_user (read_value "Run as user" ())
          and g = check_group (read_value "Run as group" ()) in
          run_as_of_user_group u g)
      |> validate_and_add ~key:"s3_host" ~default:(fun () ->
          if !s3_host <> "" then !s3_host
          else load "LIBRES3_HOST" ()) (read_value "S3 (DNS) name")
      |> generate_ssl_certificate ~ssl_key_file:!ssl_key_file ~ssl_cert_file:!ssl_cert_file
      |> validate_and_add_opt !ssl ~key:"s3_ssl_privatekey_file" ~default:(fun () ->
          !ssl_key_file)
          (read_value "SSL private key file")
      |> validate_and_add_opt !ssl ~key:"s3_ssl_certificate_file" ~default:(fun () ->
          !ssl_cert_file)
          (read_value "SSL certificate file")
      |> (if !ssl then
          validate_and_add ~key:"s3_https_port" ~validate:port_validate ~default:(fun () ->
              if !s3_https_port <> "" then !s3_https_port
              else load "LIBRES3_HTTPS_PORT" ()) (read_value "S3 HTTPS port")
          else fun m -> m)
      |> validate_and_add ~key:"s3_http_port" ~validate:port_validate ~default:(fun () ->
              if !s3_http_port <> "" then !s3_http_port
              else load "LIBRES3_HTTP_PORT" ()) (read_value "S3 HTTP port")
      |> validate_and_add ~key:"volume_size" ~default:(fun () ->
          if !default_volume_size <> "" then !default_volume_size
          else load "LIBRES3_VOLUMESIZE" ())
        (read_value "Default volume size [use K, M, G and T suffixes]")
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
      let portstr = StringMap.find "s3_https_port" generated in
      let port = int_of_string portstr in
      let cert_file = StringMap.find "s3_ssl_certificate_file" generated in
      update_s3cfg (Some cert_file) s3_host port admin_key (Filename.concat Configure.sysconfdir "libres3/libres3.sample.s3cfg");
      generate_boto (Some cert_file) s3_host port admin_key (Filename.concat Configure.sysconfdir "libres3/libres3.sample.boto")
    end;
    let s3_port = int_of_string (StringMap.find "s3_http_port" generated) in
    update_s3cfg None s3_host s3_port admin_key (Filename.concat Configure.sysconfdir "libres3/libres3-insecure.sample.s3cfg");
    generate_boto None s3_host s3_port admin_key (Filename.concat Configure.sysconfdir "libres3/libres3-insecure.sample.boto");
    flush_all ();

    begin match !Configfile.sx_host with
    | None -> ()
    | Some host ->
      let base =
        Neturl.make_url SXC.syntax ~scheme:"sx" ~user:!Config.key_id
          ~host ~port:!Config.sx_port ~path:[""] in
      lwt_run (fun () ->
          let generated = StringMap.filter (fun k _ -> is_meta_entry k) generated in
          SXC.update_settings ~max_wait:10. base (StringMap.bindings generated) >>= fun settings ->
          print_configuration settings)
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

