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

let anonymize = ref false

let filter out str =
  let str = if !anonymize then Anonymize.filter str else str in
  output_string out str

let fprintf out fmt = Printf.kprintf (filter out) fmt
let eprintf = Printf.printf

let print_wrap out name f =
  fprintf out "--- %s: ---\n" name;
  f ();
  fprintf out "---=---\n"

let dump_channel out ch =
  try
    while true; do
      let line = input_line ch in
      fprintf out "\t%s\n" line;
    done
  with End_of_file -> ()

let dump_file out name =
  print_wrap out name (fun () ->
      let buf = String.make Config.buffer_size ' ' in
      try
        let f = open_in name in
        dump_channel out f;
        close_in f;
      with Sys_error e ->
        fprintf out "\t%s\n" e
  )

let print_section out name =
  fprintf out "\n%s" name;
  fprintf out "\n%s\n" (String.make (String.length name) '-')

let dump_command out cmd =
  print_wrap out cmd (fun () ->
    try
      let f = Unix.open_process_in cmd in
      dump_channel out f;
      match Unix.close_process_in f with
      | Unix.WEXITED 0 -> ();
      | Unix.WEXITED n ->
        fprintf out "Error executing '%s': exited with code %d\n" cmd n
      | Unix.WSIGNALED n | Unix.WSTOPPED n ->
        fprintf out "Error executing '%s': exited due to signal %d\n" cmd n
    with Unix.Unix_error (err,fn,_) ->
      fprintf out "Error executing '%s' in %s: %s\n" cmd fn (Unix.error_message err)
  )

type 'a result = OK of 'a | Error of exn

let print_str_opt () = function
  | Some s -> Printf.sprintf "%s" s
  | None -> Printf.sprintf "N/A"

let print_str_list lst () =
  (String.concat ":" (List.map Filename.quote lst))

let secret_re = Netstring_str.regexp ".*secret_key.*"
let rec dump_cfg out ch =
  let line = input_line ch in
  begin match Netstring_str.string_match secret_re line 0 with
  | None ->
    fprintf out "%s\n" line
  | Some _ ->
    fprintf out "<secret_key not printed>\n"
  end;
  dump_cfg out ch

let dump_cfg_file out path =
  try
    let ch = open_in path in
    print_wrap out path (fun () ->
      try
        dump_cfg out ch;
      with End_of_file ->
        close_in ch
    )
  with _ -> ()

open SXLwt
open IO.Op

let check_host host =
  let url = Neturl.make_url ~encoded:false
      ~scheme:"sx" ~user:!Config.key_id ~port:!Config.sx_port ~host ~path:[""] SXC.syntax in
  let urlstr = Neturl.string_of_url url in
  try_catch (fun () ->
      SXIO.check (SXIO.of_neturl url) >>= function
      | Some uuid ->
        return (Some (urlstr, None))
      | None ->
        return (Some (urlstr, Some "no uuid"))
  ) (fun e -> return (Some (urlstr, Some (Printexc.to_string e)))) ()

let resolve_host out ~kind host =
  eprintf "Looking up %s host '%s' ... %!" kind host;
  fprintf out "%s host '%s' " kind host;
  let results = try
    match Unix.getaddrinfo host "" [Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_CANONNAME] with
    | [] ->
        fprintf out "cannot be resolved\n"; []
    | result ->
      fprintf out "resolves to:\n";
      List.rev_map (fun ai ->
          match ai.Unix.ai_addr with
          | Unix.ADDR_INET (inet, _) ->
            let addr = Unix.string_of_inet_addr inet in
            fprintf out "\t%s (%s)\n" addr ai.Unix.ai_canonname;
            Some addr
          | _ -> None
        ) result
    with e ->
      fprintf out "failed to resolve: %s\n" (Printexc.to_string e); []
  in
  eprintf "done\n%!";
  results

let check_sx_hosts out sxhost =
  let addrs = resolve_host out "SX" sxhost in
  Default.register ();
  eprintf "Checking connection ... %!";
  (* check all hosts in parallel *)
  let check_results = List.rev_map (function
      | Some host -> check_host host
      | None -> return None) addrs in
  (* wait for all results *)
  let results = Lwt_main.run (List.fold_left (fun accum res ->
      accum >>= fun a -> res >>= function
    | Some r -> return (r :: a)
    | None -> return a) (return []) check_results) in
  List.iter (fun (host, result) ->
      fprintf out "\t %s: %s\n" host (match result with
          | None -> "OK"
          | Some err -> err)
  ) results;
  eprintf "done\n%!"

let check_s3_hosts out s3host =
  ignore (resolve_host out "S3" s3host)

let run out result =
  print_section out "Build configuration";
  fprintf out "Source code version: %s\n" Version.version;
  fprintf out "sysconfdir: %s\n" Configure.sysconfdir;
  fprintf out "localstatedir: %s\n" Configure.localstatedir;
  fprintf out "sbindir: %s\n" Configure.sbindir;
  print_wrap out "Package versions" (fun () ->
      output_string out Version.package_info
  );
  print_wrap out "Build environment" (fun () ->
    output_string out Version.env_info
  );
    fprintf out "OCaml compiler version: %s\n" Sys.ocaml_version;

  (* TODO: bindir, sysconfdir, localstatedir *)
  print_section out "System information";
  fprintf out "OS type: %s\n" Sys.os_type;
  fprintf out "Word size: %d\n" Sys.word_size;
  fprintf out "Executable: %s\n" Sys.argv.(0);
  dump_command out "uname -mrsv";
  dump_command out "lsb_release -ds 2>/dev/null";
  dump_file out "/proc/cpuinfo";
  dump_command out "/sbin/ifconfig 2>/dev/null";
  dump_command out "free 2>/dev/null";

  print_section out "Runtime configuration";
  fprintf out "Buckets dir: %s\n" !Configfile.buckets_dir;
  fprintf out "Syslog_facility: %a\n" print_str_opt !Configfile.syslog_facility;
  fprintf out "Run as: %a:%a\n"
    print_str_opt !Configfile.user print_str_opt !Configfile.group;
  fprintf out "Pid file: %s\n" !Configfile.pidfile;
  fprintf out "SSL certificate file: %a\n" print_str_opt !Configfile.ssl_certificate_file;
  fprintf out "SSL private key file: %a\n" print_str_opt !Configfile.ssl_privatekey_file;
  fprintf out "Base host: %s, port: %d, SSL port: %d\n"
    !Configfile.base_hostname !Configfile.base_port !Configfile.base_ssl_port;
  fprintf out "Access key id: %s\n" !Config.key_id;
  fprintf out "Secret access key present: %d bytes\n"
    (String.length !Config.secret_access_key);
  fprintf out "SX host: %a\n" print_str_opt !Configfile.sx_host;
  fprintf out "Max connected: %d\n" !Configfile.max_connected;
  fprintf out "Daemonize: %b\n" !Configfile.daemonize;
  fprintf out "Verbose: %b\n" !Configfile.verbose;
  fprintf out "Initialization: ";
  begin match result with
  | OK _ -> fprintf out "OK\n";
  | Error e -> fprintf out "Error: %s\n" (Printexc.to_string e)
  end;
  dump_cfg_file out (!Paths.config_file);
  dump_file out (Paths.generated_config_file);
  let dir = !Paths.log_dir in
  dump_file out (Filename.concat dir "errors.log");
  dump_file out (Filename.concat dir "warnings.log");
  dump_file out (Filename.concat dir "info.log");
  check_s3_hosts out !Configfile.base_hostname;
  check_s3_hosts out ("test." ^ !Configfile.base_hostname);
  match !Configfile.sx_host with
  | Some host ->
      check_sx_hosts out host
  | None -> ()
;;

let () =
  let now = int_of_float (Unix.gettimeofday ()) in
  let output_file = ref (Printf.sprintf "libres3-report-%d.log" now) in
  Cmdline.parse_cmdline ~print_conf_help:false [
    "--output", Arg.Set_string output_file,
      Printf.sprintf " Save output to given file (default: %s)" !output_file;
    "--anonymize", Arg.Set anonymize, " Anonymize IP addresses, URLs, and hostnames";
    "--no-ssl", Arg.Clear Config.sx_ssl, "";
  ];
  let result =
    try OK (Cmdline.load_and_validate_configuration ())
    with e -> Error e in
  let output =
    if !output_file = "-" then stdout
    else open_out_bin !output_file in

  run output result;

  if output <> stdout then begin
    eprintf "Report stored in %s\nYou can attach it to a bugreport at https://bugzilla.skylable.com\n%!" !output_file;
    close_out output
  end


