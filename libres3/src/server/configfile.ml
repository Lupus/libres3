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

open Netstring_str

let expect f ref s =
  ref := f s

let expect_opt f ref s =
  ref := Some (f s)

let validate_string ~desc ~expect regex s =
  if string_match regex s 0 = None then
    failwith ("Invalid " ^ desc ^ " (expected " ^ expect ^ "): " ^ s);
  s

let parse_int s =
  try Int64.of_string s
  with Failure _ ->
    failwith ("Invalid number: " ^ s)

let parse_bool s = match String.lowercase s with
  | "true" | "yes" | "on" -> true
  | "false" | "no" | "off" -> false
  | _ -> failwith ("expected boolean value, but got " ^ s)

let parse_size s =
  try
    Scanf.sscanf s "%g%c" (fun size suffix ->
        (* TODO: these suffixes are wrong, but they match what sx uses *)
        let size = match suffix with
          | 'T' | 't' -> size *. (2. ** 40.)
          | 'G' | 'g' -> size *. (2. ** 30.)
          | 'M' | 'm' -> size *. (2. ** 20.)
          | 'K' | 'k' -> size *. (2. ** 10.)
          | _ -> failwith ("Unknown suffix in " ^ s) in
        size)
  with _ ->
    failwith ("expected size with suffix: " ^ s)

let validate_filename path =
  if String.length path = 0 then
    failwith "Empty path"
  else if path.[0] <> '/' then
    failwith ("Expected absolute path, got: " ^ path)
  else if not (Sys.file_exists path) then
    failwith (Printf.sprintf "File '%s' does not exist" path)
  else
    path

let validate_readable path =
  try
    Unix.access (validate_filename path) [Unix.R_OK];
    path
  with Unix.Unix_error(e,_,_) ->
    failwith (Printf.sprintf "Cannot read file '%s': %s"
                path (Unix.error_message e))

let validate_directory path =
  if not (Sys.is_directory path) then
    failwith ("Invalid path, expected an existing directory: " ^ path);
  path

let base64_re = regexp "^[A-Za-z0-9+/]+$"

let expect_length str expected =
  let n = String.length str in
  if n <> expected then
    failwith (Printf.sprintf "Invalid length %d, expected %d" n expected)

let validate_secret_key token =
  try
    expect_length token 56;
    try
      let decoded = Netencoding.Base64.decode token in
      let expected_bin = 56 / 4 * 3 in
      expect_length decoded expected_bin;
      let padding_0 = Char.code decoded.[expected_bin-2]
      and padding_1 = Char.code decoded.[expected_bin-1] in
      if padding_0 <> 0 || padding_1 <> 0 then
        failwith (Printf.sprintf "Invalid padding in token: %02x%02x" padding_0 padding_1);
      let sub = "CLUSTER/ALLNODE/ROOT/USER" in
      if String.sub token 0 (String.length sub) = sub then
        failwith "You cannot use the cluster key: use the admin key instead!";
      token
    with Invalid_argument _ ->
      failwith "expected base64-encoded string"
  with Failure msg ->
    failwith ("Invalid SX access token (secret key): " ^ msg)

let dns_re = regexp "^[a-zA-Z0-9-]+\\(\\.[a-zA-Z0-9-]+\\)*$"

let validate_dns_name name =
  if Ipaddr.of_string name <> None then
    failwith "Hostname expected, but IP address found";
  validate_string ~desc:"DNS name" ~expect:"alphanumeric or hyphen" dns_re name

let parse_ip s =
  Ipaddr.of_string_exn s

let parse_port s =
  let i = parse_int s in
  if i < 1L || i > 65535L then
    failwith (Printf.sprintf "Invalid port number (out of range): %Ld" i);
  Int64.to_int i

let validate_host s =
  match Ipaddr.of_string s with
  | Some (Ipaddr.V4 v4) -> Ipaddr.V4.to_string v4
  | Some (Ipaddr.V6 v6) -> Printf.sprintf "[%s]" (Ipaddr.V6.to_string v6)
  | None ->
    (* try to parse as a DNS name *)
    try validate_dns_name s
    with _ ->
      failwith (Printf.sprintf
                  "Invalid hostname (expected DNS name or IP address): %s" s)

let parse_positive_int s =
  let i = parse_int s in
  if i <= 0L || i >= 1073741824L then
    failwith (Printf.sprintf "Invalid number (must be strictly positive and <2^30): %Ld" i);
  Int64.to_int i

let parse_positive_float s =
  try
    let f = float_of_string s in
    if f < 0. then
      failwith "negative";
    f
  with Failure _ ->
    failwith (Printf.sprintf "Invalid number (must be floating point >= 0): %s" s)

let user_sep = regexp ":"
let user_re = regexp "^[A-Za-z0-9._][A-Za-z0-9._-]*$"

let validate_username u =
  validate_string ~desc:"user/group" ~expect:"POSIX-compatible name" user_re u

let parse_user_group_opt ref_user ref_group s =
  match split user_sep s with
  | user :: [] ->
    ref_user := Some (validate_username user)
  | user :: group :: [] ->
    ref_user := Some (validate_username user);
    ref_group := Some (validate_username group)
  | _ ->
    failwith ("Invalid user:group: " ^ s)

let validate_syslog s =
  let l = String.lowercase s in
  match l with
  | "auth" | "authpriv" | "console" | "cron" | "daemon" | "ftp" | "kernel"
  | "lpr"  | "local0" | "local1" | "local2" | "local3" | "local4" | "local5"
  | "local6" | "local7" | "mail" | "ntp" | "news" | "security" | "syslog"
  | "uucp" | "user" -> l
  | _ ->
    failwith ("Unknown syslog facility: " ^ l)

let deprecated ~old ~use f v field =
  old, (fun s ->
      Printf.eprintf "\nWarning: The config key '%s' is deprecated, use '%s' instead\n" old use;
      f v field s
    ), ""

(* server-specific configuration *)

let tmpdir = ref None
let base_hostname = ref ""
let base_listen_ip = ref None
let base_port = ref 8008
let base_ssl_port = ref 8443
let sx_host = ref None
let ssl_certificate_file = ref None
let ssl_privatekey_file = ref None
let pidfile = ref ""
let user = ref None
let group = ref None
let max_connected = ref 0 (* each server sets this *)
let daemonize = ref true

(* must match the value in _oasis *)
let config_file = "libres3/libres3.conf";;


let buckets_dir = ref "/"
(* TODO: use this *)
let syslog_facility = ref None (* local7 *)
let small_buffer_size = 4096
let min_multipart = 5242880L
let reply_ns = "http://s3.amazonaws.com/doc/2006-03-01/"
let max_keys = 1000
let verbose = ref false
let mimefile = ref (Filename.concat Configure.sysconfdir "libres3/mime.types")
let min_threads = ref None
let max_threads = ref None
let max_pool_threads = ref 64
let maxrequestbodysize = ref 5120 (* defined by S3 proto *)
let maxdetachedcomputationsqueued = ref None
let timeout = ref 30
let keepalivetimeout = ref 30
let shutdowntimeout = ref 10
let netbuffersize = ref 65536
let filebuffersize = ref 8192
let maxretries = ref 10

let check_interval = ref (float_of_int (24*60*60 + Random.int 3600 - 30*60))
let initial_interval = ref (float_of_int (Random.int 10800))

(* libres3.conf entries *)
let entries : (string * (string -> unit) * string) list = [
  "secret_key", expect validate_secret_key Config.secret_access_key,
  " SX secret access key";
  "s3_host", expect validate_dns_name base_hostname,
  " Base hostname to use (equivalent of s3.amazonaws.com, host_base in .s3cfg)";
  "s3_listen_ip", expect_opt parse_ip base_listen_ip,
  " Bind to specified IP (default: any IPv4/IPv6. To bind to IPv4 only use 0.0.0.0)";
  "s3_http_port", expect parse_port base_port,
  " Bind to specified HTTP port";
  "s3_https_port", expect parse_port base_ssl_port,
  " Bind to specified HTTPS port";
  "s3_ssl_certificate_file", expect_opt validate_readable ssl_certificate_file,
  " The path to the SSL certificate";
  "s3_ssl_privatekey_file", expect_opt validate_readable ssl_privatekey_file,
  " The path to the SSL certificate's private key";
  "sx_host", expect_opt validate_host sx_host,
  " Hostname of an SX cluster node";
  "sx_port", expect parse_port Config.sx_port,
  " Port of an SX cluster node";
  "replica_count", expect parse_positive_int Config.replica_count,
  " Default volume replica count";
  "volume_size", expect parse_size Config.volume_size,
  " Default volume size [K,M,G,T suffixes accepted]";
  "tmpdir", expect_opt validate_directory tmpdir,
  " Temporary directory";
  "logdir", expect (fun s -> s) Paths.log_dir,
  Printf.sprintf " Log directory (default: %s)" !Paths.log_dir;
  "syslog_facility", expect_opt validate_syslog syslog_facility,
  " Syslog facility to use (default: log to file only)";
  "pidfile", expect (fun s -> s) pidfile,
  " Specify the file where to write the pid of the server";
  "run-as", parse_user_group_opt user group,
  " user:group to use when dropping privileges";
  "max_parallel", expect parse_positive_int max_connected,
  " Maximum number of connections to handle in parallel";
  "allow_volume_create_any_user", expect parse_bool Config.volume_create_elevate_to_admin,
  " Allow creating volumes as any user (elevate to admin privileges)";
  "mimefile", expect validate_readable mimefile,
  (Printf.sprintf " Path to mime.types file (default: %s)" !mimefile);
  (* advanced configuration *)
  "min_threads", expect_opt parse_positive_int min_threads, "";
  "max_detached_threads", expect_opt parse_positive_int max_threads, "";
  "max_pool_threads", expect parse_positive_int max_pool_threads, "";
  "maxrequestbodysize_MiB", expect parse_positive_int maxrequestbodysize, "";
  "maxdetachedcomputationsqueued", expect_opt parse_positive_int maxdetachedcomputationsqueued, "";
  "timeout", expect parse_positive_int timeout, "";
  "keepalivetimeout", expect parse_positive_int keepalivetimeout, "";
  "shutdowntimeout", expect parse_positive_int shutdowntimeout, "";
  "netbuffersize", expect parse_positive_int netbuffersize, "";
  "filebuffersize", expect parse_positive_int filebuffersize, "";
  "maxretries", expect parse_positive_int maxretries, "";
  "keyid", expect (fun s -> s) Config.key_id, "";
  "list_cache_expires", expect parse_positive_float Config.list_cache_expires, "";
  "ver_check_interval", expect parse_positive_float check_interval, "";
  "ver_initial_interval", expect parse_positive_float initial_interval, "";
  (* for backwards compatibility *)
  deprecated ~old:"user" ~use:"run-as" expect_opt validate_username user;
  deprecated ~old:"group" ~use:"run-as" expect_opt validate_username group;
  deprecated ~old:"pid_file" ~use:"pidfile" expect (fun s -> s) pidfile;
  deprecated ~old:"s3_ssl_port" ~use:"s3_https_port" expect parse_port base_ssl_port;
  deprecated ~old:"s3_port" ~use:"s3_htts_port" expect parse_port base_port;
]
