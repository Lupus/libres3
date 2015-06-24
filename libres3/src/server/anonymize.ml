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

open Re_posix
open Cryptokit
open Ipaddr

let cidr = Prefix.of_string_exn

(* from SX's anonymize.c *)
let ip_classify = [
  "IP4-RFC1122", [ cidr "0.0.0.0/8"; cidr "127.0.0.0/8" ];
  "IP4-RFC1918", [
    cidr "10.0.0.0/8";
    cidr "172.16.0.0/12";
    cidr "192.168.0.0/16"
  ];
  "IP4-RFC2544", [ cidr "198.18.0.0/15" ];
  "IP4-RFC3068", [ cidr "192.88.99.0/24" ];
  "IP4-RFC3927", [ cidr "169.254.0.0/16" ];
  "IP4-RFC6333", [ cidr "192.0.0.0/29" ];
  "IP4-RFC6598", [ cidr "100.64.0.0/10" ];
  "IP4-RFC919", [ cidr "255.255.255.255/32" ];
  "IP6-LOOPBACK", [ cidr "::1/128" ];
  "IP6-UNSPEC", [ cidr "::/128" ];
  "IP6-V4MAPPED", [ cidr "::ffff:0:0/96" ];
  "IP6-RFC6052", [ cidr "64:ff9b::/96" ];
  "IP6-RFC6666", [ cidr "100::/64" ];
  "IP6-RFC5180", [ cidr "2001:2::/48" ];
  "IP6-RFC3849", [ cidr "2001:db8::/32" ];
  "IP6-RFC4843", [ cidr "2001:10::/28" ];
  "IP6-RFC4380", [ cidr "2001::/32" ];
  "IP6-RFC3056", [ cidr "2002::/16" ];
  "IP6-RFC4193", [ cidr "fc00::/7" ];
  "IP6-LL", [ cidr "fe80::/10" ]
]

let ip_cidr_match addr (_, cidrs) =
  List.exists (fun cidr ->
    match addr, cidr with
    | V4 _, V4 _ -> Prefix.mem addr cidr
    | V6 _, V6 _ -> Prefix.mem addr cidr
    | _ -> false
  ) cidrs

let anon_ip str =
  match of_string str with
  | None -> None
  | Some addr ->
    try
      let act, _ = List.find (ip_cidr_match addr) ip_classify in
      Some act
    with Not_found ->
      match addr with
      | V4 _ ->
        Some "IP4"
      | V6 _ ->
        Some "IP6"

let anon_quoted str =
  if String.length str > 1 && str.[String.length str - 2] <> '}' then
    Some "QSTR"
  else
    None

let anon_mac _ =
  Some ":MAC"

let anon_dns _ = Some "NAME"

let verb_re = Re_posix.compile_pat "[A-Z]+"

let anon_verb _ =
  Some ":URL"

let regexes = [
  "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}", anon_ip;
  " [0-9a-fA-F]{2}(:[0-9a-fA-F]{2}){5}", anon_mac;
  "[0-9a-fA-F]{0,4}(:[0-9a-fA-F]{0,4}){1,6}:([0-9a-fA-F]{0,4}|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))", anon_ip;
  "[a-zA-Z-][a-zA-Z0-9-]*(\\.[a-zA-Z0-9-]+)+", anon_dns;
  "'[^']+'", anon_quoted;
  "(OPTIONS|GET|HEAD|POST|PUT|DELETE|TRACE|CONNECT)[^:]+://[^| ]+", anon_verb
]

let filters = List.rev (List.rev_map (fun (regex, action) ->
  compile_pat regex, fun str -> try action str with _ -> None
) regexes)

let hmac_key = ref ""

let truncate_half str =
  String.sub str 0 (String.length str / 2)

let anonymize_item act item =
  let hmac = MAC.hmac_sha1 !hmac_key in
  let b64 = transform_string (Base64.encode_compact ())
    (truncate_half (hash_string hmac (act ^ "\x00" ^ item))) in
  for i = 0 to String.length b64-1 do
    if b64.[i] = '/' then
      b64.[i] <- '_'
    else if b64.[i] = '+' then
      b64.[i] <- '-'
  done;
  act ^ "{" ^ b64 ^ "}"

let buf = Buffer.create 1024

let filter_regex str (regex, action) =
  Buffer.reset buf;
  let rec loop pos =
    if pos >= String.length str then
      Buffer.contents buf
    else
      let matchresult =
        try
          let subs = Re.exec ~pos regex str in
          Some (Re.get_ofs subs 0, Re.get subs 0)
        with Not_found -> None in
      match matchresult with
      | Some ((matchpos, matchend), pat) ->
        begin match action pat with
        | Some act ->
          Buffer.add_substring buf str pos (matchpos - pos);
          Buffer.add_string buf (anonymize_item act pat);
          loop matchend
        | None ->
          Buffer.add_char buf str.[pos];
          loop (pos+1)
        end
      | None ->
          Buffer.add_substring buf str pos (String.length str - pos);
          Buffer.contents buf
  in
  loop 0

let filter str =
  List.fold_left filter_regex str filters
