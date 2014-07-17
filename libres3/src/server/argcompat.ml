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

(* print -h and --help instead of -help and --help, and always hide flags
 * without documentation *)

open Netstring_str

let rec is_whitespace str pos =
  if pos < String.length str then
    if str.[pos] = ' ' then
      is_whitespace str (pos+1)
    else false
  else true

let usage_hide spec usage =
  (* hide flags with empty docs *)
  Printf.printf "%s\n" usage;
  List.iter (fun (spec, _, doc) ->
    if not (is_whitespace doc 0) then
      Printf.printf "  %s %s\n" spec doc
  ) (Arg.align spec)

let noop () = ()
let print_help ~extra spec usage () =
  usage_hide !spec usage;
  extra ();
  exit 0

let parse_align ~extra spec f usage =
  let spec = ref spec in
  spec := Arg.align (!spec @ [
    "-help",Arg.Unit (fun () -> raise (Arg.Bad "use --help or -h")),"";
    "-h",Arg.Unit (print_help ~extra spec usage), " Display this list of options";
    "--help",Arg.Unit (print_help ~extra spec usage), " Display this list of options"
  ]);
  Arg.parse !spec f usage
