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

open SXDefaultIO
let opt f = function
  | None -> "-"
  | Some v -> f v

let print_timestamp = Netdate.mk_date ~fmt:"%F %T UTC"

let string_of_timestamp timestamp =
  print_timestamp (timestamp)

let empty = None

let aname = [Html5.M.a_style "text-align: left"]
let adate = [Html5.M.a_style "text-align: left"]
let asize = [Html5.M.a_style "text-align: right"]

let chop_prefix path ~prefix =
  let n = String.length prefix in
  if String.length path >= n && String.sub path 0 n = prefix then
    String.sub path n (String.length path - n)
  else path

let html_of_list volume path l =
  let open Html5.M in
  let buf = Buffer.create 1024 in
  let titlestr = "Index of " ^ path in
  let prefix = chop_prefix path ~prefix:("/" ^ volume) in
  EventLog.debug (fun () -> Printf.sprintf "volume: %S, prefix: %S" volume prefix);
  let tr_of_file (name, meta) =
    let name = chop_prefix name ~prefix in
    EventLog.debug (fun () -> Printf.sprintf "-> %S" name);
    tr (
      (td ~a:adate [a ~a:[a_href name] [pcdata name]]) ::
      match meta with
      | None -> []
      | Some meta -> [
        td ~a:aname [pcdata (string_of_timestamp meta.mtime)];
        td ~a:asize [pcdata (Int64.to_string meta.size)]
      ]
    )
  in
  Html5.P.print ~output:(Buffer.add_string buf) (
    (html
       (head (title (pcdata titlestr)) [])
       (body [
           h1 [pcdata titlestr];
           hr ();
           tablex
             ~a:[a_style "width: 100%"]
             ~thead:(thead [
               tr [th ~a:aname [pcdata "Name"]; th ~a:adate [pcdata "Last Modified"]; th ~a:asize [pcdata "Size"]]
             ])
             [tbody (
                tr_of_file ("../", empty)
                ::
                (List.rev (List.rev_map tr_of_file l)))];
           hr ();
           p [pcdata "LibreS3 server"]
         ]
       )
    )
  );
  Buffer.contents buf

let html_of_volumes lst =
  let map_volume (vol, meta) =
    vol ^ "/", empty
  in
  html_of_list "" "" (List.rev (List.rev_map map_volume lst))
