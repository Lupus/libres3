(**************************************************************************)
(*  HTTP header helpers                                                   *)
(*  Copyright (C) 2012-2015 Skylable Ltd. <info-copyright@skylable.com>   *)
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

open Cohttp
open Sexplib.Std

(* rfc7234#section-5.2 *)
type t = (string * string option) list [@@deriving sexp]

type field = string
let max_age = "max-age"
let max_stale = "max-stale"
let min_fresh = "min-fresh"
let no_cache = "no-cache"
let no_store = "no-store"
let no_transform = "no-transform"
let only_if_cached = "only-if-cached"
let must_revalidate = "must-revalidate"
let public = "public"
let private_ = "private"
let proxy_revalidate = "proxy-revalidate"
let s_maxage = "s-maxage"

let parse_directive s =
  (* TODO: parse quoted-string *)
  match Stringext.split ~max:2 ~on:'=' s with
  | [ key; v ] -> (key, Some v)
  | [ key ] -> (key, None)
  | [] -> ("", None)
  | _ -> assert false

let fold_split accum v =
  List.rev_append (Stringext.split v ~on:',' |> List.rev_map String.trim) accum

let get_all h k =
  Header.get_multi h k |>
  List.fold_left fold_split []

let of_header h =
  get_all h "cache-control" |>
  List.rev_map parse_directive

let has_field = List.mem_assoc

let get_field field l =
  try List.assoc field l
  with Not_found -> None

type delta_seconds = int

let delta_seconds_of_string str =
  let v = Int64.of_string (String.trim str) in
  Int64.to_int (min v 1073741824L)

type field_name = string
let field_name_of_string = function
  | None -> []
  | Some s -> [ s ] (* TODO: how to parse *)

let add_field_opt headers field = function
  | None -> headers
  | Some value ->
    let controls = of_header headers in
    if has_field field controls then headers
    else Header.add headers "Cache-Control" (field ^ "=" ^ value)
