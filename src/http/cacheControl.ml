(**************************************************************************)
(*  Copyright (C) 2012-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
(*                                                                        *)
(*  Permission to use, copy, modify, and distribute this software for     *)
(*  any purpose with or without fee is hereby granted, provided that the  *)
(*  above copyright notice and this permission notice appear in all       *)
(*  copies.                                                               *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL         *)
(*  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED         *)
(*  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE      *)
(*  AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL  *)
(*  DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA    *)
(*  OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER     *)
(*  TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR      *)
(*  PERFORMANCE OF THIS SOFTWARE.                                         *)
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
