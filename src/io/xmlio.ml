(**************************************************************************)
(*  Copyright (C) 2014-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
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

open Markup
open Rresult

type xml = 'a Markup.node as 'a
let api_ns = "http://s3.amazonaws.com/doc/2006-03-01"

let element ?ns name ?(attrs=[]) children =
  let ns, attrs = match ns with
  | None -> api_ns, attrs
  | Some ns ->
      ns, ((Ns.xmlns, "xmlns"), ns) :: attrs
  in
  `Element ((ns,name), attrs, children)

let element_nons name ?(attrs=[]) children =
  `Element (("",name),attrs,children)

let add_opt_element ?ns name ?attrs f v lst =
  match v with
  | None -> lst
  | Some v ->
      element ?ns name ?attrs (f v) :: lst

let text str = [ `Text str ]

let text_of_int i = text (string_of_int i)
    
let opt f = function
| None -> []
| Some v -> f v

let root name ?attrs children =
  element ~ns:api_ns name ?attrs children

let id x = x

let prefix accum v = match accum with
| false -> [v], Some false
| true -> [`Xml { version="1.0"; encoding=Some "UTF-8"; standalone=None }; v], Some false

let namespace ns = Some ns

let to_string (t:xml) =
  let report =
    let count = ref 0 in
    fun location error ->
      Printf.eprintf "Xmlio.to_string: %s\n%!" (Error.to_string error);
      count := !count + 1;
      if !count >= 10 then raise_notrace Exit
  in
  from_tree id t |>
  transform prefix true |>
  write_xml ~report ~prefix:namespace |>
  to_string

let expect_root tag : xml option -> (xml list, Rresult.R.msg) result  = function
| Some (`Element ((_, actual_tag), _, children)) when tag = actual_tag -> Ok children
| Some (`Element ((_,actual_tag),_,_)) ->
    R.error_msgf "Bad xml root tag, expected '%s', but got '%s'" tag actual_tag
| Some (#Markup.node) -> R.error_msg "Bad xml: no root element"
| None -> Ok []

let is_tag ~tag = function
| `Element ((_, actual_tag),_,_) -> actual_tag = tag
| #Markup.node -> false

let of_string str =
  let report =
    let count = ref 0 in
    fun location error ->
      Logs.warn (fun m -> m "Xmlio.of_string: %s" (Error.to_string error));
      count := !count + 1;
      if !count >= 10 then raise_notrace Exit
  in
  Logs.debug (fun m -> m "Xmlio.of_string: %s" str);
  str |> string |> parse_xml ~report |> signals |>
  trees ~text:(fun ss -> `Text (String.concat "" ss))
    ~element:(fun tag attrs children -> `Element (tag, attrs, children)) |>
  to_list |> function
  | [] ->
      Logs.debug (fun m -> m "parsed to none!");
      None
  | hd :: tl -> Some hd
