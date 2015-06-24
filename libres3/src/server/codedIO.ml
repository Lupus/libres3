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

let small_buffer_size = 128
module Xml = struct
  type t = t Xmlm.frag

  (* helpers to build an Xmlm tree
   * without having to use the verbose specification
   * with namespaces *)

  let name ?ns s = match ns with
    | None -> "", s
    | Some ns -> ns, s;;

  let attr ?ns attr_name attr_value =
    name ?ns attr_name, attr_value;;

  let tag ?ns tag_name ?attrs children =
    `El (
      (
        name ?ns tag_name,
        match attrs with None -> [] | Some a -> a
      ),
      children
    );;

  let d str = `Data str

  let parse_string str : t =
    let source = Xmlm.make_input ~strip:true (`String (0, str)) in
    snd (Xmlm.input_doc_tree
           ~el:(fun tag lst -> `El (tag, lst)) ~data:(fun data -> `Data data) source
        );;

  let conv_id (x:t) = x

  let to_string ?decl tree =
    let buf = Buffer.create small_buffer_size in
    let out = Xmlm.make_output ~ns_prefix:(fun s -> Some "xml") ?decl (`Buffer buf) in
    Xmlm.output_doc_tree conv_id out (None, tree);
    Buffer.contents buf
  ;;
end

module Json = struct
  type t = [ `Null | `Bool of bool | `Float of float| `String of string
           | `A of t list | `O of (string * t) list ]

  exception Error of string

  let err d e =
    let b = Buffer.create small_buffer_size in
    let fmt = Format.formatter_of_buffer b in
    let (l1,c1),(l2,c2) = Jsonm.decoded_range d in
    Format.fprintf fmt "Bad JSON at %d:%d-%d:%d: %a%!" l1 c1 l2 c2
      Jsonm.pp_error e;
    raise (Error (Buffer.contents b));;

  let rec value d = function
    | `Lexeme `Os -> obj d []
    | `Lexeme `As -> arr d []
    | `Lexeme (`Null | `Bool _ | `String _ | `Float _ as v) -> v
    | `Error e -> err d e
    | `End | `Lexeme _ ->
      raise (Error ("Unexpected end of JSON"));
    | `Await -> assert false
  and arr d vs = match Jsonm.decode d with
    | `Lexeme `Ae -> `A (List.rev vs)
    | v -> arr d (value d v :: vs)
  and obj d ms = match Jsonm.decode d with
    | `Lexeme `Oe -> `O (List.rev ms)
    | `Lexeme `Name n ->
      obj d ((n, value d (Jsonm.decode d)) :: ms)
    | `Error e -> err d e
    | _ -> raise (Error ("Expected object end or name"))

  let of_string str : t =
    let d = Jsonm.decoder (`String str) in
    (value d (Jsonm.decode d))

  let expect_obj = function
    | `O o -> o
    | #t -> raise (Error "JSON object expected")

  let expect_array = function
    | `A a -> List.rev a
    | #t -> raise (Error ("JSON array expected"))

  let expect_string = function
    | `String s -> s
    | #t -> raise (Error ("JSON string expected"))

  let expect_array_or_string = function
    | `A a -> List.rev_map expect_string a
    | `String s -> [ s ]
    | #t -> raise (Error ("JSON array expected"))

  let to_string (json : t) =
    let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
    let rec value v k e = match v with
      | `A vs -> arr vs k e
      | `O ms -> obj ms k e
      | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
    and arr vs k e = enc e `As; arr_vs vs k e
    and arr_vs vs k e = match vs with
      | v :: vs' -> value v (arr_vs vs' k) e
      | [] -> enc e `Ae; k e
    and obj ms k e = enc e `Os; obj_ms ms k e
    and obj_ms ms k e = match ms with
      | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
      | [] -> enc e `Oe; k e
    in
    let b = Buffer.create small_buffer_size in
    let e = Jsonm.encoder ~minify:false (`Buffer b) in
    let finish e = ignore (Jsonm.encode e `End) in
    match json with
    | `A _ | `O _ as json ->
      value json finish e;
      Buffer.contents b
    | _ -> invalid_arg "invalid json text"
end
