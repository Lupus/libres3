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

open Json_encoding

let default_strict = false (* for debugging, for production this will be false *)

let strict =
  try (Sys.getenv "LIBRES3_STRICT") = "1"
  with _ -> default_strict

let unexpected unexpected expected =
  raise (Cannot_destruct([], Unexpected(unexpected, expected)))

type 'a encoding = 'a Json_encoding.encoding

module Int53 = struct
  type t = int64
  let max_int = Int64.shift_left 1L 53
  let min_int = Int64.neg max_int
  let max_float = Int64.to_float max_int
  let min_float = Int64.to_float min_int

  let of_int64_exn v =
    if v < min_int || v > max_int then
      invalid_arg (Printf.sprintf "integer out of 53 bit range: %Ld" v);
    v

  let to_int64 v = v

  let of_string_exn str = of_int64_exn (Int64.of_string str)
  let to_string = Int64.to_string

  let of_float_exn f  =
    if f < min_float || f > max_float then
      invalid_arg (Printf.sprintf "Integer out of range: %f" f);
    Int64.of_float f

  let to_float = Int64.to_float

  let pp = Fmt.int64

  let encoding =
    let int53_of_float_exn f =
      let frac, integral = modf f in
      if frac > epsilon_float then
        unexpected  (string_of_float f) "Integer";
      try of_float_exn integral
      with Invalid_argument msg ->
        unexpected msg "Integer"
    in
    let schema = Json_schema.(Integer |> element |> create) in
    conv to_float int53_of_float_exn ~schema float
end

let http_date =
    let schema = Json_schema.(Number |> element |> create) in
    let to_unix d = Http_date.to_unix_timestamp d in
    conv to_unix Http_date.of_unix_timestamp ~schema float

let ipaddr = conv Ipaddr.to_string Ipaddr.of_string_exn string

let () =
  Printexc.register_printer (function
    | Json_encoding.Cannot_destruct _ as e ->
        Some (Fmt.to_to_string Json_encoding.print_error e)
    | _ -> None
    )

let assoc_any = assoc any_ezjson_value

open Json_schema
let singleton key_to_string key_of_string value_encoding =
  let to_assoc (k,v) = [key_to_string k, v] in
  let of_assoc = function
  | [k, v] -> (key_of_string k, v)
  | [] -> unexpected "empty object" "singleton object"
  | _ -> unexpected "additional fields" "singleton object"
  in
  assoc value_encoding |>
  conv to_assoc of_assoc

let obj_opt (type a) : a encoding -> a encoding = fun t ->
  if strict then t
  else
  match (t |> schema |> root).kind with
| Object obj_spec ->
    let known_fields = List.rev_map (fun (field,_,_,_) -> field) obj_spec.properties in
    let is_known_field (field,_) = List.mem field known_fields in
    let filter_fields all =
      List.filter is_known_field all |> construct assoc_any |>
      destruct t
    in
    let to_fields x = x |> construct t |> destruct assoc_any
    in
    let schema =
      let s = schema assoc_any in
      update (element (Object { object_specs with additional_properties = Some (root s)})) s
    in
    conv ~schema to_fields filter_fields assoc_any
| _ -> invalid_arg "Object schema expected"

open Jsonio

type 'a outer =
  {
    decode_header : json -> 'a;
    encode_header : 'a -> json;
    extract: field_stream -> (json * lexeme t option) Boundedio.t;
    streamingfield: string;
  }

type 'a obj_field = {
  decode_field : string * lexeme Jsonio.t -> 'a Boundedio.t;
  encode_field : 'a -> string * lexeme Jsonio.t;
}

type 'a arr_element = {
  decode_element : lexeme Jsonio.t -> 'a Boundedio.t;
  encode_element : 'a -> lexeme Jsonio.t;
}

type 'a inner =
  | Obj of 'a obj_field
  | Arr of 'a arr_element

type ('a,'b) streaming = 'a outer * 'b inner

open Boundedio
let streaming encoding streamingfield =
  match (encoding |> schema |> root).kind with
  | Object obj_spec ->
      let known_fields = List.rev_map (fun (field,_,_,_) -> field) obj_spec.properties in
      let decode_header json =
        Json_encoding.destruct encoding json in
      let encode_header v = Json_encoding.construct encoding v in
      let extract = extract_fields known_fields streamingfield in
      { decode_header; encode_header; extract; streamingfield}
  | _ -> invalid_arg "Object schema expected"

let obj_streaming enc field streamenc =
  let decode_field (n, s) =
    Jsonio.to_json s >>= fun json ->
    return (n, Json_encoding.destruct streamenc json) in
  let encode_field (n, v) =
    n, Json_encoding.construct streamenc v |> Jsonio.of_json in
  streaming enc field,
  Obj { decode_field; encode_field }

let arr_streaming enc field streamenc =
  let decode_element s =
    Jsonio.to_json s >>= fun json ->
    (* TODO: error handling, report parent fields and pos *)
    return (Json_encoding.destruct streamenc json) in
  let encode_element v =
    Json_encoding.construct streamenc v |> Jsonio.of_json in
  streaming enc field,
  Arr { decode_element; encode_element; }

open Lwt
let decode (d,i) stream =
  expect_object stream >>= fun obj_stream ->
  obj_stream |> fields |> d.extract >>= fun (json, v) ->
  match v with
  | None -> fail (Failure "TODOX")
  | Some s ->
      let h = d.decode_header json in
      match i with
      | Obj os ->
          expect_object s >>= fun stream ->
           return (h,
                   Lwt_stream.map_s os.decode_field (fields stream))
      | Arr es ->
          expect_array s >>= fun stream ->
          return (h,
                  Lwt_stream.map_s es.decode_element (elements stream))

let encode (e,i) (header, body) : [> `Os] Jsonio.t =
  match e.encode_header header with
  | `O fields ->
      let fields = List.rev_map (fun (n,v) -> n, Jsonio.of_json v) fields in
      let last = match i with
      | Obj os -> Lwt_stream.map os.encode_field body |> Jsonio.build_object
      | Arr es -> Lwt_stream.map es.encode_element body |> Jsonio.build_array
      in
      (e.streamingfield, last) :: fields |> List.rev |>
      Lwt_stream.of_list |> Jsonio.build_object
  | #json -> assert false
