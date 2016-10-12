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

(* based on ezjonsm and jsonfilter *)

open Boundedio

module Error = struct
  type range = (int * int) * (int * int)
  let pp_range ppf ((l1,c1),(l2,c2)) =
    Fmt.pf ppf "%d:%d-%d:%d" l1 c1 l2 c2

  type t = Syntax of Jsonm.error | Expected of string * Jsonm.lexeme
  let pp ppf = function
  | Syntax e -> Jsonm.pp_error ppf e
  | Expected (expected, actual) ->
      Fmt.pf ppf "Expected %s but got %a" expected Jsonm.pp_lexeme actual

  exception Json of range * t

  let () =
    Printexc.register_printer (function
      | Json (range, err) ->
          Some (Fmt.strf "JSON error at %a: %a" pp_range range pp err)
      | _ -> None
      )

  let fail_json d e = Json(Jsonm.decoded_range d, e) |> fail
end

type parsed = Jsonm.decoder
type generated = unit
type +'a t = 'a * Jsonm.lexeme Lwt_stream.t

type json_value = [`String of string | `Bool of bool | `Float of float | `Null]
type json = Json_repr.ezjsonm

let rec lexeme stream d = match Jsonm.decode d with
| `Lexeme l -> Lwt.return_some l
| `Error e -> Error.(fail_json d (Syntax e))
| `End -> Lwt.return_none
| `Await -> Lwt_stream.get stream >>= function
  | None ->
      Jsonm.Manual.src d "" 0 0; lexeme stream d
  | Some str ->
      assert (String.length str > 0);
      Jsonm.Manual.src d str 0 (String.length str);
      lexeme stream d

let of_strings ?encoding stream =
  let d = Jsonm.decoder ?encoding `Manual in
  d, Lwt_stream.from (fun () -> lexeme stream d)

let expect_object (d,signals) =
  let fields () = Lwt_stream.next signals >>= function
    | `Oe -> return_none
    | `Name n -> return_some (n, (d,signals))
    | #Jsonm.lexeme -> assert false
  in
  Lwt_stream.next signals >>= function
  | `Os -> Lwt_stream.from fields |> return
  | #Jsonm.lexeme as l -> Error.(fail_json d (Expected("object", l)))

let expect_array (d,signals) =
  let elements () = Lwt_stream.next signals >>= function
    | `Ae -> return_none
    | #Jsonm.lexeme as l -> return_some l
  in
  Lwt_stream.next signals >>= function
  | `As -> return (d, Lwt_stream.from elements)
  | #Jsonm.lexeme as l -> Error.(fail_json d (Expected("array", l)))

let rec next signals =
  Lwt_stream.next signals >>= value signals
and value signals = function
| #json_value as v -> return v
| `As -> arr signals []
| `Os -> obj signals []
| `Ae | `Oe | `Name _ -> assert false
and arr signals lst = Lwt_stream.next signals >>= function
  | `Ae -> return (`A (List.rev lst))
  | v -> value signals v >>= fun e -> arr signals (e :: lst)
and obj signals lst = Lwt_stream.next signals >>= function
  | `Oe ->
      (* not required, but preserving original order makes it easier to test *)
      return (`O (List.rev lst))
  | `Name n -> next signals >>= fun v ->
      obj signals ((n, v) :: lst)
  | #json_value | `As | `Os | `Ae -> assert false

let to_json (_, signals) = next signals

let expect_eof (d,signals) =
  Lwt_stream.get signals >>= function
  | None -> return_unit
  | Some l -> Error.(fail_json d (Expected("EOF", l)))

let always _ = true
let drain (_,signals) = Lwt_stream.junk_while always signals

let of_json json =
  let stream, push = Lwt_stream.create () in
  let rec value = function
  | #json_value as v -> push (Some v)
  | `A lst ->
      push (Some `As);
      List.iter value lst;
      push (Some `Ae)
  | `O lst ->
      push (Some `Os);
      List.iter fields lst;
      push (Some `Oe)
  and fields (n, v) =
    push (Some (`Name n));
    value v
  in
  value json;
  push None;
  (), stream

let to_strings ?minify (_,signals) =
  (* we could use `Manual, but it doesn't support -safe-string,
     strings don't have to be the same size though, so just flush the buffer when size is exceeded *)
  let limit = Lwt_io.default_buffer_size () in
  let buf = Buffer.create (2 * limit) in
  let e = Jsonm.encoder ?minify (`Buffer buf) in
  let flush () =
    let s = Buffer.contents buf in
    Buffer.reset buf;
    if String.length s = 0 then return_none
    else return_some s
  in
  let encode v k =
    match Jsonm.encode e v with
    | `Ok ->
        if Buffer.length buf < limit then k ()
        else flush ()
    | `Partial -> assert false
  in
  let rec get () =
    Lwt_stream.get signals >>= function
    | None -> encode `End flush
    | Some l -> encode (`Lexeme l) get
  in
  Lwt_stream.from get
