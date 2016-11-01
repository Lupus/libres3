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

open Sx_services
open Cohttp
open Cryptokit

let get_date h = match Header.get h "Date" with
| Some v -> v
| None -> assert false

let hash_body body =
  let hash = Hash.sha1 () in
  let update chunk =
    hash#add_string chunk
  in
  Body.to_string_list body |> List.iter update;
  hash#result

let drop1 s =
  let n = String.length s in
  String.sub s 1 (n-1)

let filter next (token, req, body) =
  let now = Http_date.of_unix_timestamp (Unix.gettimeofday ()) in
  let headers = Http_date.add_header now (Request.headers req) in
  let d = Sx_config.Token.binary token in
  let i = String.sub d 0 20
  and k = String.sub d 20 20 in
  let resource = Request.uri req |> Uri.path_and_query |> drop1 in
  Logs.debug (fun m -> m "SX resource: %s" resource);
  let buf = String.concat "\n" [
      Request.meth req |> Code.string_of_method;
      resource;
      get_date headers;
      body |> hash_body |> transform_string (Hexa.encode ());
      ""
    ] in
  let signed = hash_string (MAC.hmac_sha1 k) buf in
  let a = i ^ signed ^ (String.sub d 40 2) in
  let auth = "SKY " ^ (transform_string (Base64.encode_compact ()) a) in
  next ((),{ req with
             Request.headers = Header.add_authorization headers (`Other auth)
           }, body)
