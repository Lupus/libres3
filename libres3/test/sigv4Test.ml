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

open OUnit2

let data_dir =
  if Array.length Sys.argv <= 1 ||
     (String.length Sys.argv.(1) > 0 &&
      Sys.argv.(1).[0] = '-') then begin
    Printf.eprintf "Must specify the path to aws4_testsuite\n";
    exit 1
  end;
  Arg.current := 1;
  Sys.argv.(1)

module StringSet = Set.Make(String)
let list_tests () =
  let files = Array.to_list (Sys.readdir data_dir) in
  let sort_uniq l =
    StringSet.elements (List.fold_left (fun accum s -> StringSet.add s accum) StringSet.empty l) in
  sort_uniq (List.rev_map Filename.chop_extension files)

let nl = Netstring_str.regexp "\r?\n"

let load name ext =
  let path = Filename.concat data_dir (name ^ "." ^ ext) in
  let f = open_in path in
  let len = in_channel_length f in
  let s = String.make len '\x00' in
  really_input f s 0 len;
  close_in f;
  Netstring_str.global_replace nl "\n" s

let secret_key_v4 = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

let assert_str_equal ?msg expected actual =
  assert_equal ?msg ~printer:(fun s -> s) expected actual;;

open CanonRequest
let nl = Netstring_str.regexp " *\n"
let sep = Netstring_str.regexp ": *"
let parse_headers h =
  let headers, _, body =
    List.fold_left (fun (accum, is_body, body) line ->
        if is_body then accum, true, line :: body
        else match Netstring_str.bounded_split sep line 2 with
          | [ k; v ] -> (k, v) :: accum, false, body
          | [ k ] -> (k, "") :: accum, false, body
          | [] -> accum, true, body
          | _ -> assert false
      ) ([], false, []) h in
  headers, String.concat "\n" body

let meth_of_string ~body = function
  | "DELETE" -> `DELETE
  | "GET" -> `GET
  | "HEAD" -> `HEAD
  | "POST" -> `POST body
  | "PUT" -> `PUT body
  | _ -> `UNSUPPORTED

let parse_req s =
  match Netstring_str.split nl s with
  | req :: headers ->
    let headers, body = parse_headers headers in
    Scanf.sscanf req "%s@ %s@ " (fun meth path ->
        canonicalize_request ~id:(RequestId.generate ())
          (meth_of_string ~body meth) {
          req_headers = headers;
          undecoded_url = path
        }), body
  | _ ->
    failwith "malformed request"

let sigv4_test name =
  name>::(fun ctx ->
      try
        let signed_req = load name "sreq"
        and expected_canonical = load name "creq"
        and expected_string_to_sign = load name "sts" in
        let canon_req, body = parse_req signed_req in
        match CanonRequest.parse_authorization canon_req with
        | AuthorizationV4 (authv4, expected_signature,_) ->
          let sha256 = Cryptokit.hash_string (Cryptokit.Hash.sha256 ()) body in
          let canonical, tosign = string_to_sign_v4 authv4 ~sha256 ~canon_req in
          assert_str_equal ~msg:"canonical request" expected_canonical canonical;
          assert_str_equal ~msg:"string-to-sign-v4" expected_string_to_sign tosign;
          let signature =
            sign_string_v4 ~key:secret_key_v4 authv4.credential tosign
          in
          assert_str_equal ~msg:"signature" expected_signature signature
        | AuthMalformed s ->
          assert_failure ("cannot parse auth header: " ^ s)
        | AuthNone -> assert_failure ("auth = none")
        | AuthEmpty ->  assert_failure "auth empty"
        | AuthDuplicate -> assert_failure "auth duplicate"
        | Authorization (a,_,_) -> assert_failure ("bad auth version: " ^ a)
      with Sys_error msg ->
        skip_if true msg
    )

let () =
  run_test_tt_main (
    "sigv4 tests">:::(List.rev_map sigv4_test (list_tests ()))
  )
