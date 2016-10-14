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

open Alcotest
open Boundedio

let lwt_run = Lwt_main.run

module Json = struct
  open Jsonio

  let test_parse_error str (l,c) () =
    lwt_run (of_string str |> to_json >>> function
    | Ok _ -> fail (Failure (Printf.sprintf "Expected parse error on %S" str))
    | Error (Error.Json(Some (_,(l2,c2)), _)) ->
        check (pair int int) "range" (l,c) (l2,c2);
        return_unit
    | Error e -> fail e)

  let roundtrip str () =
    let transform s = lwt_run (of_string s |> observe ~prefix:"roundtrip" |>
                               expect_object >>= to_string) in
    let s1 = transform str in
    let s2 = transform s1 in
    Logs.debug (fun m -> m "%s" s1);
    check string "roundtrip" s1 s2

  let all = "{\"field1\":[37,1.4,-5.7,true,false,null,\"else\u2713\",{\"kettő\":{}}]}"
  let all_json = Jsonio.(of_string all |> to_json |> lwt_run)

  let test_of_json json () =
    let actual = Jsonio.(of_json json |> to_json |> lwt_run) in
    (* TODO: json dump *)
    assert (json = actual)

  open Jsonenc
  module type S = sig
    type header
    type t
    val streaming : (header,t) Jsonenc.streaming
    val example : string
    val pp : t Fmt.t
  end
  let test_example (module M:S) () =
    lwt_run (
      Jsonio.of_string M.example |>
      Jsonenc.decode M.streaming >>= fun r ->
      r |> Jsonenc.encode M.streaming |> Jsonio.to_string >>= fun str ->
      check string "roundtrip" M.example str;
      return_unit
    )

  let tests =
    "jsonio", [
      "empty", `Quick, test_parse_error "" (1,0);
      "{}", `Quick, roundtrip "{}";
      all, `Quick, roundtrip all;
      "{\"a\":}", `Quick, test_parse_error "{\"a\":}" (1,4);
      "of_json", `Quick, test_of_json all_json;
      "volume", `Quick, test_example (module Sx_volume)
    ]

end

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "LibreS3" [
    Json.tests
  ]
