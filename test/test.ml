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

open Sx_types
open Astring

open Sx_service

let lwt_run = Lwt_main.run

module Json = struct
  open Jsonio

  let json_check = (module struct
    type t = string
    let equal a b =
      Jsonio.equal (Ezjsonm.from_string a) (Ezjsonm.from_string b)

    let rec canon = function
    | `O lst -> `O (canon_obj lst)
    | `A lst -> `A (canon_arr lst)
    | #json_primitive as v -> v

    and canon_obj lst =
      String.Map.(lst |> of_list |> map canon |> bindings)

    and canon_arr lst =
      (List.rev_map canon lst |> List.rev)

    let canon_lst = function
    | `O lst -> `O (canon_obj lst)
    | `A lst -> `A (canon_arr lst)

    let pp ppf s =
      s |> Ezjsonm.from_string |> canon_lst |> Ezjsonm.to_string ~minify:false |>
      Fmt.string ppf

  end : TESTABLE with type t = string)

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

  let all = "{\"field1\":[37,1.4,-5.7,true,false,null,\"else\\u2713\",{\"kettő\":{}}]}"
  let all_json = Jsonio.(of_string all |> to_json |> lwt_run)

  let test_of_json json () =
    let actual = Jsonio.(of_json json |> to_json |> lwt_run) in
    (* TODO: json dump *)
    assert (json = actual)

  module type S = sig
    type header
    type t
    type element
    val streaming : (header, element) Jsonenc.streaming
    val example : string
    val pp : t Fmt.t
  end

  let test_example (module M:S) () =
    (* TODO: also test inserting unknown field *)
    (* TODO: order of fields in object doesn't matter, allow for that *)
    lwt_run (
      let s = Jsonio.of_string M.example in
      Jsonenc.decode M.streaming s >>= fun r ->
      r |> Jsonenc.encode M.streaming |> Jsonio.to_string >>= fun str ->
      check json_check "roundtrip" M.example str;
      return_unit
    )

  let test_simple_example (module M:JsonQuery) () =
    (* TODO: also test inserting unknown field *)
    (* TODO: order of fields in object doesn't matter, allow for that *)
    lwt_run (
      Jsonio.of_string M.example |>
      Jsonio.to_json >>= fun json ->
      let r = Json_encoding.destruct M.encoding json in
      r |> Json_encoding.construct M.encoding |> Jsonio.of_json |>
      expect_object >>= Jsonio.to_string >>= fun str ->
      check json_check "roundtrip" M.example str;
      return_unit
    )

  let tests =
    "jsonio", [
      "empty", `Quick, test_parse_error "" (1,0);
      "{}", `Quick, roundtrip "{}";
      all, `Quick, roundtrip all;
      "{\"a\":}", `Quick, test_parse_error "{\"a\":}" (1,4);
      "of_json", `Quick, test_of_json all_json;
      "job", `Quick, test_simple_example (module Job);
      "job poll", `Quick, test_simple_example (module Job.Poll);
      "nodelist", `Quick, test_simple_example (module Sx_cluster.ListNodes);
      "get cluster meta", `Quick, test_simple_example (module Sx_cluster.Meta.Get);
      "get cluster meta", `Quick, test_simple_example (module Sx_cluster.Meta.Set);
      "user list", `Quick, test_simple_example (module Sx_cluster.Users.List);
      "create user", `Quick, test_simple_example (module Sx_cluster.Users.Create);
      "self", `Quick, test_simple_example (module Sx_cluster.Users.Self);
      "modify user", `Quick, test_simple_example (module Sx_cluster.Users.Modify);
      "volume list", `Quick, test_simple_example (module Sx_volume.ListVolumes);
      "volume create", `Quick, test_simple_example (module Sx_volume.Create);
      "volume modify", `Quick, test_simple_example (module Sx_volume.Modify);
      "volume modify replica", `Quick, test_simple_example (module Sx_volume.ModifyReplica);
      "get volume acl", `Quick, test_simple_example (module Sx_volume.Acl.Get);
      "update volume acl", `Quick, test_simple_example (module Sx_volume.Acl.Update);
      "list files", `Quick, test_example (module Sx_volume.ListFiles);
      "get file", `Quick, test_example (module Sx_file.Get);
      "get file meta", `Quick, test_simple_example (module Sx_file.Meta);
      "initialize file put", `Quick, test_example (module Sx_file.Initialize.Request);
      "initialize file reply", `Quick, test_example (module Sx_file.Initialize.Reply);
      "initialize add chunk", `Quick, test_simple_example (module Sx_file.Initialize.AddChunk);
      "list revisions", `Quick, test_simple_example (module Sx_file.ListRevisions)
    ]
end

module Xml = struct
  let test_xml (module M : S3_types.S) () =
    List.iter (fun (input, expected_output) ->
        match M.to_reply input with
        | _, None -> failwith "Expected XML reply, got none"
        | _, Some xml ->
            check string "xml reply" expected_output (Xmlio.to_string xml)
      ) M.examples

  let tests = "xmlio", [
      "GET Service", `Quick, test_xml (module Bucket.Service);
      "List Objects V2", `Quick, test_xml (module Bucket.ListObjects.Reply);
    ]
end

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "LibreS3" [
    Json.tests;
    Xml.tests;
  ]
