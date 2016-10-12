#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let build_tests c =
  if Conf.build_tests c then
    let qtest = Conf.tool "qtest" `Host_os in
    let skip path =
      let ext = Fpath.get_ext path in
      path = "src/cli" || (ext <> "" && ext <> ".ml") in
    OS.File.fold ~skip List.cons [] ["src"] >>= fun files ->
    OS.Cmd.run Cmd.(qtest % "extract" % "-o" % "test/run_qtest.ml" %% of_list files)
  else
  Topkg.Ok ()

let () =
  let open Pkg in
  let lint_deps_excluding = Some ["qtest";"qcheck"] in
  let opams = [opam_file ~install:false ~lint_deps_excluding "opam"] in
  (* TODO: build_cmd: use -j and non-classic display *)
  let cmd c os files = OS.Cmd.run Cmd.(Pkg.build_cmd c os % "-j" % "0" %% of_list files) in
  let build = build ~pre:build_tests ~cmd () in
  describe "libres3" ~opams ~build
    ~metas:[] ~licenses:[std_file "COPYING"]
    ~change_logs:[std_file "NEWS"] @@ fun _ ->
  Ok [
    sbin "src/server/libres3";
    bin "src/cli/libres3_setup";
    test "test/test";
    test "test/run_qtest";
  ]
