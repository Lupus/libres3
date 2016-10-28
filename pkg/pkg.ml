#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let core_bench = Conf.with_pkg "core_bench"

let () =
  let open Pkg in
  let lint_deps_excluding = Some ["qtest";"qcheck"] in
  let opams = [opam_file ~install:false ~lint_deps_excluding "opam"] in
  (* TODO: build_cmd: use -j and non-classic display *)
  let cmd c os files = OS.Cmd.run Cmd.(Pkg.build_cmd c os % "-j" % "0" %% of_list files) in
  let build = build ~cmd () in
  describe "libres3" ~opams ~build
    ~metas:[] ~licenses:[std_file "COPYING"]
    ~change_logs:[std_file "NEWS"] @@ fun c ->
  let bench = Conf.value c core_bench in
  Ok [
    sbin "src/server/libres3";
    bin "src/cli/libres3_setup";
    test "test/test";
    test ~run:false "test/ls";
    test ~cond:bench ~run:false "test/json_bench";
    test ~run:false "test/minserv";
  ]
