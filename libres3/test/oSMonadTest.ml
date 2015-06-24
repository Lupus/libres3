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

open OUnit
module OS = EventIO.OS
open Lwt

let id x = x
let run = Lwt_unix.run

let expect_unix_error code f =
  let has_run = ref false in
  let codestr = Unix.error_message code in
  run (Lwt.catch
         (fun () ->
            f () >>= fun _ ->
            assert_failure (Printf.sprintf "Expected exception '%s' not raised" codestr)
         )
         (function
           | Unix.Unix_error(actual,_,_) ->
             assert_equal ~printer:Unix.error_message code actual;
             has_run := true;
             return ()
           | e ->
             assert_failure
               (Printf.sprintf
                  "Bad exception: %s" (Printexc.to_string e))
         )
      );
  assert_bool
    (Printf.sprintf "Expected '%s'" codestr)
    !has_run
;;

let test_openfile_enoent (_,name) =
  expect_unix_error Unix.ENOENT (fun () ->
      OS.openfile name [] 0o700
    );;

let test_openfile_create (_,name) =
  run (
    OS.openfile name [Unix.O_CREAT] 0o700 >>= fun fd ->
    OS.close fd >>= fun () ->
    OS.openfile name [] 0o700 >>= fun fd ->
    OS.close fd >>= fun () ->
    OS.unlink name
  );;

let test_write_read (_,name) =
  run (
    OS.openfile name [Unix.O_CREAT;Unix.O_WRONLY] 0o700 >>= fun fd ->
    OS.write fd "test" 0 4 >>= fun _ ->
    OS.close fd >>= fun () ->
    OS.openfile name [] 0o700 >>= fun fd ->
    let buf = String.create 4 in
    OS.read fd buf 0 4 >>= fun _ ->
    assert_equal ~printer:id "test" buf;
    OS.close fd >>= fun () ->
    OS.unlink name
  );;

let test_write_read_seek (_,name) =
  run (
    OS.openfile name [Unix.O_CREAT;Unix.O_WRONLY] 0o700 >>= fun fd ->
    OS.write fd "Xtest" 1 4 >>= fun _ ->
    OS.LargeFile.lseek fd 0L Unix.SEEK_CUR >>= fun pos ->
    assert_equal ~printer:Int64.to_string ~msg:"write pos" 4L pos;
    OS.LargeFile.lseek fd (-1L) Unix.SEEK_END >>= fun pos ->
    assert_equal ~printer:Int64.to_string ~msg:"write pos" 3L pos;
    OS.LargeFile.lseek fd 4L Unix.SEEK_SET >>= fun pos ->
    assert_equal ~printer:Int64.to_string ~msg:"write pos" 4L pos;
    OS.write fd "XYTESTY" 2 4 >>= fun _ ->
    OS.close fd >>= fun () ->
    OS.openfile name [] 0o700 >>= fun fd ->
    let buf = String.create 9 in
    buf.[0] <- 'a';
    OS.read fd buf 1 8 >>= fun _ ->
    assert_equal ~printer:id "atestTEST" buf;
    OS.close fd >>= fun () ->
    OS.unlink name
  );;

let test_unlink_enoent (_,name) =
  expect_unix_error Unix.ENOENT (fun () ->
      OS.unlink name
    );;

let setup_tmpdir () =
  let name = Filename.temp_file "tmplibres3test" ".dir" in
  Sys.remove name;
  Unix.mkdir name 0o700;
  name, Filename.concat name "testname";;

let teardown_tmpdir (name, _) =
  Unix.rmdir name

let bracket_tmpdir f =
  bracket setup_tmpdir f teardown_tmpdir;;

let largefile_name = "test-largefile"

let setup_largefile () =
  OS.openfile largefile_name [Unix.O_CREAT;Unix.O_RDWR] 0o700;;

let teardown_largefile file =
  run (
    file >>= fun fd ->
    OS.close fd >>= fun () ->
    OS.unlink largefile_name
  );;

let bracket_largefile f =
  bracket setup_largefile f teardown_largefile;;

let test_largefile file =
  run (
    file >>= fun fd ->
    let large_pos = Int64.shift_left 1L 33 in
    OS.LargeFile.fstat fd >>= fun stat ->
    assert_equal ~printer:Int64.to_string stat.Unix.LargeFile.st_size 0L;
    OS.write fd "a" 0 1 >>= fun _ ->
    OS.LargeFile.lseek fd large_pos Unix.SEEK_SET >>= fun pos ->
    assert_equal ~printer:Int64.to_string large_pos pos;
    OS.write fd "t" 0 1 >>= fun _ ->
    OS.LargeFile.fstat fd >>= fun stat2 ->
    assert_equal ~printer:Int64.to_string
      stat2.Unix.LargeFile.st_size
      (Int64.add large_pos 1L);
    let buf = String.create 1 in
    OS.LargeFile.lseek fd 0L Unix.SEEK_SET >>= fun _ ->
    OS.read fd buf 0 1 >>= fun _ ->
    assert_equal ~printer:id "a" buf;
    OS.LargeFile.lseek fd large_pos Unix.SEEK_SET >>= fun _ ->
    OS.read fd buf 0 1 >>= fun _ ->
    assert_equal ~printer:id "t" buf;
    return ();
  );;

let tests =
  "OS">:::
  (List.map (fun (name, f) ->  name>::bracket_tmpdir f)
     [
       "openfile enoent", test_openfile_enoent;
       "unlink enoent", test_unlink_enoent;
       "openfile create", test_openfile_create;
       "write_read", test_write_read;
       "write_read_seek", test_write_read_seek;
     ]
  ) @
  [
    "large file">:: bracket_largefile test_largefile
  ]
