(**************************************************************************)
(*  LibreS3 server                                                        *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
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

(* Deterministic and fast test file generator *)
module Gen : sig
  type t
  val init : unit -> t
  val generate : t -> int -> Bytes.t
end = struct
  type state = { xx: int64; yy: int64 }
  type t = state ref

  let rotl x n =
    Int64.logor (Int64.shift_left x n) (Int64.shift_right_logical x (64 - n))

  open EndianBytes.NativeEndian_unsafe

  let rec rand_2cmres buf amount xx yy pos =
    if pos < amount then begin
      let t = xx in
      let xx = Int64.mul t 3188803096312630803L in
      let xx = Int64.sub (rotl xx 33) t in
      let t = yy in
      let yy = Int64.mul t (-3563753556205350509L) in
      let yy = Int64.sub (rotl yy 30) t in
      set_int64 buf pos (Int64.add xx yy);
      rand_2cmres buf amount xx yy (pos + 8)
    end else  { xx = xx; yy = yy }

  let generate s n =
    let buf = Bytes.create n in
    s := rand_2cmres buf n !s.xx !s.yy 0;
    buf

  let init () = ref { xx = 6012399790265199092L; yy = 3324834171879146134L }
end

let buf_size = 65536

let rec generate out g n =
  let buf = Gen.generate g buf_size in
  if Int64.of_int buf_size <= n then begin
    output_string out buf;
    generate out g (Int64.sub n (Int64.of_int buf_size))
  end else if n > 0L then
    output_string out (String.sub buf 0 (Int64.to_int n))

let () =
  if Array.length Sys.argv <> 3 then begin
    Printf.eprintf "Usage: %s <amount-min> <amount-max\n" Sys.argv.(0);
    exit 1
  end;
  let lo = Int64.of_string Sys.argv.(1)
  and hi = Int64.of_string Sys.argv.(2) in
  let g = Gen.init () in
  let filesize =
    Int64.add lo
      (max (Int64.sub (Int64.div (Int64.sub hi lo) 2L) 1L) 0L) in
  let t0 = Unix.gettimeofday () in

  generate stdout g filesize;

  let t1 = Unix.gettimeofday () in
  Printf.eprintf "Speed: %.0fMB/s, Filesize: %Ld\n"
    ((Int64.to_float filesize) /. (t1 -. t0) /. 1048576.)
    filesize
