(**************************************************************************)
(*  SX client                                                             *)
(*  Copyright (C) 2012-2014 Skylable Ltd. <info-copyright@skylable.com>   *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version.    *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,            *)
(*  MA  02110-1301  USA                                                   *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License. By "a publicly distributed version    *)
(*  of the Library", we mean either the unmodified Library, or a          *)
(*  modified version of the Library that is distributed under the         *)
(*  conditions defined in clause 3 of the GNU Library General Public      *)
(*  License. This exception does not however invalidate any other         *)
(*  reasons why the executable file might be covered by the GNU Library   *)
(*  General Public License.                                               *)
(**************************************************************************)

let buffer_size = 128*1024
let small_buffer_size = 4096
type output_data = string * int * int
module Thread = struct
  type 'a wakener = 'a Lwt.result -> unit
  let bad_result = Lwt.make_error (Failure "http dispatch")

  let result f =
    try
      Lwt.make_value (f ())
    with e ->
      Lwt.make_error e
    ;;

  let wait () =
    let waiter, wakener = Lwt.wait () in
    let result = ref bad_result in
    let notif_id = Lwt_unix.make_notification ~once:true (fun () ->
      Lwt.wakeup_result wakener !result) in
    waiter, (fun r ->
      result := r;
      Lwt_unix.send_notification notif_id
    )
  ;;
end

module OS = Lwt_unix
open Lwt
type output_stream = string -> int -> int -> unit Lwt.t

let try_finally fn_try fn_finally value =
  Lwt.catch (fun () -> fn_try value)
    (fun e ->
       (* run finally, ignoring any exceptions, and reraise original *)
       Lwt.catch (fun () -> fn_finally value) (fun _ -> Lwt.return_unit) >>= fun () ->
       Lwt.fail e) >>= fun result ->
  fn_finally value >>= fun () ->
  return result

let sleep = OS.sleep

let unlink = OS.unlink

type file_descr = OS.file_descr

let rec really_read fd buf pos n =
  if n > 0 then
    OS.read fd buf pos n >>= fun amount ->
    if amount = 0 then return pos
    else really_read fd buf (pos + amount) (n - amount)
  else return pos

let rec really_write out str pos len =
  if len <= 0 then return ()
  else
    OS.write out str pos len >>= fun amount ->
    really_write out str (pos + amount) (len - amount);;

let lseek fd pos =
  OS.LargeFile.lseek fd pos Unix.SEEK_SET >>= fun _ -> return ()

(* TODO: create a temporary dir, and create all tmpfiles there *)
let rng = Cryptokit.Random.device_rng "/dev/urandom"

let tempfilename () =
  let rand = Cryptokit.Random.string rng 16 in
  let hex = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) rand in
  Filename.concat (Netsys_tmp.tmp_directory ())
    (Printf.sprintf "libres3_upload_%s.tmp" hex)

let tmp_open name =
  OS.openfile name [Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL] 0o600 >>=
  fun fd -> OS.unlink name >>= fun () -> return fd

let with_tempfile f =
  let tmpfile = tempfilename () in
  tmp_open tmpfile >>=
  try_finally f Lwt_unix.close
