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

open UnixLabels

let write_pid pidfile =
  let fd = openfile ~mode:[O_RDWR;O_CREAT] ~perm:0o750 pidfile in
  let pid = getpid () in
  try
    lockf fd ~mode:F_TLOCK ~len:0;
    UnixLabels.LargeFile.ftruncate fd ~len:0L;
    let str = Printf.sprintf "%d\n" pid in
    let _ = write fd ~buf:str ~pos:0 ~len:(String.length str) in
    ignore (UnixLabels.LargeFile.lseek fd 0L ~mode:SEEK_SET);
    lockf fd ~mode:F_TRLOCK ~len:0;
    (*at_exit (fun () -> try unlink pidfile with _ -> ());*)
  with
  | Unix_error(EACCES|EAGAIN as e ,_,_) ->
    failwith (Printf.sprintf "Another instance is running already, cannot lock pidfile '%s': %s"
                pidfile (error_message e))
  | Unix_error(e,_,_) ->
    Printf.eprintf "Failed to write pidfile '%s': %s\n%!" pidfile (error_message e);
    raise Exit;;

let is_running pid =
  try kill ~pid:pid ~signal:0; true
  with Unix_error(ESRCH,_,_) -> false

let rec wait_pid pid =
  if is_running pid then begin
    sleep 1;
    wait_pid pid
  end

let with_pidfile_read name f =
  try
    let ch = open_in name in
    Paths.readlocked (fun ch ->
        let pid = int_of_string (input_line ch) in
        f pid;
        close_in ch
    ) ch;
    None
  with
  | Sys_error e | Failure e ->
      Some (Printf.sprintf "PIDfile %s cannot be opened: %s\n%!" name e)
  | End_of_file -> Some ""

let kill_pid name =
  begin match with_pidfile_read name (fun pid ->
      Printf.printf "Sending TERM to PID %d ... %!" pid;
      begin try
          kill ~pid:(-pid) ~signal:15;
          Printf.printf "\n%!";
        with Unix_error(e,_,_) ->
          Printf.eprintf "Kill failed: %s!\n%!" (error_message e);
      end;
      Printf.printf "Waiting for PID %d ... %!" pid;
      wait_pid pid;
      Printf.printf "\n%!";
  ) with
  | Some str -> print_endline str
  | None -> ()
  end;
  try Sys.remove name with Sys_error _ -> ();;

let print_status name =
  print_endline "--- LibreS3 STATUS ---";
  Printf.printf "LibreS3 is ";
  begin match with_pidfile_read name (fun pid ->
      if is_running pid then
        Printf.printf "running (PID %d)" pid
      else
        Printf.printf "NOT running";
    ) with
  | Some _ -> Printf.printf "NOT running"
  | None -> ();
  end;
  print_endline "\n\n--- LibreS3 INFO ---";
  begin match !Configfile.ssl_privatekey_file with
    | Some f -> Printf.printf "SSL private key: %s" f
    | None -> ()
  end;
  Printf.printf "LibreS3 logs: ";
  begin match !Configfile.syslog_facility with
    | Some facility ->
      Printf.printf "syslog facility %s\n" facility
    | None->
      Printf.printf "directory %s\n" !Paths.log_dir
  end
