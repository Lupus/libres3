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

(* load initial configuration file
 * live reload (SIGUSR1)
 * cache remote configuration locally
 * *)

open Boundedio
open Sx_config

let () =
  Printexc.register_printer (function
    | Unix.Unix_error (err, fn, arg) ->
        Some (Printf.sprintf "%s(%S): %s" fn arg (Unix.error_message err))
    | _ -> None
    )

let of_result = function
| Error (`Msg e) -> fail (Invalid_argument e)
| Ok v -> return v

let dotdot = function
| "." | ".." -> false
| _ -> true

let load_sx ?dir uri =
  (Location.of_uri ?dir uri |> of_result) >>= fun loc ->
  let open! Location in
  Lwt_io.with_file ~mode:Lwt_io.input loc.config Lwt_io.read >>= fun config ->
  Lwt_io.with_file ~mode:Lwt_io.input loc.auth Lwt_io.read >>= fun auth ->
  Lwt_unix.files_of_directory loc.nodes |> Lwt_stream.filter dotdot |> Lwt_stream.to_list >>= fun nodes ->
  of_config ~config ~auth ~nodes |> of_result

let base_hostname = ref ""
