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

open Core.Std
open Core_bench.Std
open Boundedio

let lwt_run_ign v = Pervasives.ignore (Lwt_main.run v)

let always _ = true
let discard stream = Lwt_stream.junk_while always stream

let raw_uncompress s =
  Cryptokit.(transform_string (Zlib.uncompress ())) s

let decode json =
  Ezjsonm.from_string json |>
  Json_encoding.destruct Sx_volume.ListFiles.all_encoding

let body_stream_of str =
  let a, b = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let wr = Lwt_io.of_fd ~mode:Lwt_io.Output a in
  let rd = Lwt_io.of_fd ~mode:Lwt_io.Input b in
  Lwt.async (fun () ->
      Lwt_io.write wr str >>= fun () ->
      Lwt_io.close wr);
  let read () =
    Lwt_io.read ~count:16384 rd >>= function
    | "" -> Lwt_io.close rd >>= fun () -> return_none
    | s -> return_some s
  in
  Lwt_stream.from read |>
  Cohttp_lwt_body.of_stream

let body_sink_of stream =
  let send str =
    Lwt_io.write Lwt_io.null str
  in
  Lwt_stream.iter_s send stream

let () =
  let json = In_channel.read_all "test/input.json.zlib" |> raw_uncompress in
  let decoded = decode json in
  let header, values = decoded in
  Lwt_io.set_default_buffer_size 65536;
  Gc.compact ();
  Command.run (Bench.make_command [
      Bench.Test.create ~name:"body tostring + ezjsonm parse + json decode" (fun () ->
          lwt_run_ign (body_stream_of json |>
                       Cohttp_lwt_body.to_string >>= fun body ->
                       let _ = decode body in
                       return_unit)
        );
      Bench.Test.create ~name:"jsonio parse + stream map" (fun () ->
          lwt_run_ign (body_stream_of json |>
                       Cohttp_lwt_body.to_stream |>
                       Jsonio.of_strings |>
                       Jsonenc.decode Sx_volume.ListFiles.streaming >>= fun (_json, map) ->
                       Lwt_stream.iter (fun (_,_) -> ()) map)
        );
      Bench.Test.create ~name:"json encode + ezjsonm tostring" (fun () ->
          match Json_encoding.construct Sx_volume.ListFiles.all_encoding decoded with
          | `O _ as v ->
              v |> Ezjsonm.to_string |> Cohttp_lwt_body.of_string |>
              Cohttp_lwt_body.to_stream |>
              body_sink_of
          | _ -> assert false
        );
      Bench.Test.create ~name:"json stream encode + jsonio" (fun () ->
          lwt_run_ign (let s = Lwt_stream.of_list values in
                       ({Sx_volume.ListFiles.volume_size=header},s) |>
                       Jsonenc.encode Sx_volume.ListFiles.streaming |>
                       Jsonio.to_strings |>
                       body_sink_of))
    ])
