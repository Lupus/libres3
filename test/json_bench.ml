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

let lwt_run_ign v = ignore (Lwt_main.run v)

let always _ = true
let discard stream = Lwt_stream.junk_while always stream

let raw_uncompress s =
  Cryptokit.(transform_string (Zlib.uncompress ())) s

let decode json =
  Ezjsonm.from_string json |>
  Json_encoding.destruct Sx_volume.all_encoding

let always _ = true
let drain_stream stream = Lwt_stream.junk_while always stream

let () =
  let json = In_channel.read_all "test/input.json.zlib" |> raw_uncompress in
  let decoded = decode json in
  let header, values = decoded in
  Lwt_io.set_default_buffer_size 65536;
  Gc.compact ();
  Command.run (Bench.make_command [
      Bench.Test.create ~name:"ezjsonm parse + json decode" (fun () ->
          ignore (decode json)
        );
      Bench.Test.create ~name:"jsonio parse + stream map" (fun () ->
          let open Boundedio in
          lwt_run_ign (Jsonio.of_string json |>
                       Jsonenc.decode Sx_volume.streaming >>= fun (_json, map) ->
                       Lwt_stream.iter (fun (_,_) -> ()) map)
        );
      Bench.Test.create ~name:"json encode + ezjsonm tostring" (fun () ->
          match Json_encoding.construct Sx_volume.all_encoding decoded with
          | `O _ as v -> ignore (Ezjsonm.to_string v)
          | _ -> assert false
        );
      Bench.Test.create ~name:"json stream encode + jsonio drain" (fun () ->
          lwt_run_ign (let s = Lwt_stream.of_list values in
                       ({Sx_volume.volume_size=header},s) |>
                       Jsonenc.encode Sx_volume.streaming |>
                       Jsonio.to_strings |>
                       drain_stream))
    ])
