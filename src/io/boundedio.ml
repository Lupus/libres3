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

(* Limits in terms of:
  *  - queue length (memory usage)
  *  - time spent in queue (timeout)
  *  - progress made on a stream
  *  - file descriptors
  *  - ephemeral ports
  *  - number of connections established to a target
  *  - fairness for queues
  * *)

open Result
type 'a t = 'a Lwt.t

let return = Lwt.return
let (>>=) = Lwt.bind

let return_unit = Lwt.return_unit
let return_some = Lwt.return_some
let return_none = Lwt.return_none

let (>>>) v f = match Lwt.state v with
| Lwt.Return v -> f (Ok v)
| Lwt.Fail e -> f (Error e)
| Lwt.Sleep -> Lwt.try_bind (fun () -> v) (fun v -> f (Ok v)) (fun e -> f (Error e))

let yield = Lwt_main.yield

let fail = Lwt.fail
let raise = `Use_fail_instead
let run = Lwt_main.run

let try_with f = try f () with exn -> Lwt.fail exn

let always_unit _ = Lwt.return_unit
let ignore t = t >>= always_unit

let delay seconds = Lwt_unix.sleep seconds
