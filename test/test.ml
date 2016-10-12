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

module Q = QCheck
open Boundedio

module Arbitrary = struct
  open Q
  let map = map ~rev:run
  let io = choose [
      map return int;
      map (fun x ->
          yield () >>= fun () ->
          return x) int;
      map (fun x ->
          yield () >>= fun () ->
          return x >>= fun y ->
          yield () >>= fun () ->
          return y) int;
    ]

  let fn = fun1 int io
end

let eq a b = (run a) = (run b)

let run tests =
  try
    QCheck_runner.(run ("LibreS3" >::: tests)) |> exit
  with Arg.Bad msg -> print_endline msg; exit 1
     | Arg.Help msg -> print_endline msg; exit 0

let () =
  run [
    Q.Test.make ~name:"left identity" Q.(pair int Arbitrary.fn) (fun (a,f) ->
        eq (return a >>= f) (f a)
      );
    Q.Test.make ~name:"right identity" Q.int (fun a ->
        let m = return a in
        eq (m >>= return) m
      );
    Q.Test.make ~name:"associativity" Q.(pair int Arbitrary.(pair fn fn)) (fun (v, (f,g)) ->
        let m1 = return v and m2 = return v in
        eq ((m1 >>= f) >>= g) (m2 >>= (fun x -> f x >>= g))
      )
  ]
