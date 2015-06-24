(**************************************************************************)
(*  Copyright (C) 2014-2015, Skylable Ltd. <info-copyright@skylable.com>  *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above            *)
(*  copyright notice, this list of conditions and the following           *)
(*  disclaimer in the documentation and/or other materials provided       *)
(*  with the distribution.                                                *)
(*                                                                        *)
(*  3. Neither the name of the copyright holder nor the names of its      *)
(*  contributors may be used to endorse or promote products derived       *)
(*  from this software without specific prior written permission.         *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,            *)
(*  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES    *)
(*  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR    *)
(*  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)    *)
(*  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,   *)
(*  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)         *)
(*  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED   *)
(*  OF THE POSSIBILITY OF SUCH DAMAGE.                                    *)
(**************************************************************************)

module type MonadTest = sig
  include LRUCacheTypes.TryMonad
  val name : string
  val run : 'a t -> 'a
  val delay : float -> unit t
end

module Make(Monad: MonadTest) = struct
  open OUnit

  module C = LRUCacheMonad.Make(Monad)

  let key1 = "test"
  let key2 = "test2"
  let val_d = 42.
  let val_p = 43.

  open Monad

  let compute_data_direct cnt key =
    incr cnt;
    assert_equal ~msg:"cache key" key1 key;
    return val_d

  let compute_data cnt _ =
    incr cnt;
    return val_p

  let compute_data_p cnt key =
    incr cnt;
    assert_equal ~msg:"cache key" key2 key;
    return val_p

  let assert_equal_int ~msg expected actual =
    assert_equal ~msg ~printer:string_of_int expected actual

  let cache = C.create 100
  let lookup cache =
    C.lookup cache

  let test_cache_bind_seq () =
    let cnt = ref 0 in
    let v1 = Monad.run (lookup cache key1 (compute_data_direct cnt)) in
    assert_equal ~msg:"cache value1" (C.Result.OK val_d) v1;
    let v2 = Monad.run (lookup cache key1 (compute_data_direct cnt)) in
    assert_equal ~msg:"cache value2" (C.Result.OK val_d) v2;
    assert_equal_int ~msg:"number of computations" 1 !cnt

  let test_cache_bind_p () =
    let cnt = ref 0 in
    Monad.run (
      lookup cache key2 (compute_data_p cnt) >>= fun v1 ->
      lookup cache key2 (compute_data_p cnt) >>= fun v2 ->
      lookup cache key2 (compute_data_p cnt) >>= fun v3 ->
      assert_equal ~msg:"cache value1" (C.Result.OK val_p) v1;
      assert_equal ~msg:"cache value2" (C.Result.OK val_p) v2;
      assert_equal ~msg:"cache value3" (C.Result.OK val_p) v3;
      assert_equal_int ~msg:"number of computations" 1 !cnt;
      return ()
    )

  let test_cache_size n () =
    let cnt = ref 0 in
    let c = C.create n in
    Monad.run (
      let prev = ref (return (C.Result.OK 0.)) in
      for i = 1 to n+1 do
        let key = string_of_int i in
        prev := !prev >>= fun _ -> lookup c key (compute_data cnt)
      done;
      !prev >>= fun _ ->
      (* clear values from weak buffer *)
      Gc.minor ();
      assert_equal ~msg:"computations" (n+1) !cnt;
      C.get c ~notfound:Not_found (string_of_int 2) >>= fun second ->
      assert_equal ~msg:"still in cache" (C.Result.OK val_p) second;
      C.get c ~notfound:Not_found (string_of_int 1) >>= fun first ->
      assert_equal ~msg:"not in cache anymore" (C.Result.Error Not_found) first;
      return ()
    )

  let tests = Monad.name >:::
    [
      "Cache.bind seq" >:: test_cache_bind_seq;
      "Cache.bind parallel" >:: test_cache_bind_p;
      "Cache size" >:::
        Array.to_list (Array.init 8 (fun i ->
          let i = i + 2 in
          string_of_int i >:: test_cache_size i
        ))
      (* TODO: more tests *)
    ]

  let was_successful results =
    List.for_all (function
        | RSuccess _ | RSkip _ -> true
        | RError _ | RFailure _ | RTodo _ -> false
      ) results

  let run () =
    if not (was_successful (run_test_tt_main tests)) then
      exit 1
end
