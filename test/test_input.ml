(**************************************************************************)
(*  Copyright (C) 2012-2016, Skylable Ltd. <info-copyright@skylable.com>  *)
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

(* Tests that we accept valid inputs.
   Obviously we cannot test all possible inputs, but we can test:
    - all possible ASCII chars
    - UTF-8 chars of different length
    - integers at boundaries
    - items/strings at/near length limit
    - qcheck-based randomized testing (to be re-evaluated)
    - long lists - check against stack overflow with small stack size (or large lists)
*)
