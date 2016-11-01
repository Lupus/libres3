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

open CalendarLib
open Cohttp
type t = Calendar.t

let imf_fixdate = "%a, %d %b %Y %H:%M:%S GMT"
let rfc850 = "%A, %d-%b-%y %H:%M:%S GMT"
let asctime = "%c"

let of_string s =
  try Printer.Calendar.from_fstring imf_fixdate  s
  with Invalid_argument _ ->
  try Printer.Calendar.from_fstring rfc850 s
  with Invalid_argument _ -> Printer.Calendar.from_fstring asctime s

let to_string d =
  Printer.Calendar.sprint imf_fixdate d

let of_unix_timestamp f = Calendar.from_unixfloat f
let to_unix_timestamp d = floor (Calendar.to_unixfloat d +. 0.5)

let add_header v h = Header.add h "Date" (to_string v)

let of_header ?(field="Date") h = match Header.get h field with
| None -> None
| Some v -> try Some (of_string v) with Invalid_argument _ -> None

let pp ppf v = Printer.Calendar.fprint "%FT%TZ" ppf v
