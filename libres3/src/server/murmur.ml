open EndianString

let int64_of_i32 h1 h2=
  Int64.(logor (shift_left (of_int32 h1) 32)
           (logand (of_int32 h2) 0xffffffffL))

open Int32

let m = 0x5bd1e995l

let mix data i h =
  let k = mul (LittleEndian.get_int32 data i) m in
  let k = mul (logxor k (shift_right_logical k 24)) m in
  logxor (mul h m) k

let xor_shift h data i shift =
  logxor h (shift_left (of_int (LittleEndian.get_uint8 data i)) shift)

let rec loop data i h1 h2 =
  let len = String.length data - i in
  if len >= 8 then loop data (i+8) (mix data i h1) (mix data (i+4) h2)
  else if len >= 4 then loop data (i+4) (mix data i h1) h2
  else
    let h2 = if len == 3 then xor_shift h2 data (i+2) 16 else h2 in
    let h2 = if len >= 2 then xor_shift h2 data (i+1) 8 else h2 in
    let h2 = if len >= 1 then mul (xor_shift h2 data i 0) m else h2 in
    let h1 = mul (logxor h1 (shift_right_logical h2 18)) m in
    let h2 = mul (logxor h2 (shift_right_logical h1 22)) m in
    let h1 = mul (logxor h1 (shift_right_logical h2 17)) m in
    let h2 = mul (logxor h2 (shift_right_logical h1 19)) m in
    int64_of_i32 h1 h2

let murmurhash64b data seed =
  let h1 = logxor seed (Int32.of_int (String.length data)) in
  loop data 0 h1 0l

