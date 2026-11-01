(* SHA-256, used to report ACL password digests instead of cleartext.

   Redis never returns a configured [requirepass] value. [ACL GETUSER] reports
   SHA-256 hashes, so this module implements the same digest locally and keeps
   the project dependency-free. *)

let round_constants =
  [|
    0x428a2f98l;
    0x71374491l;
    0xb5c0fbcfl;
    0xe9b5dba5l;
    0x3956c25bl;
    0x59f111f1l;
    0x923f82a4l;
    0xab1c5ed5l;
    0xd807aa98l;
    0x12835b01l;
    0x243185bel;
    0x550c7dc3l;
    0x72be5d74l;
    0x80deb1fel;
    0x9bdc06a7l;
    0xc19bf174l;
    0xe49b69c1l;
    0xefbe4786l;
    0x0fc19dc6l;
    0x240ca1ccl;
    0x2de92c6fl;
    0x4a7484aal;
    0x5cb0a9dcl;
    0x76f988dal;
    0x983e5152l;
    0xa831c66dl;
    0xb00327c8l;
    0xbf597fc7l;
    0xc6e00bf3l;
    0xd5a79147l;
    0x06ca6351l;
    0x14292967l;
    0x27b70a85l;
    0x2e1b2138l;
    0x4d2c6dfcl;
    0x53380d13l;
    0x650a7354l;
    0x766a0abbl;
    0x81c2c92el;
    0x92722c85l;
    0xa2bfe8a1l;
    0xa81a664bl;
    0xc24b8b70l;
    0xc76c51a3l;
    0xd192e819l;
    0xd6990624l;
    0xf40e3585l;
    0x106aa070l;
    0x19a4c116l;
    0x1e376c08l;
    0x2748774cl;
    0x34b0bcb5l;
    0x391c0cb3l;
    0x4ed8aa4al;
    0x5b9cca4fl;
    0x682e6ff3l;
    0x748f82eel;
    0x78a5636fl;
    0x84c87814l;
    0x8cc70208l;
    0x90befffal;
    0xa4506cebl;
    0xbef9a3f7l;
    0xc67178f2l;
  |]

let initial_state =
  [|
    0x6a09e667l;
    0xbb67ae85l;
    0x3c6ef372l;
    0xa54ff53al;
    0x510e527fl;
    0x9b05688cl;
    0x1f83d9abl;
    0x5be0cd19l;
  |]

let rotate_right value bits =
  Int32.logor
    (Int32.shift_right_logical value bits)
    (Int32.shift_left value (32 - bits))

let big_endian_word input offset =
  let byte index =
    Int32.of_int (Char.code (Bytes.get input (offset + index)))
  in
  Int32.logor
    (Int32.shift_left (byte 0) 24)
    (Int32.logor
       (Int32.shift_left (byte 1) 16)
       (Int32.logor (Int32.shift_left (byte 2) 8) (byte 3)))

let pad message =
  let length = String.length message in
  let padded_length = (length + 9 + 63) / 64 * 64 in
  let padded = Bytes.make padded_length '\000' in
  Bytes.blit_string message 0 padded 0 length;
  Bytes.set padded length '\x80';
  let bit_length = Int64.mul (Int64.of_int length) 8L in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    let byte =
      Int64.logand (Int64.shift_right_logical bit_length shift) 0xFFL
      |> Int64.to_int
    in
    Bytes.set padded (padded_length - 8 + index) (Char.chr byte)
  done;
  padded

let digest message =
  let padded = pad message in
  let state = Array.copy initial_state in
  let schedule = Array.make 64 0l in
  for block = 0 to (Bytes.length padded / 64) - 1 do
    let base = block * 64 in
    for index = 0 to 15 do
      schedule.(index) <- big_endian_word padded (base + (index * 4))
    done;
    for index = 16 to 63 do
      let previous_15 = schedule.(index - 15) in
      let previous_2 = schedule.(index - 2) in
      let sigma0 =
        Int32.logxor
          (Int32.logxor
             (rotate_right previous_15 7)
             (rotate_right previous_15 18))
          (Int32.shift_right_logical previous_15 3)
      in
      let sigma1 =
        Int32.logxor
          (Int32.logxor
             (rotate_right previous_2 17)
             (rotate_right previous_2 19))
          (Int32.shift_right_logical previous_2 10)
      in
      schedule.(index) <-
        Int32.add
          (Int32.add schedule.(index - 16) sigma0)
          (Int32.add schedule.(index - 7) sigma1)
    done;
    let a = ref state.(0) in
    let b = ref state.(1) in
    let c = ref state.(2) in
    let d = ref state.(3) in
    let e = ref state.(4) in
    let f = ref state.(5) in
    let g = ref state.(6) in
    let h = ref state.(7) in
    for index = 0 to 63 do
      let sigma1 =
        Int32.logxor
          (Int32.logxor (rotate_right !e 6) (rotate_right !e 11))
          (rotate_right !e 25)
      in
      let choose =
        Int32.logxor (Int32.logand !e !f) (Int32.logand (Int32.lognot !e) !g)
      in
      let first =
        Int32.add
          (Int32.add (Int32.add !h sigma1)
             (Int32.add choose round_constants.(index)))
          schedule.(index)
      in
      let sigma0 =
        Int32.logxor
          (Int32.logxor (rotate_right !a 2) (rotate_right !a 13))
          (rotate_right !a 22)
      in
      let majority =
        Int32.logxor
          (Int32.logxor (Int32.logand !a !b) (Int32.logand !a !c))
          (Int32.logand !b !c)
      in
      let second = Int32.add sigma0 majority in
      h := !g;
      g := !f;
      f := !e;
      e := Int32.add !d first;
      d := !c;
      c := !b;
      b := !a;
      a := Int32.add first second
    done;
    state.(0) <- Int32.add state.(0) !a;
    state.(1) <- Int32.add state.(1) !b;
    state.(2) <- Int32.add state.(2) !c;
    state.(3) <- Int32.add state.(3) !d;
    state.(4) <- Int32.add state.(4) !e;
    state.(5) <- Int32.add state.(5) !f;
    state.(6) <- Int32.add state.(6) !g;
    state.(7) <- Int32.add state.(7) !h
  done;
  let output = Bytes.create 32 in
  Array.iteri
    (fun index word ->
      for byte_index = 0 to 3 do
        let shift = (3 - byte_index) * 8 in
        let byte =
          Int32.logand (Int32.shift_right_logical word shift) 0xFFl
          |> Int32.to_int
        in
        Bytes.set output ((index * 4) + byte_index) (Char.chr byte)
      done)
    state;
  output

let hex message =
  let bytes = digest message in
  let buffer = Buffer.create (Bytes.length bytes * 2) in
  Bytes.iter
    (fun byte ->
      Buffer.add_string buffer (Printf.sprintf "%02x" (Char.code byte)))
    bytes;
  Buffer.contents buffer
