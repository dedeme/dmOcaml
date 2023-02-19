(* Copyright 18-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

open Libkut

let run () =
  Test.eq (B64.decode (B64.encode "")) "";
  Test.eqi (Bytes.length (B64.decode_bytes (B64.encode_bytes Bytes.empty))) 0;

  Test.eq (B64.decode (B64.encode "¿Vió un cañón?")) "¿Vió un cañón?";
  let bs = Bytes.create 4 in
  Bytes.set bs 0 (char_of_int 1);
  Bytes.set bs 1 (char_of_int 255);
  Bytes.set bs 2 (char_of_int 0);
  Bytes.set bs 3 (char_of_int 11);
  Test.eq
    (Bytes.to_string (B64.decode_bytes (B64.encode_bytes bs)))
    (Bytes.to_string bs)
