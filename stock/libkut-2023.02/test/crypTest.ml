(* Copyright 18-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

open Libkut

let run () =
  Random.self_init ();
  Test.eqi (String.length (Cryp.genk 5)) 5;
  let k = Cryp.key "abc" 8 in
  Test.eq k "C8vYu4C/";

  let code = Cryp.encode k "El cañón disparó" in
  assert (code <> "El cañón disparó");
  Test.eq (Cryp.decode k code) "El cañón disparó";

  Test.eq (Cryp.decode k "") "";
  Test.eq (Cryp.encode k "") ""
