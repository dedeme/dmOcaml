(* Copyright 14-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

(** Test utilities

{{!eqf} eqf} | {{!eqi} eqi} | {{!eq} eq}

*)

(** Tests strings
      - actual  : Calculated value.
      - expected: Expected value.
*)
let eq actual expected =
  let show actual expected =
    print_endline ("Expected: " ^ expected);
    print_endline ("  Actual: " ^ actual);
    Printexc.record_backtrace true
  in
  if actual <> expected then (
    show actual expected;
    assert false)

(** Tests ints
      - actual  : Calculated value.
      - expected: Expected value.
*)
let eqi actual expected = eq (string_of_int actual) (string_of_int expected)

(** Tests floats
      - actual  : Calculated value.
      - expected: Expected value.
*)
let eqf actual expected = eq (string_of_float actual) (string_of_float expected)
