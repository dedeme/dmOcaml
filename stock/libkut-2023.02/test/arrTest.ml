(* Copyright 14-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

open Libkut

let run () =
  Random.self_init ();

  Test.eqi (Arr.size (ref [||])) 0;
  let a = ref [||] in
  assert (Arr.empty a);
  Arr.push a 1;
  assert (not (Arr.empty a));
  Arr.push a 2;
  Test.eqi (Arr.size a) 2;
  let e = Arr.pop a in
  Test.eqi (Arr.size a) 1;
  Test.eqi e 2;
  Arr.unshift a 22;
  Test.eqi !a.(0) 22;
  !a.(0) <- 20;
  Test.eqi !a.(0) 20;
  let e = Arr.shift a in
  Test.eqi e 20;
  Test.eqi (Arr.size a) 1;
  Arr.push a 2;
  Arr.push a 5;
  Test.eqi !a.(2) 5;
  !a.(2) <- 3;
  Test.eq (Arr.to_str a string_of_int) "[1,2,3]";

  assert (Arr.all (ref [||]) (fun e -> e < 20));
  assert (Arr.all a (fun e -> e < 20));
  assert (not (Arr.all a (fun e -> e < 2)));
  assert (not (Arr.all a (fun e -> e < 0)));

  assert (not (Arr.any (ref [||]) (fun e -> e > 0)));
  assert (Arr.any a (fun e -> e < 20));
  assert (Arr.any a (fun e -> e < 2));
  assert (not (Arr.any a (fun e -> e < 0)));

  let b = ref [| 4; 5 |] in
  let c = Arr.copy a in
  Arr.cat c b;
  assert (c = ref [| 1; 2; 3; 4; 5 |]);
  assert (c = Arr.add a b);
  assert (Arr.eq c (Arr.add a b) ( = ));
  assert (c = Arr.concat (ref [| a; b |]));
  Arr.clear c;
  assert (Arr.size c = 0);
  assert (Arr.eq c (ref [||]) ( = ));
  Test.eqi (Arr.index a (( = ) 23)) (-1);
  Test.eqi (Arr.index a (( = ) 2)) 1;

  assert (Arr.drop a (-1) = a);
  assert (Arr.drop a 0 = a);
  assert (Arr.drop a 1 = ref [| 2; 3 |]);
  assert (Arr.drop a 3 = ref [||]);
  assert (Arr.drop a 25 = ref [||]);

  assert (Arr.drop_while a (fun e -> e = 1) = ref [| 2; 3 |]);
  assert (Arr.drop_while a (fun e -> e = 23) = a);
  assert (Arr.drop_while a (fun e -> e < 23) = ref [||]);

  assert (Arr.take a (-1) = ref [||]);
  assert (Arr.take a 0 = ref [||]);
  assert (Arr.take a 1 = ref [| 1 |]);
  assert (Arr.take a 3 = a);
  assert (Arr.take a 25 = a);

  assert (Arr.take_while a (fun e -> e = 1) = ref [| 1 |]);
  assert (Arr.take_while a (fun e -> e = 23) = ref [||]);
  assert (Arr.take_while a (fun e -> e < 23) = a);

  let sum = ref 0 in
  Arr.each a (fun e -> sum := !sum + e);
  Test.eqi !sum 6;
  sum := 0;
  Arr.each_ix a (fun e _ -> sum := !sum + e);
  Test.eqi !sum 6;
  sum := 0;
  Arr.each_ix a (fun _ i -> sum := !sum + i);
  Test.eqi !sum 3;

  c := [| 1; 2; 4; 1 |];
  let dist, dup = Arr.duplicates (Arr.add a c) ( = ) in
  assert (dist = ref [| 1; 2; 3; 4 |]);
  assert (dup = ref [| 1; 2; 1 |]);

  let c2 = Arr.copy c in
  Arr.filter_in c2 (fun e -> e < 2);
  assert (c2 = ref [| 1; 1 |]);
  assert (c2 = Arr.filter c (fun e -> e < 2));

  Arr.clear c2;
  Arr.filter_in c2 (fun e -> e < 2);
  assert (c2 = ref [||]);
  assert (c2 = Arr.filter c2 (fun e -> e < 2));

  assert (Arr.peek a = 3);

  Test.eqi (Arr.reduce a 0 (fun r e -> r + e)) 6;
  Test.eqi (Arr.reduce (ref [||]) 0 (fun r e -> r + e)) 0;

  c := !(Arr.copy a);
  Arr.reverse_in c;
  assert (c = ref [| 3; 2; 1 |]);
  assert (c = Arr.reverse a);
  assert (a = Arr.reverse c);

  Arr.clear c;
  Arr.reverse_in c;
  assert (c = ref [||]);
  assert (c = Arr.reverse (ref [||]))

(* Manual test
   c := !(Arr.copy a);
   Arr.shuffle c;
   Test.eq (Arr.to_str c string_of_int) "[]";
*)
