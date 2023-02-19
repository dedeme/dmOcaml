(* Copyright 14-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

(** Array container.

[Arr] is a reference to [array]. There are the following literal operations:
  - Construction: [let a = ref [|1; 2|] in ...].
  - Indexing: [let e = !a.(i)] and [!a.(i) <- e].

{{!add} add} | {{!all} all} | {{!any} any} | {{!cat} cat} | {{!clear} clear} | {{!concat} concat} | {{!copy} copy} | {{!drop} drop} | {{!drop_while} drop_while} | {{!duplicates} duplicates} | {{!each} each} | {{!each_ix} each_ix} | {{!empty} empty} | {{!eq} eq} | {{!filter} filter} | {{!filter_in} filter_in} | {{!find} find} | {{!get} get} | {{!index} index} | {{!join} join} | {{!map} map} | {{!mk} mk} | {{!peek} peek} | {{!pop} pop} | {{!push} push} | {{!reduce} reduce} | {{!reverse} reverse} | {{!reverse_in} reverse_in} | {{!set} set} | {{!shift} shift} | {{!shuffle} shuffle} | {{!size} size} | {{!take} take} | {{!take_while} take_while} | {{!to_str} to_str} | {{!unshift} unshift}

*)

(** Array type. *)
type 'a t = 'a array ref

(** [add a1 a2] returns a new array appending [a2] to [a1]. *)
let add (a1 : 'a t) (a2 : 'a t) : 'a t = ref (Array.append !a1 !a2)

(** [all a fn] returns [true] if [fn] returns [true] with every element of [a] *)
let all (a : 'a t) fn = Array.for_all fn !a

(** [any a fn] returns [true] if [fn] returns [true] with al least one element of [a] *)
let any (a : 'a t) fn = Array.exists fn !a

(** [cat a1 a2] appends [a2] to [a1] in place. *)
let cat (a : 'a t) (b : 'a t) = a := Array.append !a !b

(** [clear a] remove every element of [a]. *)
let clear (a : 'a t) = a := [||]

(** [concat arrs] returns a new array adding all the subarrays of [arrs].*)
let concat (arrs : 'a t t) : 'a t =
  ref (Array.fold_left (fun r e -> Array.append r !e) [||] !arrs)

(** [copy a] returns a shalow copy of [a]. *)
let copy (a : 'a t) : 'a t = ref (Array.copy !a)

(** [index a fn] returns the index of the firt element [e] of [a] such that
    [fn e] returns [true].
*)
let index (a : 'a t) fn =
  let r = ref (-1) in
  let continue = ref true in
  let i = ref 0 in
  let ar = !a in
  let len = Array.length ar in
  while !continue do
    if !i == len then continue := false
    else if fn ar.(!i) then (
      r := !i;
      continue := false)
    else i := !i + 1
  done;
  !r

(** [drop a i] returns the elements of [a] from [!a.(i)] (inclusive) to the end.

    - if [i < 0] returns all the elements.
    - If [i > size a] returns an empty array.
*)
let drop (a : 'a t) i : 'a t =
  let ar = !a in
  if i < 0 then ref (Array.copy ar)
  else
    let len = Array.length ar in
    if i >= len then ref [||] else ref (Array.sub ar i (len - i))

(** [drop_while a fn] returns the elements of [a] from the first element, such
    that [fn e] returns false (inclusive), to the end.
*)
let drop_while (a : 'a t) fn : 'a t =
  let i = index a (fun e -> not (fn e)) in
  if i < 0 then ref [||] else drop a i

(** [each a fn] runs [fn e] whit each element of [a]. *)
let each (a : 'a t) fn = Array.iter fn !a

(** [each a fn] runs [fn e i] whit each element of [a] and its index. *)
let each_ix (a : 'a t) fn = Array.iteri (fun i e -> fn e i) !a

(** [empty a ] returns [true] if [a] is empty. *)
let empty (a : 'a t) = Array.length !a = 0

(** [eq a1 a2 fn] returns [true] if [size a1 = size a2] and for each index
    [i] results [fn !a1.(i) !a2.(i) = true].

    [eq a1 a2 (=)] give the same result as [a1 = a2].
*)
let eq (a1 : 'a t) (a2 : 'a t) fn =
  let aa1 = !a1 in
  let aa2 = !a2 in
  let l1 = Array.length aa1 in
  let l2 = Array.length aa2 in
  if l1 != l2 then false
  else
    let r = ref true in
    let continue = ref true in
    let i = ref 0 in
    while !continue do
      if !i == l1 then continue := false
      else if not (fn aa1.(!i) aa2.(!i)) then (
        r := false;
        continue := false)
      else i := !i + 1
    done;
    !r

(** [filter_in a fn] filters in place those elements such that [fn e = true]. *)
let filter_in (a : 'a t) fn =
  let aa = !a in
  let ix = ref 0 in
  for i = 0 to Array.length aa - 1 do
    if fn aa.(i) then (
      aa.(!ix) <- aa.(i);
      ix := !ix + 1)
  done;
  a := Array.sub aa 0 !ix

(** [filter a fn] filters in a new array those elements of [a] such that
    [fn e = true].
*)
let filter (a : 'a t) fn =
  let a2 = copy a in
  filter_in a2 fn;
  a2

(** [find a fn] returns [Some e] with the first elements such that [fn e = true]
    or [None] if no elements satisfais the function.
*)
let find (a : 'a t) fn =
  let ix = index a fn in
  if ix == -1 then None else Some !a.(ix)

(** [get a i] returns the element in position i. Also can be used [!a.(i)]. *)
let get (a : 'a t) i = !a.(i)

(** [join a s] returns a string joinning elements of [a] with [s] *)
let join (a : string t) s =
  Array.fold_left (fun r e -> if r == "" then e else r ^ s ^ e) "" !a

(** [map a fn] returns a new array with every element of [a] modified by [fn] *)
let map (a : 'a t) fn : 'b t = ref (Array.map fn !a)

(** [mk n v] returns an array with [n] elements with the value [v] *)
let mk n v : 'a t = ref (Array.make n v)

(** [of_list] returns a list with the elements of [l] *)
let of_list (l : 'a list) = ref (Array.of_list l)

(** [peek a] returns the last element of [a].

    Raises [Invalid_argument] if [a] is empty.
*)
let peek (a : 'a t) = !a.(Array.length !a - 1)

(** [pop a] removes and returns the last element of [a]. *)
let pop (a : 'a t) =
  let len1 = Array.length !a - 1 in
  let e = !a.(len1) in
  a := Array.init len1 (fun i -> !a.(i));
  e

(** [push a e] adds [e] at the end of [a]. *)
let push (a : 'a t) e = a := Array.append !a [| e |]

(** [duplicates a fn] returns two arrays using the 'equals' function [fn]:

    - First not duplicated elements.
    - The other (duplicates) elements. This array can have duplicates.
*)
let duplicates (a : 'a t) fn : 'a t * 'a t =
  let els = ref [||] in
  let dup = ref [||] in
  each a (fun e ->
      if any els (fun e2 -> fn e e2) then push dup e else push els e);
  (els, dup)

(** [reduce a seed fn] returns the result of applying [fn] successively
    [f ... (f (f seed !a.(0)) !a.(1)) ... !a.(size a - 1)].

    If [a] is empty it returns [seed].
*)
let reduce (a : 'a t) seed fn =
  let aa = !a in
  let r = ref seed in
  for i = 0 to Array.length aa - 1 do
    r := fn !r aa.(i)
  done;
  !r

(** [reverse_in a] reverses in place the order of [a] elements. *)
let reverse_in (a : 'a t) =
  let aa = !a in
  let len = ref (Array.length aa) in
  let limit = !len / 2 in
  for i = 0 to limit - 1 do
    len := !len - 1;
    let tmp = aa.(i) in
    aa.(i) <- aa.(!len);
    aa.(!len) <- tmp
  done

(** [reverse a] returns a new array with elements of [a] reversed. *)
let reverse (a : 'a t) =
  let a2 = copy a in
  reverse_in a2;
  a2

(** [set a i e] sets the element of [a] in position [i].
    Also can be used [!a.(i) <- e]. *)
let set (a : 'a t) i e = !a.(i) <- e

(** [shift a] removes and returns the first element of [a]. *)
let shift (a : 'a t) =
  let len1 = Array.length !a - 1 in
  let e = !a.(0) in
  a := Array.init len1 (fun i -> !a.(i + 1));
  e

(** [size a] return array length. *)
let size (a : 'a t) = Array.length !a

(** [shuffle a] randomly reorders elements of [a] in place. *)
let shuffle (a : 'a t) =
  let aa = !a in
  for i = Array.length aa downto 1 do
    let n = Random.int i in
    let i1 = i - 1 in
    let tmp = aa.(n) in
    aa.(n) <- aa.(i1);
    aa.(i1) <- tmp
  done

(** [take a i] returns the first [i] elements of [a].

    - if [i < 0] returns an empty array.
    - If [i > size a] returns all the elements.
*)
let take (a : 'a t) i : 'a t =
  if i < 0 then ref [||]
  else
    let ar = !a in
    let len = Array.length ar in
    if i >= len then ref (Array.copy ar) else ref (Array.sub ar 0 i)

(** [take a fn] returns the intial elements of [a] until [fn e] returns [false]
    (exclusive).
*)
let take_while (a : 'a t) fn : 'a t =
  let i = index a (fun e -> not (fn e)) in
  if i < 0 then a else take a i

(** [to_list a] returns a list with the elements of [a]. *)
let to_list (a : 'a t) = Array.to_list !a

(** [toStr a fn] returns a string representation of [a] using [fn] to
    convert [a] elements to string. *)
let to_str (a : 'a t) (fn : 'a -> string) = "[" ^ join (map a fn) "," ^ "]"

(** [unshift a e] adds [e] at the beginning of [a]. *)
let unshift (a : 'a t) e = a := Array.append [| e |] !a
