(* Copyright 18-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

(** Encription utilities.

{{!decode} decode} | {{!encode} encode} | {{!genk} genk} | {{!key} key}

*)

(** [genk len] generates a b64 random key of a length [len]. *)
let genk len =
  String.sub
    (B64.encode (String.init len (fun _ -> char_of_int (Random.int 256))))
    0 len

(** [key s len] encodes [s] in irreversible way with [lenght] base 64
    characters.
*)
let key s len =
  let k = s ^ "codified in irreversibleDeme is good, very good!\n\r8@@" in
  let lenk = String.length k in
  let sum = ref 0 in
  for i = 0 to lenk - 1 do
    sum := !sum + int_of_char k.[i]
  done;
  let len2 = lenk + len in
  let r0 = String.make len2 ' ' in
  let r = Bytes.of_string r0 in
  let r1 = Bytes.of_string r0 in
  let r2 = Bytes.of_string r0 in
  let ik = ref 0 in
  for i = 0 to len2 - 1 do
    let v1 = int_of_char k.[!ik] in
    let v2 = v1 + int_of_char k.[v1 mod lenk] in
    let v3 = v2 + int_of_char k.[v2 mod lenk] in
    let v4 = v3 + int_of_char k.[v3 mod lenk] in
    sum := (!sum + i + v4) land 255;
    let ch = char_of_int !sum in
    Bytes.set r1 i ch;
    Bytes.set r2 i ch;
    ik := if !ik = lenk - 1 then 0 else !ik + 1
  done;

  for i = 0 to len2 - 1 do
    let v1 = int_of_char (Bytes.get r2 i) in
    let v2 = v1 + int_of_char (Bytes.get r2 (v1 mod len2)) in
    let v3 = v2 + int_of_char (Bytes.get r2 (v2 mod len2)) in
    let v4 = v3 + int_of_char (Bytes.get r2 (v3 mod len2)) in
    sum := (!sum + v4) land 255;
    Bytes.set r2 i (char_of_int !sum);
    Bytes.set r i (char_of_int ((!sum + int_of_char (Bytes.get r1 i)) land 255))
  done;

  String.sub (B64.encode_bytes r) 0 len

(** [encode k msg] encodes [msg] using the key [k]. *)
let encode k msg =
  let m = B64.encode msg in
  let len = String.length m in
  let k2 = key k len in
  let mb = Bytes.of_string m in
  let kb = Bytes.of_string k2 in
  let r = Bytes.of_string m in
  for i = 0 to len - 1 do
    Bytes.set r i
      (char_of_int
         (int_of_char (Bytes.get mb i) + int_of_char (Bytes.get kb i)))
  done;
  B64.encode_bytes r

(** [decode k code] decodes [code] using the key [k].

    [code] was codified with [encode].

    Raise Raise Invalid_argument if [code] is not valid.
*)
let decode k code =
  let mb = B64.decode_bytes code in
  let len = Bytes.length mb in
  let k2 = key k len in
  let kb = Bytes.of_string k2 in
  let r = Bytes.of_string k2 in
  for i = 0 to len - 1 do
    Bytes.set r i
      (char_of_int
         (int_of_char (Bytes.get mb i) - int_of_char (Bytes.get kb i)))
  done;
  B64.decode (Bytes.to_string r)
