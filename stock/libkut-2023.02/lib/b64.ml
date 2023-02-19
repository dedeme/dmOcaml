(* Copyright 17-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

(** Encode - decode in base 64

{{!decode} decode} | {{!decode_bytes} decode_bytes} | {{!encode} encode} | {{!encode_bytes} encode_bytes}

*)

(** [decode b64] decodes a base 64 string.

    Raise Invalid_argument if b64 is no a valid encoding.
*)
let decode b64 = Base64.decode_exn b64

(** [decode_bytes b64] decodes a base 64 string.

    Raise Invalid_argument if b64 is no a valid encoding.
*)
let decode_bytes b64 = Bytes.of_string (decode b64)

(** [encode s] encodes [s] in base 64. *)
let encode s = Base64.encode_exn s

(** [encode_bytes bs] encodes [bs] in base 64. *)
let encode_bytes bs = encode (Bytes.to_string bs)
