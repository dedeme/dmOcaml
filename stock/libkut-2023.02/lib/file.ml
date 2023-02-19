(* Copyright 18-Jan-2023 ºDeme
   GNU General Public License - V3 <http://www.gnu.org/licenses/> *)

(** File utilities.

{{!aopen} aopen} | {{!cd} cd} | {{!close} close} | {{!read} read} | {{!read_bytes} read_bytes} | {{!read_chunk} read_chunk} | {{!read_chunks} read_chunks} | {{!read_line} read_line} | {{!read_lines} read_lines} | {{!ropen} ropen} | {{!wopen} wopen} | {{!write} write} | {{!write_bytes} write_bytes} | {{!write_chunk} write_chunk} | {{!write_text} write_text}

*)

(** File descriptor for read and write files *)
type fd_t = Stdlib.in_channel option * Stdlib.out_channel option

(** [aopen path] open a file for appending and returns if file descriptor. *)
let aopen path : fd_t = (None, Some (open_out_gen [ Open_append ] 0o644 path))

(** [ropen path] open a file for reading and returns if file descriptor. *)
let ropen path : fd_t = (Some (open_in path), None)

(** [aopen path] open a file for writing and returns if file descriptor. *)
let wopen path : fd_t = (None, Some (open_out path))

(** [close fd] closes a file wose file descriptor is [fd]. *)
let close (fd : fd_t) =
  match fd with
  | Some ic, _ -> close_in ic
  | None, Some oc -> close_out_noerr oc
  | _ -> raise (Invalid_argument "File descriptor is (None, None)")

(** [read_line fd] reads a line from the read file descriptor [fd]. *)
let read_line (fd : fd_t) =
  match fd with
  | Some ic, _ -> ( try Some (input_line ic) with End_of_file -> None)
  | _ -> raise (Invalid_argument "File descriptor is not an input one")

(** [read_line fd] reads a bytes chunk of a maximum of [buf] bytes from the
    read file descriptor [fd].
*)
let read_chunk (fd : fd_t) buf =
  match fd with
  | Some ic, _ -> (
      let bs = Bytes.make buf '0' in
      match input ic bs 0 buf with
      | 0 -> Bytes.empty
      | n -> if n = buf then bs else Bytes.sub bs 0 n)
  | _ -> raise (Invalid_argument "File descriptor is not an input one")

(** [write_text fd] writes a text to the write file descriptor [fd]. *)
let write_text (fd : fd_t) tx =
  match fd with
  | _, Some oc -> output_string oc tx
  | _ -> raise (Invalid_argument "File descriptor is not an output one")

(** [write_chunk fd] writes a bytes chunk to the write file descriptor [fd]. *)
let write_chunk (fd : fd_t) bs =
  match fd with
  | _, Some oc -> output_bytes oc bs
  | _ -> raise (Invalid_argument "File descriptor is not an output one")

(** [read_chunks path buf fn] applies [fn] to each bytes chunck of [path].

    - path: File path.
    - buf: Length to create a bytes buffer for reading.
    - fn: Function to apply.
*)
let read_chunks path buf fn =
  let rec rd ic bs =
    match input ic bs 0 buf with
    | 0 -> close_in ic
    | n ->
        if n = buf then fn bs else fn (Bytes.sub bs 0 n);
        rd ic bs
  in
  rd (open_in path) (Bytes.make buf '0')

(** [read_bytes path] reads completely the binary file [path]. *)
let read_bytes path =
  let n = ref 0 in
  let lbs = ref [] in
  read_chunks path 8192 (fun bs ->
      n := !n + Bytes.length bs;
      lbs := bs :: !lbs);
  let r = Bytes.make !n '0' in
  let m = ref 0 in
  Arr.each
    (Arr.of_list (List.rev !lbs))
    (fun bs ->
      let len = Bytes.length bs in
      Bytes.blit bs 0 r !m len;
      m := !m + len);
  r

(** [read_lines path fn] applies [fn] to each line of [path]. *)
let read_lines path fn =
  let ic = open_in path in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop () =
    match try_read () with
    | Some s ->
        fn s;
        loop ()
    | None -> close_in ic
  in
  loop ()

(** [read path] reads completely the text file [path]. *)
let read path = Bytes.to_string (read_bytes path)

(** [write path tx] writes in [path] [tx]. If the file exists is overwritten. *)
let write path tx =
  let oc = open_out path in
  output_string oc tx;
  close_out oc

(** [write path tx] writes in [path] [bs]. If the file exists is overwritten. *)
let write_bytes path bs =
  let oc = open_out path in
  output_bytes oc bs;
  close_out oc

(** [cd dir] changes working directory to [dir]. *)
let cd = Unix.chdir
