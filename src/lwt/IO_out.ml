open Common_

class type t = object
  method output_char : char -> unit
  method output : bytes -> int -> int -> unit
  method flush : unit -> unit
  method close : unit -> unit
end

let create ?(flush = ignore) ?(close = ignore) ~output_char ~output () : t =
  object
    method flush () = flush ()
    method close () = close ()
    method output_char c = output_char c
    method output bs i len = output bs i len
  end

let dummy : t =
  object
    method flush () = ()
    method close () = ()
    method output_char _ = ()
    method output _ _ _ = ()
  end

let of_unix_fd ?(close_noerr = false) ?(buf = Bytes.create _default_buf_size) fd
    : t =
  let buf_off = ref 0 in

  let[@inline] is_full () = !buf_off = Bytes.length buf in

  let flush () =
    if !buf_off > 0 then (
      IO.write fd buf 0 !buf_off;
      buf_off := 0
    )
  in

  object
    method output_char c =
      if is_full () then flush ();
      Bytes.set buf !buf_off c;
      incr buf_off

    method output bs i len : unit =
      let i = ref i in
      let len = ref len in

      while !len > 0 do
        (* make space *)
        if is_full () then flush ();

        let n = min !len (Bytes.length buf - !buf_off) in
        Bytes.blit bs !i buf !buf_off n;
        buf_off := !buf_off + n;
        i := !i + n;
        len := !len - n
      done;
      (* if full, write eagerly *)
      if is_full () then flush ()

    method close () =
      if close_noerr then (
        try
          flush ();
          Unix.close fd
        with _ -> ()
      ) else (
        flush ();
        Unix.close fd
      )

    method flush = flush
  end

let of_buffer (buf : Buffer.t) : t =
  object
    method close () = ()
    method flush () = ()
    method output_char c = Buffer.add_char buf c
    method output bs i len = Buffer.add_subbytes buf bs i len
  end

(** Output the buffer slice into this channel *)
let[@inline] output_char (self : #t) c : unit = self#output_char c

(** Output the buffer slice into this channel *)
let[@inline] output (self : #t) buf i len : unit = self#output buf i len

let[@inline] output_string (self : #t) (str : string) : unit =
  self#output (Bytes.unsafe_of_string str) 0 (String.length str)

let output_line (self : #t) (str : string) : unit =
  output_string self str;
  output_char self '\n'

(** Close the channel. *)
let[@inline] close self : unit = self#close ()

(** Flush (ie. force write) any buffered bytes. *)
let[@inline] flush self : unit = self#flush ()

let output_int self i =
  let s = string_of_int i in
  output_string self s

let output_lines self seq = Seq.iter (output_line self) seq

let tee (l : t list) : t =
  match l with
  | [] -> dummy
  | [ oc ] -> oc
  | _ ->
    let output bs i len = List.iter (fun oc -> output oc bs i len) l in
    let output_char c = List.iter (fun oc -> output_char oc c) l in
    let close () = List.iter close l in
    let flush () = List.iter flush l in
    create ~flush ~close ~output ~output_char ()
