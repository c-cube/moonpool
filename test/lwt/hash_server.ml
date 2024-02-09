(* vendored from https://github.com/dbuenzli/uuidm *)

let sha_1 s =
  (* Based on pseudo-code of RFC 3174. Slow and ugly but does the job. *)
  let sha_1_pad s =
    let len = String.length s in
    let blen = 8 * len in
    let rem = len mod 64 in
    let mlen =
      if rem > 55 then
        len + 128 - rem
      else
        len + 64 - rem
    in
    let m = Bytes.create mlen in
    Bytes.blit_string s 0 m 0 len;
    Bytes.fill m len (mlen - len) '\x00';
    Bytes.set m len '\x80';
    if Sys.word_size > 32 then (
      Bytes.set m (mlen - 8) (Char.unsafe_chr ((blen lsr 56) land 0xFF));
      Bytes.set m (mlen - 7) (Char.unsafe_chr ((blen lsr 48) land 0xFF));
      Bytes.set m (mlen - 6) (Char.unsafe_chr ((blen lsr 40) land 0xFF));
      Bytes.set m (mlen - 5) (Char.unsafe_chr ((blen lsr 32) land 0xFF))
    );
    Bytes.set m (mlen - 4) (Char.unsafe_chr ((blen lsr 24) land 0xFF));
    Bytes.set m (mlen - 3) (Char.unsafe_chr ((blen lsr 16) land 0xFF));
    Bytes.set m (mlen - 2) (Char.unsafe_chr ((blen lsr 8) land 0xFF));
    Bytes.set m (mlen - 1) (Char.unsafe_chr (blen land 0xFF));
    m
  in
  (* Operations on int32 *)
  let ( &&& ) = ( land ) in
  let ( lor ) = Int32.logor in
  let ( lxor ) = Int32.logxor in
  let ( land ) = Int32.logand in
  let ( ++ ) = Int32.add in
  let lnot = Int32.lognot in
  let sr = Int32.shift_right in
  let sl = Int32.shift_left in
  let cls n x = sl x n lor Int32.shift_right_logical x (32 - n) in
  (* Start *)
  let m = sha_1_pad s in
  let w = Array.make 16 0l in
  let h0 = ref 0x67452301l in
  let h1 = ref 0xEFCDAB89l in
  let h2 = ref 0x98BADCFEl in
  let h3 = ref 0x10325476l in
  let h4 = ref 0xC3D2E1F0l in
  let a = ref 0l in
  let b = ref 0l in
  let c = ref 0l in
  let d = ref 0l in
  let e = ref 0l in
  for i = 0 to (Bytes.length m / 64) - 1 do
    (* For each block *)
    (* Fill w *)
    let base = i * 64 in
    for j = 0 to 15 do
      let k = base + (j * 4) in
      w.(j) <-
        sl (Int32.of_int (Char.code @@ Bytes.get m k)) 24
        lor sl (Int32.of_int (Char.code @@ Bytes.get m (k + 1))) 16
        lor sl (Int32.of_int (Char.code @@ Bytes.get m (k + 2))) 8
        lor Int32.of_int (Char.code @@ Bytes.get m (k + 3))
    done;
    (* Loop *)
    a := !h0;
    b := !h1;
    c := !h2;
    d := !h3;
    e := !h4;
    for t = 0 to 79 do
      let f, k =
        if t <= 19 then
          !b land !c lor (lnot !b land !d), 0x5A827999l
        else if t <= 39 then
          !b lxor !c lxor !d, 0x6ED9EBA1l
        else if t <= 59 then
          !b land !c lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
        else
          !b lxor !c lxor !d, 0xCA62C1D6l
      in
      let s = t &&& 0xF in
      if t >= 16 then
        w.(s) <-
          cls 1
            (w.(s + 13 &&& 0xF)
            lxor w.(s + 8 &&& 0xF)
            lxor w.(s + 2 &&& 0xF)
            lxor w.(s));
      let temp = cls 5 !a ++ f ++ !e ++ w.(s) ++ k in
      e := !d;
      d := !c;
      c := cls 30 !b;
      b := !a;
      a := temp
    done;
    (* Update *)
    h0 := !h0 ++ !a;
    h1 := !h1 ++ !b;
    h2 := !h2 ++ !c;
    h3 := !h3 ++ !d;
    h4 := !h4 ++ !e
  done;
  let h = Bytes.create 20 in
  let i2s h k i =
    Bytes.set h k (Char.unsafe_chr (Int32.to_int (sr i 24) &&& 0xFF));
    Bytes.set h (k + 1) (Char.unsafe_chr (Int32.to_int (sr i 16) &&& 0xFF));
    Bytes.set h (k + 2) (Char.unsafe_chr (Int32.to_int (sr i 8) &&& 0xFF));
    Bytes.set h (k + 3) (Char.unsafe_chr (Int32.to_int i &&& 0xFF))
  in
  i2s h 0 !h0;
  i2s h 4 !h1;
  i2s h 8 !h2;
  i2s h 12 !h3;
  i2s h 16 !h4;
  Bytes.unsafe_to_string h

(*---------------------------------------------------------------------------
   Copyright (c) 2008 The uuidm programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

(* server that reads from sockets lists of files, and returns hashes of these files *)

module M = Moonpool
module M_lwt = Moonpool_lwt
module Trace = Trace_core

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let to_hex s =
  let i2h i = String.get (spf "%x" i) 0 in
  let n = String.length s in
  let bs = Bytes.create (n * 2) in
  for i = 0 to n - 1 do
    Bytes.set bs (2 * i) (i2h ((Char.code s.[i] land 0b1111_0000) lsr 4));
    Bytes.set bs ((2 * i) + 1) (i2h (Char.code s.[i] land 0b0000_1111))
  done;
  Bytes.unsafe_to_string bs

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

[@@@ocaml.warning "-48"]

let read_file filename : string =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "read-file" ~data:(fun () ->
        [ "f", `String filename ])
  in
  In_channel.with_open_bin filename In_channel.input_all

let main ~port ~runner () : unit Lwt.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let lwt_fut, _lwt_prom = Lwt.wait () in

  (* TODO: handle exit?? *)
  Printf.printf "listening on port %d\n%!" port;

  let handle_client client_addr ic oc =
    let _sp =
      Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "handle.client"
        ~data:(fun () -> [ "addr", `String (str_of_sockaddr client_addr) ])
    in

    try
      while true do
        Trace.message "read";
        let filename =
          M_lwt.run_in_lwt_and_await (fun () -> Lwt_io.read_line ic)
          |> String.trim
        in
        Trace.messagef (fun k -> k "hash %S" filename);

        match read_file filename with
        | exception e ->
          Printf.eprintf "error while reading %S:\n%s\n" filename
            (Printexc.to_string e);
          M_lwt.run_in_lwt_and_await (fun () ->
              Lwt_io.write_line oc (spf "%s: error" filename));
          M_lwt.run_in_lwt_and_await (fun () -> Lwt_io.flush oc)
        | content ->
          (* got the content, now hash it *)
          let hash =
            let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "hash" in
            sha_1 content |> to_hex
          in

          M_lwt.run_in_lwt_and_await (fun () ->
              Lwt_io.write_line oc (spf "%s: %s" filename hash));
          M_lwt.run_in_lwt_and_await (fun () -> Lwt_io.flush oc)
      done
    with End_of_file | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
      Trace.exit_manual_span _sp;
      Trace.message "exit handle client"
  in

  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let _server = M_lwt.TCP_server.establish' ~runner addr handle_client in

  lwt_fut

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let port = ref 1234 in
  let j = ref 4 in

  let opts =
    [
      "-p", Arg.Set_int port, " port"; "-j", Arg.Set_int j, " number of threads";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo server";

  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:!j () in
  Lwt_engine.set @@ new Lwt_engine.libev ();
  Lwt_main.run @@ main ~runner ~port:!port ()
