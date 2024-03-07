open struct
  let _default_buf_size = 16 * 1024
end

type file_descr = Unix.file_descr

let await_readable fd =
  let loop = U_loop.cur () in
  (* wait for FD to be ready *)
  Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
      ignore
        (loop#on_readable fd (fun ev ->
             wakeup ();
             Cancel_handle.cancel ev)
          : Cancel_handle.t))

let rec read fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.read fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      await_readable fd;
      read fd buf i len
    | n -> n
  )

let await_writable fd =
  let loop = U_loop.cur () in
  (* wait for FD to be ready *)
  Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
      ignore
        (loop#on_writable fd (fun ev ->
             wakeup ();
             Cancel_handle.cancel ev)
          : Cancel_handle.t))

let rec write_once fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.write fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      await_writable fd;
      write_once fd buf i len
    | n -> n
  )

let write fd buf i len : unit =
  let i = ref i in
  let len = ref len in
  while !len > 0 do
    let n = write_once fd buf !i !len in
    i := !i + n;
    len := !len - n
  done

module Out = struct
  include Iostream.Out

  let of_unix_fd ?(close_noerr = false) ?(buf = Bytes.create _default_buf_size)
      fd : t =
    let buf_off = ref 0 in

    let[@inline] is_full () = !buf_off = Bytes.length buf in

    let flush () =
      if !buf_off > 0 then (
        write fd buf 0 !buf_off;
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
end

module In = struct
  include Iostream.In

  let of_unix_fd ?(close_noerr = false) ?(buf = Bytes.create _default_buf_size)
      (fd : Unix.file_descr) : t =
    let buf_len = ref 0 in
    let buf_off = ref 0 in

    let refill () =
      buf_off := 0;
      buf_len := read fd buf 0 (Bytes.length buf)
    in

    object
      method input b i len : int =
        if !buf_len = 0 then refill ();
        let n = min len !buf_len in
        if n > 0 then (
          Bytes.blit buf !buf_off b i n;
          buf_off := !buf_off + n;
          buf_len := !buf_len - n
        );
        n

      method close () =
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd
    end
end
