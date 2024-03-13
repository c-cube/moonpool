open Common_

open struct
  let _default_buf_size = 16 * 1024
end

exception Closed

type state =
  | Closed
  | Open of {
      fd: Unix.file_descr;
      close_noerr: bool;
    }

type t = { st: state A.t } [@@unboxed]

let[@inline] st (self : t) : state = A.get self.st

let[@inline] fd (self : t) =
  match st self with
  | Closed -> raise Closed
  | Open { fd; _ } -> fd

let create ?(close_noerr = true) fd : t =
  Unix.set_nonblock fd;
  { st = A.make @@ Open { close_noerr; fd } }

let close (self : t) : unit =
  while
    let old = A.get self.st in
    match old with
    | Closed -> false
    | Open { fd; close_noerr } ->
      if A.compare_and_set self.st old Closed then (
        if close_noerr then (
          try Unix.close fd with _ -> ()
        ) else
          Unix.close fd;

        false
      ) else
        true
  do
    ()
  done

let await_readable (self : t) =
  let loop = Ev_loop.get_or_create () in
  match A.get self.st with
  | Closed -> raise Closed
  | Open { fd; _ } ->
    (* wait for FD to be ready *)
    Moonpool.Private.Suspend_.suspend
      {
        handle =
          (fun ~run:_ ~resume sus ->
            ignore
              (Ev_loop.on_readable loop fd (fun () -> resume sus @@ Ok ())
                : Cancel_handle.t));
      }

let rec read (self : t) buf i len : int =
  match st self with
  | Closed -> 0
  | Open { fd; _ } ->
    if len = 0 then
      0
    else (
      match Unix.read fd buf i len with
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (match await_readable self with
        | () -> read self buf i len
        | exception Closed -> 0)
      | n -> n
    )

let await_writable (self : t) =
  let loop = Ev_loop.get_or_create () in
  match st self with
  | Closed -> raise Closed
  | Open { fd; _ } ->
    (* wait for FD to be ready *)
    Moonpool.Private.Suspend_.suspend
      {
        handle =
          (fun ~run:_ ~resume sus ->
            ignore
              (Ev_loop.on_writable loop fd (fun () -> resume sus @@ Ok ())
                : Cancel_handle.t));
      }

let rec write_once (self : t) buf i len : int =
  match st self with
  | Closed -> 0
  | Open { fd; _ } ->
    if len = 0 then
      0
    else (
      match Unix.write fd buf i len with
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (match await_writable self with
        | () -> write_once self buf i len
        | exception Closed -> 0)
      | n -> n
    )

let write self buf i len : unit =
  let i = ref i in
  let len = ref len in
  while !len > 0 do
    let n = write_once self buf !i !len in
    if n = 0 then raise Closed;
    i := !i + n;
    len := !len - n
  done

class to_in (self : t) : Iostream.In.t =
  object
    method input bs i len = read self bs i len
    method close () = close self
  end

class to_out (self : t) : Iostream.Out.t =
  object
    method output bs i len = write self bs i len
    method close () = close self
  end

class to_in_buf ?bytes (self : t) : Iostream.In_buf.t =
  object
    inherit Iostream.In_buf.t_from_refill ?bytes ()

    method private refill (slice : Iostream.Slice.t) =
      slice.off <- 0;
      slice.len <- read self slice.bytes 0 (Bytes.length slice.bytes)

    method close () = close self
  end

class to_out_buf ?bytes (self : t) : Iostream.Out_buf.t =
  object
    inherit Iostream.Out_buf.t_from_output ?bytes ()
    method private close_underlying () = close self
    method private output_underlying bs i len = write self bs i len
  end
