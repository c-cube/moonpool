open Common_
module Slice = Iostream_types.Slice
module Fut = Moonpool.Fut

let rec read (fd : Fd.t) buf i len : int =
  if len = 0 || Fd.closed fd then
    0
  else (
    match Unix.read fd.fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      (* wait for FD to be ready *)
      let cancel = Cancel_handle.create () in
      let@ () =
        Fiber.with_on_self_cancel (fun _ -> Cancel_handle.cancel cancel)
      in
      Moonpool.Private.Suspend_.suspend
        {
          handle =
            (fun ~run:_ ~resume sus ->
              Ev_loop.wait_readable fd.fd cancel (fun cancel ->
                  resume sus @@ Ok ();
                  Cancel_handle.cancel cancel));
        };
      read fd buf i len
    | n -> n
  )

let rec write_once (fd : Fd.t) buf i len : int =
  if len = 0 || Fd.closed fd then
    0
  else (
    match Unix.write fd.fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      (* wait for FD to be ready *)
      let cancel = Cancel_handle.create () in
      let@ () =
        Fiber.with_on_self_cancel (fun _ -> Cancel_handle.cancel cancel)
      in
      Moonpool.Private.Suspend_.suspend
        {
          handle =
            (fun ~run:_ ~resume sus ->
              Ev_loop.wait_writable fd.fd cancel (fun cancel ->
                  resume sus @@ Ok ();
                  Cancel_handle.cancel cancel));
        };
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

module Reader = struct
  include Iostream_types.In

  let of_fd ?(close_noerr = false) (fd : Fd.t) : t =
    object
      method input buf i len = read fd buf i len

      method close () =
        if close_noerr then
          Fd.close_noerr fd
        else
          Fd.close fd
    end

  let of_slice (slice : Slice.t) : t =
    (Iostream.In.of_bytes ~off:slice.off ~len:slice.len slice.bytes :> t)
end

module Buf_reader = struct
  include Iostream_types.In_buf

  let of_fd ?(close_noerr = false) ~(buf : bytes) (fd : Fd.t) : t =
    let eof = ref false in
    object
      inherit Iostream.In_buf.t_from_refill ~bytes:buf ()

      method private refill (slice : Slice.t) =
        if not !eof then (
          slice.off <- 0;
          slice.len <- read fd slice.bytes 0 (Bytes.length slice.bytes);
          if slice.len = 0 then eof := true
        )

      method close () =
        if close_noerr then
          Fd.close_noerr fd
        else
          Fd.close fd
    end
end

module Writer = struct
  include Iostream_types.Out

  let of_fd ?(close_noerr = false) (fd : Fd.t) : t =
    object
      method output buf i len = write fd buf i len

      method close () =
        if close_noerr then
          Fd.close_noerr fd
        else
          Fd.close fd
    end
end

module Buf_writer = struct
  include Iostream_types.Out_buf

  let of_fd ?(close_noerr = false) ~(buf : bytes) (fd : Fd.t) : t =
    object
      inherit Iostream.Out_buf.t_from_output ~bytes:buf ()
      method private output_underlying bs i len = write fd bs i len

      method private close_underlying () =
        if close_noerr then
          Fd.close_noerr fd
        else
          Fd.close fd
    end
end

module Buf_pool = struct
  type t = { with_buf: 'a. int -> (bytes -> 'a) -> 'a } [@@unboxed]

  let dummy : t = { with_buf = (fun size f -> f (Bytes.create size)) }
end

(** A TCP server abstraction. *)
module TCP_server = struct
  type conn_handler =
    client_addr:Sockaddr.t -> Buf_reader.t -> Buf_writer.t -> unit

  class type t = object
    method endpoint : unit -> Sockaddr.t
    method active_connections : unit -> int
    method running : unit -> bool
    method run : unit -> unit
    method stop : unit -> unit
    method await : unit -> unit
  end

  let[@inline] run (self : #t) = self#run ()
  let[@inline] stop (self : #t) = self#stop ()
  let[@inline] endpoint (self : #t) = self#endpoint ()
  let[@inline] await (self : #t) = self#await ()

  type state =
    | Created
    | Running
    | Stopped

  let rec accept_ (sock : Unix.file_descr) =
    match Unix.accept sock with
    | csock, addr -> csock, addr
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      Ev_loop.wait_readable sock Cancel_handle.dummy ignore;
      accept_ sock

  class base_server ?(listen = 32) ?(buf_pool = Buf_pool.dummy)
    ?(buf_size = 4096) ~runner ~(handle : conn_handler) (addr : Sockaddr.t) :
    t =
    let n_active_ = A.make 0 in
    let st = A.make Created in
    let fut, promise = Fut.make () in

    object
      method endpoint () = addr
      method active_connections () = A.get n_active_
      method running () = A.get st = Running

      method stop () =
        match A.exchange st Stopped with
        | Stopped -> ()
        | Created | Running -> Fut.fulfill_idempotent promise @@ Ok ()

      method await () = Fut.await fut

      method run () =
        (* set to Running *)
        let can_start =
          let rec loop () =
            match A.get st with
            | Created -> A.compare_and_set st Created Running || loop ()
            | _ -> false
          in
          loop ()
        in

        if can_start then (
          let sock =
            try
              let sock =
                Unix.socket (Sockaddr.domain addr) Unix.SOCK_STREAM 0
              in
              Unix.setsockopt sock Unix.TCP_NODELAY true;
              Unix.set_nonblock sock;
              Unix.bind sock addr;
              Unix.listen sock listen;
              sock
            with e ->
              let bt = Printexc.get_raw_backtrace () in
              A.set st Stopped;
              Fut.fulfill_idempotent promise @@ Error (Exn_bt.make e bt);
              Printexc.raise_with_backtrace e bt
          in
          while A.get st = Running do
            let client_sock, client_addr = accept_ sock in
            let client_fd = Fd.create client_sock in

            (* start a fiber to handle the client *)
            let _ : _ Fiber.t =
              Fiber.spawn_top ~on:runner (fun () ->
                  A.incr n_active_;

                  let@ () =
                    Fun.protect ~finally:(fun () ->
                        A.decr n_active_;
                        Fd.close_noerr client_fd)
                  in

                  let@ buf_in = buf_pool.with_buf buf_size in
                  let@ buf_out = buf_pool.with_buf buf_size in

                  let ic = Buf_reader.of_fd client_fd ~buf:buf_in in
                  let oc = Buf_writer.of_fd client_fd ~buf:buf_out in
                  handle ~client_addr ic oc)
            in
            ()
          done
        )
    end

  let create ?(after_init = ignore) ?listen ?buf_pool ?buf_size ~runner
      ~(handle : conn_handler) (addr : Sockaddr.t) : t =
    let self =
      new base_server ?listen ?buf_pool ?buf_size ~runner ~handle addr
    in
    after_init self;
    self

  let with_server ?listen ?buf_pool ?buf_size ~runner ~handle addr (f : _ -> 'a)
      : 'a =
    let server =
      new base_server ?listen ?buf_pool ?buf_size ~runner ~handle addr
    in
    run server;
    let@ () = Fun.protect ~finally:(fun () -> stop server) in
    f server
end

module TCP_client = struct
  (** connect asynchronously *)
  let rec connect_ sock addr =
    match Unix.connect sock addr with
    | () -> ()
    | exception
        Unix.Unix_error
          ((Unix.EWOULDBLOCK | Unix.EINPROGRESS | Unix.EAGAIN), _, _) ->
      Ev_loop.wait_writable sock Cancel_handle.dummy ignore;
      connect_ sock addr

  let with_connect' addr (f : Fd.t -> 'a) : 'a =
    let sock = Unix.socket (Sockaddr.domain addr) Unix.SOCK_STREAM 0 in
    Unix.set_nonblock sock;
    Unix.setsockopt sock Unix.TCP_NODELAY true;

    connect_ sock addr;
    let sock = Fd.create sock in

    let finally () = Fd.close_noerr sock in
    let@ () = Fun.protect ~finally in
    f sock

  let with_connect ?(buf_pool = Buf_pool.dummy) ?(buf_size = 4096) addr
      (f : _ -> _ -> 'a) : 'a =
    with_connect' addr (fun sock ->
        let@ buf_in = buf_pool.with_buf buf_size in
        let@ buf_out = buf_pool.with_buf buf_size in

        let ic = Buf_reader.of_fd ~buf:buf_in sock in
        let oc = Buf_writer.of_fd ~buf:buf_out sock in
        f ic oc)
end
