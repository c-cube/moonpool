open Common_
module B_queue = Moonpool.Blocking_queue

type io_mode =
  | Read
  | Write

module IO_wait = struct
  type t = { f: unit -> unit } [@@unboxed]
  (** A single event, waiting on a unix FD *)

  let[@inline] make f : t = { f }
end

module Per_fd = struct
  type t = {
    fd: Unix.file_descr;
    mutable reads: IO_wait.t Handle.Map.t;
    mutable writes: IO_wait.t Handle.Map.t;
  }

  let[@inline] no_reads self = Handle.Map.is_empty self.reads
  let[@inline] no_writes self = Handle.Map.is_empty self.writes
  let[@inline] is_empty self = no_reads self && no_writes self

  let cancel_read (self : t) h : bool =
    if Handle.Map.mem h self.reads then (
      self.reads <- Handle.Map.remove h self.reads;
      true
    ) else
      false

  let cancel_write (self : t) h : bool =
    if Handle.Map.mem h self.writes then (
      self.reads <- Handle.Map.remove h self.writes;
      true
    ) else
      false
end

module IO_tbl = struct
  type t = {
    mutable n_read: int;
    mutable n_write: int;
    tbl: (Unix.file_descr, Per_fd.t) Hashtbl.t;
  }

  let create () : t = { tbl = Hashtbl.create 32; n_read = 0; n_write = 0 }

  let get_or_create (self : t) fd : Per_fd.t =
    try Hashtbl.find self.tbl fd
    with Not_found ->
      let per_fd =
        { Per_fd.fd; reads = Handle.Map.empty; writes = Handle.Map.empty }
      in
      Hashtbl.add self.tbl fd per_fd;
      per_fd

  let update_subs poll (per_fd : Per_fd.t) =
    let ev =
      match Per_fd.no_reads per_fd, Per_fd.no_writes per_fd with
      | true, true -> Poll.Event.none
      | true, false -> Poll.Event.write
      | false, true -> Poll.Event.read
      | false, false -> Poll.Event.read_write
    in
    Poll.set poll per_fd.fd ev

  let add_io_wait (self : t) poll fd mode (h : Handle.t) (ev : IO_wait.t) =
    let per_fd = get_or_create self fd in
    (match mode with
    | Read ->
      self.n_read <- 1 + self.n_read;
      per_fd.reads <- Handle.Map.add h ev per_fd.reads
    | Write ->
      self.n_write <- 1 + self.n_write;
      per_fd.writes <- Handle.Map.add h ev per_fd.writes);
    update_subs poll per_fd

  let cancel (self : t) (fd : Unix.file_descr) (h : Handle.t) : unit =
    match Hashtbl.find_opt self.tbl fd, Handle.handle_type h with
    | None, _ -> ()
    | Some per_fd, H_read ->
      if Per_fd.cancel_read per_fd h then self.n_read <- self.n_read - 1
    | Some per_fd, H_write ->
      if Per_fd.cancel_write per_fd h then self.n_write <- self.n_write - 1
    | Some _, H_timer -> assert false

  let[@inline] trigger_waiter (_h : Handle.t) (io : IO_wait.t) = io.f ()

  let handle_ready_read (self : t) ~ignore_read fd (per_fd : Per_fd.t) =
    if fd <> ignore_read then (
      Handle.Map.iter trigger_waiter per_fd.reads;
      self.n_read <- self.n_read - Handle.Map.cardinal per_fd.reads;
      per_fd.reads <- Handle.Map.empty
    )

  let handle_ready_write (self : t) (per_fd : Per_fd.t) =
    Handle.Map.iter trigger_waiter per_fd.writes;
    self.n_write <- self.n_write - Handle.Map.cardinal per_fd.writes;
    per_fd.writes <- Handle.Map.empty

  let update_all (self : t) ~ignore_read (poll : Poll.t) : unit =
    let handle_ready fd (ev : Poll.Event.t) =
      match Hashtbl.find self.tbl fd with
      | exception Not_found -> ()
      | per_fd ->
        if ev.readable then handle_ready_read self ~ignore_read fd per_fd;
        if ev.writable then handle_ready_write self per_fd;
        update_subs poll per_fd;
        if Per_fd.is_empty per_fd then Hashtbl.remove self.tbl fd
    in
    Poll.iter_ready poll ~f:handle_ready
end

(** Messages from other threads *)
module Incoming_msg = struct
  type t =
    | On_ready of Unix.file_descr * io_mode * Handle.t * (unit -> unit)
    | Run_after of float * Handle.t * (unit -> unit)
    | Run_every of float * Handle.t * (Cancel_handle.t -> unit)
    | Cancel_io of Unix.file_descr * Handle.t
    | Cancel_timer of Handle.t
end

module Main_loop = struct
  (** Process timers that have expired, and return the duration until the next timer *)
  let process_expired_timers_ (t : Timer.t) : float option =
    let rec loop () =
      match Timer.next t with
      | Timer.Empty -> None
      | Timer.Run f ->
        f ();
        loop ()
      | Timer.Wait f ->
        if f > 0. then
          Some f
        else
          None
    in
    loop ()

  type state = {
    timer: Timer.t;
    io_tbl: IO_tbl.t;
    incoming: Incoming_msg.t B_queue.t;
    poll: Poll.t;
    in_poll: bool A.t;
    buf4: bytes;
    _magic_pipe_read: Unix.file_descr;
    _magic_pipe_write: Unix.file_descr;
  }

  let create () : state =
    let _magic_pipe_read, _magic_pipe_write = Unix.pipe ~cloexec:true () in
    Unix.set_nonblock _magic_pipe_read;
    Unix.set_nonblock _magic_pipe_write;
    let poll = Poll.create () in
    {
      timer = Timer.create ();
      io_tbl = IO_tbl.create ();
      incoming = B_queue.create ();
      in_poll = A.make false;
      poll;
      buf4 = Bytes.create 4;
      _magic_pipe_read;
      _magic_pipe_write;
    }

  let push (self : state) (msg : Incoming_msg.t) =
    B_queue.push self.incoming msg;
    if A.exchange self.in_poll false then (
      (* maybe wake up the loop *)
      let b = Bytes.make 1 '\x01' in
      ignore (Unix.write self._magic_pipe_write b 0 1 : int)
    )

  let cancel_timer_ (self : state) (h : Handle.t) =
    push self (Incoming_msg.Cancel_timer h)

  (** Make sure the pipe is empty *)
  let drain_pipe_ (self : state) =
    while
      match Unix.read self._magic_pipe_read self.buf4 0 4 with
      | 0 -> false
      | _n -> true
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        false
    do
      ()
    done

  let process_msg (self : state) (msg : Incoming_msg.t) =
    match msg with
    | On_ready (fd, mode, h, f) ->
      IO_tbl.add_io_wait self.io_tbl self.poll fd mode h (IO_wait.make f)
    | Run_after (delay, h, f) -> Timer.run_after_s self.timer delay h f
    | Run_every (delay, h, f) ->
      let cancel = Cancel_handle.Cancel2 (cancel_timer_, self, h) in
      Timer.run_every_s self.timer delay h (fun () -> f cancel)
    | Cancel_io (fd, h) -> IO_tbl.cancel self.io_tbl fd h
    | Cancel_timer h -> Timer.cancel self.timer h

  let loop (self : state) : unit =
    let local = Queue.create () in
    while true do
      (* process incoming messages *)
      B_queue.transfer self.incoming local;
      while not (Queue.is_empty local) do
        let msg = Queue.pop local in
        process_msg self msg
      done;

      (* check IOs *)
      IO_tbl.update_all self.io_tbl ~ignore_read:self._magic_pipe_read self.poll;

      (* update timers, get next timeout *)
      let timer = process_expired_timers_ self.timer in
      let timeout =
        match timer with
        | None -> Poll.Timeout.Never
        | Some t_s ->
          let t_ns = Int64.of_float @@ ceil @@ (t_s *. 1e9) in
          Poll.Timeout.After t_ns
      in

      (* wait for something to happen *)
      A.set self.in_poll true;
      ignore (Poll.wait self.poll timeout : [ `Ok | `Timeout ]);
      A.set self.in_poll false;

      drain_pipe_ self;

      IO_tbl.update_all self.io_tbl ~ignore_read:self._magic_pipe_read self.poll
    done
end

type t = Main_loop.state

let cur_ : t option A.t = A.make None

open struct
  (* used only for init *)
  let m = Mutex.create ()

  let[@inline never] create_and_set_ () : t =
    Mutex.lock m;
    match A.get cur_ with
    | Some t ->
      Mutex.unlock m;
      t
    | None ->
      let st = Main_loop.create () in
      (* start background thread *)
      let _t : Thread.t =
        Moonpool.start_thread_on_some_domain Main_loop.loop st
      in
      A.set cur_ (Some st);
      Mutex.unlock m;
      st
end

let[@inline] get_or_create () : t =
  match A.get cur_ with
  | Some t -> t
  | None -> create_and_set_ ()

let on_readable (self : t) fd f : Cancel_handle.t =
  let h = Handle.fresh H_read in
  Main_loop.push self (Incoming_msg.On_ready (fd, Read, h, f));
  Cancel_handle.Cancel2 (Main_loop.push, self, Incoming_msg.Cancel_io (fd, h))

let on_writable (self : t) fd f : Cancel_handle.t =
  let h = Handle.fresh H_write in
  Main_loop.push self (Incoming_msg.On_ready (fd, Write, h, f));
  Cancel_handle.Cancel2 (Main_loop.push, self, Incoming_msg.Cancel_io (fd, h))

let run_after_s (self : t) delay f : Cancel_handle.t =
  let h = Handle.fresh H_timer in
  Main_loop.push self (Incoming_msg.Run_after (delay, h, f));
  Cancel_handle.Cancel2 (Main_loop.push, self, Incoming_msg.Cancel_timer h)

let run_every_s (self : t) delay f : Cancel_handle.t =
  let h = Handle.fresh H_timer in
  Main_loop.push self (Incoming_msg.Run_every (delay, h, f));
  Cancel_handle.Cancel2 (Main_loop.push, self, Incoming_msg.Cancel_timer h)

(*
class unix_ev_loop =
  let _timer = Timer.create () in
  let _io_wait : IO_tbl.t = IO_tbl.create () in
  let _in_blocking_section = ref false in

  let _magic_pipe_read, _magic_pipe_write = Unix.pipe ~cloexec:true () in
  let () =
    Unix.set_nonblock _magic_pipe_read;
    Unix.set_nonblock _magic_pipe_write
  in

  let[@inline] has_pending_tasks () =
    _io_wait.n_read > 0 || _io_wait.n_write > 0 || Timer.has_tasks _timer
  in

  object
    (* val read_ : (cancel_handle -> unit) Int_tbl.t = Int_tbl.create 32 *)
    method one_step ~block () : unit =
      let delay = process_expired_timers_ _timer in

      let delay =
        if block then
          Option.value delay ~default:10.
        else
          (* do not wait *)
          0.
      in

      let reads, writes = IO_tbl.prepare_select _io_wait in
      if has_pending_tasks () then (
        _in_blocking_section := true;
        let reads, writes, _ =
          Unix.select (_magic_pipe_read :: reads) writes [] delay
        in
        _in_blocking_section := false;
        IO_tbl.handle_ready ~ignore_read:_magic_pipe_read _io_wait reads writes
      );
      ()

    method on_readable
        : Unix.file_descr -> (cancel_handle -> unit) -> cancel_handle =
      fun fd f : cancel_handle ->
        let ev = IO_wait.make f in
        IO_tbl.add_io_wait _io_wait fd Read ev;
        ev.as_cancel_handle

    method on_writable
        : Unix.file_descr -> (cancel_handle -> unit) -> cancel_handle =
      fun fd f : cancel_handle ->
        let ev = IO_wait.make f in
        IO_tbl.add_io_wait _io_wait fd Write ev;
        ev.as_cancel_handle

    method on_timer
        : float -> repeat:bool -> (cancel_handle -> unit) -> cancel_handle =
      fun delay ~repeat f ->
        if repeat then
          Timer.run_every _timer delay f
        else
          Timer.run_after _timer delay f

    method interrupt_if_in_blocking_section =
      if !_in_blocking_section then (
        let b = Bytes.create 1 in
        ignore (Unix.write _magic_pipe_write b 0 1 : int)
      )

    method has_pending_tasks : bool = has_pending_tasks ()
  end
  *)
