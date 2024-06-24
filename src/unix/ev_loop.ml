open Common_

(** Action scheduled from outside the loop *)
module Action = struct
  type cb = Cancel_handle.t -> unit

  (** Action that we ask the lwt loop to perform, from the outside *)
  type t =
    | Wait_readable of Unix.file_descr * cb * Cancel_handle.t
    | Wait_writable of Unix.file_descr * cb * Cancel_handle.t
    | Run_after_s of float * cb * Cancel_handle.t
    | Run_every_s of float * cb * Cancel_handle.t
end

(** Thread-safe queue of actions *)
module Action_queue = struct
  type t = { q: Action.t list Atomic.t } [@@unboxed]

  let create () : t = { q = Atomic.make [] }
  let pop_all (self : t) : _ list = Atomic.exchange self.q []

  (** Push the action and return whether the queue was previously empty *)
  let push (self : t) (a : Action.t) : bool =
    let is_first = ref true in
    while
      let old = Atomic.get self.q in
      if Atomic.compare_and_set self.q old (a :: old) then (
        is_first := old = [];
        false
      ) else
        true
    do
      ()
    done;
    !is_first
end

type io_mode =
  | Read
  | Write

module IO_wait = struct
  type t = {
    mutable active: bool;
    f: Cancel_handle.t -> unit;
    as_cancel_handle: Cancel_handle.t;
  }
  (** A single event, waiting on a unix FD *)

  let make cancel f : t =
    let self = { active = true; f; as_cancel_handle = cancel } in
    Cancel_handle.on_cancel cancel (fun () -> self.active <- false);
    self
end

module Per_fd = struct
  type t = {
    fd: Unix.file_descr;
    mutable reads: IO_wait.t list;
    mutable writes: IO_wait.t list;
  }

  let[@inline] is_empty self = self.reads = [] && self.writes = []
end

(** Keep track of the subscriptions to channels *)
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
      let per_fd = { Per_fd.fd; reads = []; writes = [] } in
      Hashtbl.add self.tbl fd per_fd;
      per_fd

  let add_io_wait (self : t) fd mode (ev : IO_wait.t) =
    let per_fd = get_or_create self fd in
    match mode with
    | Read ->
      self.n_read <- 1 + self.n_read;
      per_fd.reads <- ev :: per_fd.reads
    | Write ->
      self.n_write <- 1 + self.n_write;
      per_fd.writes <- ev :: per_fd.writes

  let prepare_select (self : t) =
    let reads = ref [] in
    let writes = ref [] in
    Hashtbl.iter
      (fun _ (per_fd : Per_fd.t) ->
        if Per_fd.is_empty per_fd then
          Hashtbl.remove self.tbl per_fd.fd
        else (
          if per_fd.reads <> [] then reads := per_fd.fd :: !reads;
          if per_fd.writes <> [] then writes := per_fd.fd :: !writes
        ))
      self.tbl;
    !reads, !writes

  let trigger_waiter (io : IO_wait.t) =
    if io.active then io.f io.as_cancel_handle

  let handle_ready ~ignore_read (self : t) (reads : Unix.file_descr list)
      (writes : Unix.file_descr list) : unit =
    List.iter
      (fun fd ->
        if fd <> ignore_read then (
          let per_fd = Hashtbl.find self.tbl fd in
          List.iter trigger_waiter per_fd.reads;
          self.n_read <- self.n_read - List.length per_fd.reads;
          per_fd.reads <- []
        ))
      reads;

    List.iter
      (fun fd ->
        let per_fd = Hashtbl.find self.tbl fd in
        List.iter trigger_waiter per_fd.writes;
        self.n_write <- self.n_write - List.length per_fd.writes;
        per_fd.writes <- [])
      writes;
    ()
end

let run_timer_ (t : Timer.t) =
  let rec loop () =
    match Timer.next t with
    | Timer.Empty -> None
    | Timer.Run (f, ev_h) ->
      f ev_h;
      loop ()
    | Timer.Wait f ->
      if f > 0. then
        Some f
      else
        None
  in
  loop ()

module Ev_loop = struct
  type t = {
    timer: Timer.t;
    actions: Action_queue.t;
    io_tbl: IO_tbl.t;  (** Used for select *)
    in_blocking_section: bool A.t;
        (** Is the ev loop thread currently waiting? *)
    pipe_read: Unix.file_descr;  (** Main thread only *)
    pipe_write: Unix.file_descr;  (** Wakeup main thread *)
    b4: bytes;  (** small buffer *)
  }

  let create () : t =
    let pipe_read, pipe_write = Unix.pipe ~cloexec:true () in
    Unix.set_nonblock pipe_read;
    Unix.set_nonblock pipe_write;
    {
      timer = Timer.create ();
      io_tbl = IO_tbl.create ();
      in_blocking_section = A.make false;
      actions = Action_queue.create ();
      pipe_read;
      pipe_write;
      b4 = Bytes.create 4;
    }

  (** Perform the action from within the ev loop thread *)
  let perform_action (self : t) (act : Action.t) : unit =
    match act with
    | Wait_readable (fd, cb, cancel) ->
      IO_tbl.add_io_wait self.io_tbl fd Read (IO_wait.make cancel cb)
    | Wait_writable (fd, cb, cancel) ->
      IO_tbl.add_io_wait self.io_tbl fd Write (IO_wait.make cancel cb)
    | Run_after_s (delay, cb, cancel) ->
      Timer.run_after_s self.timer delay cancel cb
    | Run_every_s (delay, cb, cancel) ->
      Timer.run_every_s self.timer delay cancel cb

  (** Gets the current set of notifications and perform them from inside the
    ev loop thread *)
  let perform_pending_actions (self : t) : unit =
    let l = Action_queue.pop_all self.actions in
    List.iter (perform_action self) l

  (** Empty the pipe *)
  let drain_pipe_ (self : t) : unit =
    try
      let continue = ref true in
      while !continue do
        match Unix.read self.pipe_read self.b4 0 (Bytes.length self.b4) with
        | n -> if n = 0 then continue := false
        | exception Unix.Unix_error (Unix.EAGAIN, _, _) -> ()
      done
    with Unix.Unix_error (Unix.EWOULDBLOCK, _, _) -> ()

  let run_step_ (self : t) : unit =
    perform_pending_actions self;

    let delay = run_timer_ self.timer in
    let delay = Option.value delay ~default:10. in

    (* run [select] *)
    let reads, writes = IO_tbl.prepare_select self.io_tbl in
    A.set self.in_blocking_section true;
    let reads, writes, _ =
      let@ _sp = Tracing_.with_span "moonpool-unix.evloop.select" in
      Unix.select (self.pipe_read :: reads) writes [] delay
    in
    A.set self.in_blocking_section false;

    drain_pipe_ self;
    IO_tbl.handle_ready ~ignore_read:self.pipe_read self.io_tbl reads writes;

    perform_pending_actions self;
    ()
end

(* ### global ev loop *)

let current_ : Ev_loop.t option A.t = A.make None

let rec get_or_set_as_current_ (ev : Ev_loop.t) : Ev_loop.t * bool =
  match A.get current_ with
  | Some ev -> ev, false
  | None ->
    if A.compare_and_set current_ None (Some ev) then
      ev, true
    else
      get_or_set_as_current_ ev

let bg_loop_ (ev_loop : Ev_loop.t) =
  let@ _sp = Tracing_.with_span "Moonpool_unix.bg-loop" in
  while true do
    Ev_loop.run_step_ ev_loop
  done

let[@inline never] start_background_loop () : Ev_loop.t =
  let ev_loop = Ev_loop.create () in
  let ev_loop, we_are_it = get_or_set_as_current_ ev_loop in
  (* start the background thread *)
  if we_are_it then ignore (Thread.create bg_loop_ ev_loop : Thread.t);
  ev_loop

(* ### external inputs *)

let[@inline] get_current_ () =
  match A.get current_ with
  | Some ev -> ev
  | None -> start_background_loop ()

let interrupt_if_in_blocking_section_ (self : Ev_loop.t) =
  if A.get self.in_blocking_section then (
    let b = Bytes.create 1 in
    ignore (Unix.write self.pipe_write b 0 1 : int)
  )

let wait_readable fd cancel f : unit =
  let ev_loop = get_current_ () in
  let first =
    Action_queue.push ev_loop.actions (Action.Wait_readable (fd, f, cancel))
  in
  if first then interrupt_if_in_blocking_section_ ev_loop

let wait_writable fd cancel f : unit =
  let ev_loop = get_current_ () in
  let first =
    Action_queue.push ev_loop.actions (Action.Wait_writable (fd, f, cancel))
  in
  if first then interrupt_if_in_blocking_section_ ev_loop

let run_after_s delay cancel f : unit =
  let ev_loop = get_current_ () in
  let first =
    Action_queue.push ev_loop.actions (Action.Run_after_s (delay, f, cancel))
  in
  if first then interrupt_if_in_blocking_section_ ev_loop

let run_every_s delay cancel f : unit =
  let ev_loop = get_current_ () in
  let first =
    Action_queue.push ev_loop.actions (Action.Run_every_s (delay, f, cancel))
  in
  if first then interrupt_if_in_blocking_section_ ev_loop
