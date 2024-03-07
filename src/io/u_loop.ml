open Common_

type io_mode =
  | Read
  | Write

module IO_wait = struct
  type t = {
    mutable active: bool;
    f: cancel_handle -> unit;
    as_cancel_handle: cancel_handle;
  }
  (** A single event, waiting on a unix FD *)

  let make f : t =
    let rec self =
      {
        active = true;
        f;
        as_cancel_handle = { cancel = (fun () -> self.active <- false) };
      }
    in
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
      let delay = run_timer_ _timer in

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

open struct
  let k_ev_loop : unix_ev_loop option ref TLS.key =
    TLS.new_key (fun () -> ref None)
end

(** Access the event loop from within it *)
let[@inline] cur () =
  match !(TLS.get k_ev_loop) with
  | None -> failwith "must be called from inside Fuseau_unix"
  | Some ev -> ev

let with_cur (ev : unix_ev_loop) f =
  let r = TLS.get k_ev_loop in
  let old = !r in
  r := Some ev;
  let finally () = r := old in
  Fun.protect ~finally f
