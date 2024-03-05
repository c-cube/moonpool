module Bb_queue = struct
  type 'a t = {
    mutex: Mutex.t;
    cond: Condition.t;
    q: 'a Queue.t;
  }

  let create () : _ t =
    { mutex = Mutex.create (); cond = Condition.create (); q = Queue.create () }

  let push (self : _ t) x : unit =
    Mutex.lock self.mutex;
    let was_empty = Queue.is_empty self.q in
    Queue.push x self.q;
    if was_empty then Condition.broadcast self.cond;
    Mutex.unlock self.mutex

  let pop (self : 'a t) : 'a =
    Mutex.lock self.mutex;
    let rec loop () =
      if Queue.is_empty self.q then (
        Condition.wait self.cond self.mutex;
        (loop [@tailcall]) ()
      ) else (
        let x = Queue.pop self.q in
        Mutex.unlock self.mutex;
        x
      )
    in
    loop ()
end

module Lock = struct
  type 'a t = {
    mutex: Mutex.t;
    mutable content: 'a;
  }

  let create content : _ t = { mutex = Mutex.create (); content }

  let with_ (self : _ t) f =
    Mutex.lock self.mutex;
    try
      let x = f self.content in
      Mutex.unlock self.mutex;
      x
    with e ->
      Mutex.unlock self.mutex;
      raise e

  let[@inline] update_map l f =
    with_ l (fun x ->
        let x', y = f x in
        l.content <- x';
        y)

  let get l =
    Mutex.lock l.mutex;
    let x = l.content in
    Mutex.unlock l.mutex;
    x
end

type domain = Domain_.t

type event =
  | Run of (unit -> unit)  (** Run this function *)
  | Decr  (** Decrease count *)

(* State for a domain worker. It should not do too much except for starting
    new threads for pools. *)
type worker_state = {
  q: event Bb_queue.t;
  th_count: int Atomic_.t;  (** Number of threads on this *)
}

(** Array of (optional) workers.

    Workers are started/stop on demand. For each index we have
    the (currently active) domain's state
    including a work queue and a thread refcount; and the domain itself,
    if any, in a separate option because it might outlive its own state. *)
let domains_ : (worker_state option * Domain_.t option) Lock.t array =
  let n = max 1 (Domain_.recommended_number ()) in
  Array.init n (fun _ -> Lock.create (None, None))

(** main work loop for a domain worker.

   A domain worker does two things:
   - run functions it's asked to (mainly, to start new threads inside it)
   - decrease the refcount when one of these threads stops. The thread
     will notify the domain that it's exiting, so the domain can know
     how many threads are still using it. If all threads exit, the domain
     polls a bit (in case new threads are created really shortly after,
     which happens with a [Pool.with_] or [Pool.create() … Pool.shutdown()]
     in a tight loop), and if nothing happens it tries to stop to free resources.
*)
let work_ idx (st : worker_state) : unit =
  let main_loop () =
    let continue = ref true in
    while !continue do
      match Bb_queue.pop st.q with
      | Run f -> (try f () with _ -> ())
      | Decr ->
        if Atomic_.fetch_and_add st.th_count (-1) = 1 then (
          continue := false;

          (* wait a bit, we might be needed again in a short amount of time *)
          try
            for _n_attempt = 1 to 50 do
              Thread.delay 0.001;
              if Atomic_.get st.th_count > 0 then (
                (* needed again! *)
                continue := true;
                raise Exit
              )
            done
          with Exit -> ()
        )
    done
  in

  while
    main_loop ();

    (* exit: try to remove ourselves from [domains]. If that fails, keep living. *)
    let is_alive =
      Lock.update_map domains_.(idx) (function
        | None, _ -> assert false
        | Some _st', dom ->
          assert (st == _st');

          if Atomic_.get st.th_count > 0 then
            (* still alive! *)
            (Some st, dom), true
          else
            (None, dom), false)
    in

    is_alive
  do
    ()
  done;
  ()

(* special case for main domain: we start a worker immediately *)
let () =
  assert (Domain_.is_main_domain ());
  let w = { th_count = Atomic_.make 1; q = Bb_queue.create () } in
  (* thread that stays alive *)
  ignore (Thread.create (fun () -> work_ 0 w) () : Thread.t);
  domains_.(0) <- Lock.create (Some w, None)

let[@inline] max_number_of_domains () : int = Array.length domains_

let run_on (i : int) (f : unit -> unit) : unit =
  assert (i < Array.length domains_);
  let w =
    Lock.update_map domains_.(i) (function
      | (Some w, _) as st ->
        Atomic_.incr w.th_count;
        st, w
      | None, dying_dom ->
        (* join previous dying domain, to free its resources, if any *)
        Option.iter Domain_.join dying_dom;
        let w = { th_count = Atomic_.make 1; q = Bb_queue.create () } in
        let worker : domain = Domain_.spawn (fun () -> work_ i w) in
        (Some w, Some worker), w)
  in
  Bb_queue.push w.q (Run f)

let decr_on (i : int) : unit =
  assert (i < Array.length domains_);
  match Lock.get domains_.(i) with
  | Some st, _ -> Bb_queue.push st.q Decr
  | None, _ -> ()

let run_on_and_wait (i : int) (f : unit -> 'a) : 'a =
  let q = Bb_queue.create () in
  run_on i (fun () ->
      let x = f () in
      Bb_queue.push q x);
  Bb_queue.pop q
