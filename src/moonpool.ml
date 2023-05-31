module A = Atomic_

type 'a or_error = ('a, exn * Printexc.raw_backtrace) result

(** Simple blocking queue *)
module S_queue : sig
  type 'a t

  val create : unit -> _ t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a
end = struct
  type 'a t = {
    mutex: Mutex.t;
    cond: Condition.t;
    q: 'a Queue.t;
  }

  let create () : _ t =
    { mutex = Mutex.create (); cond = Condition.create (); q = Queue.create () }

  let push (self : _ t) x : unit =
    Mutex.lock self.mutex;
    Queue.push x self.q;
    Condition.signal self.cond;
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

(** Static pool of domains *)
module D_pool_ = struct
  type domain = Domain_.t

  let work_ _i q : unit =
    while true do
      let f = S_queue.pop q in
      try f () with _ -> ()
    done

  (* A domain level worker. It should not do too much except for starting
      new threads for pools. *)
  type worker = { q: (unit -> unit) S_queue.t } [@@unboxed]

  let domains_ : worker array lazy_t =
    lazy
      (let n = Domain_.recommended_number () in
       Array.init n (fun i ->
           let q = S_queue.create () in
           let _domain : domain = Domain_.spawn_on (fun () -> work_ i q) in
           { q }))

  (** Number of domains in the pool *)
  let[@inline] n_domains () : int = Array.length (Lazy.force domains_)

  let run_on (i : int) (f : unit -> unit) : unit =
    let (lazy arr) = domains_ in
    assert (i < Array.length arr);
    S_queue.push arr.(i).q f
end

module Pool = struct
  (* TODO: use a better queue for the tasks *)

  type t = {
    active: bool A.t;
    threads: Thread.t array;
    q: (unit -> unit) S_queue.t;
  }

  let[@inline] run self f : unit = S_queue.push self.q f

  let worker_thread_ ~on_exn (active : bool A.t) (q : _ S_queue.t) : unit =
    while A.get active do
      let task = S_queue.pop q in
      try task ()
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        on_exn e bt
    done

  let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

  let create ?(on_init_thread = default_thread_init_exit_)
      ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
      ?(min = 1) ?(per_domain = 0) () : t =
    (* number of threads to run *)
    let min = max 1 min in
    let n_domains = D_pool_.n_domains () in
    assert (n_domains >= 1);
    let n = max min (n_domains * per_domain) in

    (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
    let offset = Random.int n_domains in

    let active = A.make true in
    let q = S_queue.create () in

    let receive_threads = S_queue.create () in

    (* start the thread with index [i] *)
    let start_thread_with_idx i =
      let dom_idx = (offset + i) mod n_domains in

      let create () =
        let thread =
          Thread.create
            (fun () ->
              let t_id = Thread.id @@ Thread.self () in
              on_init_thread ~dom_id:dom_idx ~t_id ();
              worker_thread_ ~on_exn active q;
              on_exit_thread ~dom_id:dom_idx ~t_id ())
            ()
        in
        (* send the thread from the domain back to us *)
        S_queue.push receive_threads (i, thread)
      in

      D_pool_.run_on dom_idx create
    in

    (* start all threads, placing them on the domains
       according to their index and [offset] in a round-robin fashion. *)
    let threads =
      let dummy = Thread.self () in
      Array.init n (fun i ->
          start_thread_with_idx i;
          dummy)
    in

    (* receive the newly created threads back from domains *)
    for _j = 1 to n do
      let i, th = S_queue.pop receive_threads in
      threads.(i) <- th
    done;

    { active; threads; q }

  let shutdown (self : t) : unit =
    let was_active = A.exchange self.active false in
    (* make sure to wakeup all the sleeping threads by scheduling one task each.
       This way, a thread that is asleep, waiting for tasks,
       will wakeup to process this trivial task, check [self.active], and terminate. *)
    if was_active then Array.iter (fun _ -> run self ignore) self.threads;
    Array.iter Thread.join self.threads
end

module Fut = struct
  type 'a waiter = 'a or_error -> unit

  type 'a state =
    | Done of 'a or_error
    | Waiting of 'a waiter list

  type 'a t = { st: 'a state A.t } [@@unboxed]
  type 'a promise = 'a t

  let make () =
    let fut = { st = A.make (Waiting []) } in
    fut, fut

  let of_result x : _ t = { st = A.make (Done x) }
  let[@inline] return x : _ t = of_result (Ok x)
  let[@inline] fail e bt : _ t = of_result (Error (e, bt))

  let on_result (self : _ t) (f : _ waiter) : unit =
    while
      let st = A.get self.st in
      match st with
      | Done x ->
        f x;
        false
      | Waiting l ->
        let must_retry =
          not (A.compare_and_set self.st st (Waiting (f :: l)))
        in
        must_retry
    do
      ()
    done

  exception Already_fulfilled

  let fulfill (self : _ t) (r : _ result) : unit =
    while
      let st = A.get self.st in
      match st with
      | Done _ -> raise Already_fulfilled
      | Waiting l ->
        let did_swap = A.compare_and_set self.st st (Done r) in
        if did_swap then (
          (* success, now call all the waiters *)
          List.iter (fun f -> try f r with _ -> ()) l;
          false
        ) else
          true
    do
      ()
    done
end
