(* TODO: use a better queue for the tasks *)

module A = Atomic_

type task = unit -> unit

type t = {
  active: bool A.t;
  threads: Thread.t array;
  qs: task Bb_queue.t array;
  cur_q: int A.t;  (** Selects queue into which to push *)
}

type thread_loop_wrapper =
  thread:Thread.t -> pool:t -> (unit -> unit) -> unit -> unit

let global_thread_wrappers_ : thread_loop_wrapper list A.t = A.make []

let add_global_thread_loop_wrapper f : unit =
  while
    let l = A.get global_thread_wrappers_ in
    not (A.compare_and_set global_thread_wrappers_ l (f :: l))
  do
    Domain_.relax ()
  done

exception Shutdown

let run self f : unit =
  let i = A.fetch_and_add self.cur_q 1 in
  let q = self.qs.(i mod Array.length self.qs) in
  try Bb_queue.push q f with Bb_queue.Closed -> raise Shutdown

let size self = Array.length self.threads

exception Got_task of task

let worker_thread_ ~on_exn (active : bool A.t) (qs : task Bb_queue.t array)
    ~(offset : int) : unit =
  let num_qs = Array.length qs in

  try
    while A.get active do
      (* last resort: block on my queue *)
      let pop_blocking () =
        let my_q = qs.(offset mod num_qs) in
        Bb_queue.pop my_q
      in

      let task =
        try
          for _retry = 1 to 3 do
            for i = 0 to num_qs - 1 do
              let q = qs.((offset + i) mod num_qs) in
              match Bb_queue.try_pop ~force_lock:false q with
              | Some f -> raise_notrace (Got_task f)
              | None -> ()
            done
          done;
          pop_blocking ()
        with Got_task f -> f
      in

      try task ()
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        on_exn e bt
    done
  with Bb_queue.Closed -> ()

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(thread_wrappers = [])
    ?(on_exn = fun _ _ -> ()) ?min:(min_threads = 1) ?(per_domain = 0) () : t =
  (* number of threads to run *)
  let min_threads = max 1 min_threads in
  let num_domains = D_pool_.n_domains () in
  assert (num_domains >= 1);
  let num_threads = max min_threads (num_domains * per_domain) in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int num_domains in

  let active = A.make true in
  let qs =
    let num_qs = min num_domains num_threads in
    Array.init num_qs (fun _ -> Bb_queue.create ())
  in

  let pool =
    let dummy = Thread.self () in
    { active; threads = Array.make num_threads dummy; qs; cur_q = A.make 0 }
  in

  (* temporary queue used to obtain thread handles from domains
     on which the thread are started. *)
  let receive_threads = Bb_queue.create () in

  (* start the thread with index [i] *)
  let start_thread_with_idx i =
    let dom_idx = (offset + i) mod num_domains in

    (* function run in the thread itself *)
    let main_thread_fun () =
      let thread = Thread.self () in
      let t_id = Thread.id thread in
      on_init_thread ~dom_id:dom_idx ~t_id ();

      let all_wrappers =
        List.rev_append thread_wrappers (A.get global_thread_wrappers_)
      in

      let run () = worker_thread_ ~on_exn active qs ~offset:i in
      (* the actual worker loop is [worker_thread_], with all
         wrappers for this pool and for all pools (global_thread_wrappers_) *)
      let run' =
        List.fold_left (fun run f -> f ~thread ~pool run) run all_wrappers
      in

      (* now run the main loop *)
      run' ();
      on_exit_thread ~dom_id:dom_idx ~t_id ()
    in

    (* function called in domain with index [i], to
       create the thread and push it into [receive_threads] *)
    let create_thread_in_domain () =
      let thread = Thread.create main_thread_fun () in
      (* send the thread from the domain back to us *)
      Bb_queue.push receive_threads (i, thread)
    in

    D_pool_.run_on dom_idx create_thread_in_domain
  in

  (* start all threads, placing them on the domains
     according to their index and [offset] in a round-robin fashion. *)
  for i = 0 to num_threads - 1 do
    start_thread_with_idx i
  done;

  (* receive the newly created threads back from domains *)
  for _j = 1 to num_threads do
    let i, th = Bb_queue.pop receive_threads in
    pool.threads.(i) <- th
  done;
  pool

let shutdown (self : t) : unit =
  let was_active = A.exchange self.active false in
  (* close the job queues, which will fail future calls to [run],
     and wake up the subset of [self.threads] that are waiting on them. *)
  if was_active then Array.iter Bb_queue.close self.qs;
  Array.iter Thread.join self.threads
