(* TODO: use a better queue for the tasks *)

module A = Atomic_

type t = {
  active: bool A.t;
  threads: Thread.t array;
  q: (unit -> unit) S_queue.t;
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

let[@inline] run self f : unit =
  try S_queue.push self.q f with S_queue.Closed -> raise Shutdown

let size self = Array.length self.threads

let worker_thread_ ~on_exn (active : bool A.t) (q : _ S_queue.t) : unit =
  while A.get active do
    match S_queue.pop q with
    | exception S_queue.Closed -> ()
    | task ->
      (try task ()
       with e ->
         let bt = Printexc.get_raw_backtrace () in
         on_exn e bt)
  done

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(thread_wrappers = [])
    ?(on_exn = fun _ _ -> ()) ?(min = 1) ?(per_domain = 0) () : t =
  (* number of threads to run *)
  let min = max 1 min in
  let n_domains = D_pool_.n_domains () in
  assert (n_domains >= 1);
  let n = max min (n_domains * per_domain) in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int n_domains in

  let active = A.make true in
  let q = S_queue.create () in

  let pool =
    let dummy = Thread.self () in
    { active; threads = Array.make n dummy; q }
  in

  (* temporary queue used to obtain thread handles from domains
     on which the thread are started. *)
  let receive_threads = S_queue.create () in

  (* start the thread with index [i] *)
  let start_thread_with_idx i =
    let dom_idx = (offset + i) mod n_domains in

    (* function run in the thread itself *)
    let main_thread_fun () =
      let thread = Thread.self () in
      let t_id = Thread.id thread in
      on_init_thread ~dom_id:dom_idx ~t_id ();

      let all_wrappers =
        List.rev_append thread_wrappers (A.get global_thread_wrappers_)
      in

      let run () = worker_thread_ ~on_exn active q in
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
      S_queue.push receive_threads (i, thread)
    in

    D_pool_.run_on dom_idx create_thread_in_domain
  in

  (* start all threads, placing them on the domains
     according to their index and [offset] in a round-robin fashion. *)
  for i = 0 to n - 1 do
    start_thread_with_idx i
  done;

  (* receive the newly created threads back from domains *)
  for _j = 1 to n do
    let i, th = S_queue.pop receive_threads in
    pool.threads.(i) <- th
  done;
  pool

let shutdown (self : t) : unit =
  let was_active = A.exchange self.active false in
  (* close the job queue, which will fail future calls to [run],
     and wake up the subset of [self.threads] that are waiting on it. *)
  if was_active then S_queue.close self.q;
  Array.iter Thread.join self.threads
