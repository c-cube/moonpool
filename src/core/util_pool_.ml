let on_exn exn bt =
  Printf.eprintf "moonpool: uncaught exception in handler:\n%s\n%s\n%!"
    (Printexc.to_string exn)
    (Printexc.raw_backtrace_to_string bt)

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?num_threads:int ->
  ?name:string ->
  'a

let num_threads ?num_threads () : int =
  let n_domains = Moonpool_dpool.max_number_of_domains () in

  (* number of threads to run *)
  let num_threads =
    match num_threads with
    | Some j -> max 1 j
    | None -> n_domains
  in

  num_threads

(* extracted from ws_pool/fifo_pool *)
let spawn_workers_round_robin ~num_threads
    (mk_thread : int -> dom_id:int -> Thread.t) : Thread.t array =
  let num_domains = Moonpool_dpool.max_number_of_domains () in

  (* make sure we don't bias towards the first domain(s) *)
  let offset = Random.int num_domains in

  (* temporary queue used to obtain thread handles from domains
     on which the threads are started. *)
  let receive_threads = Bb_queue.create () in

  for i = 0 to num_threads - 1 do
    let dom_id = (offset + i) mod num_domains in
    (* function called in domain with index [dom_id], to
       create the thread and push it into [receive_threads] *)
    Moonpool_dpool.run_on dom_id (fun () ->
        let thread = mk_thread i ~dom_id in
        Bb_queue.push receive_threads (i, thread))
  done;

  (* receive the newly created threads back from domains *)
  let threads = Array.make num_threads (Thread.self ()) in
  for _j = 1 to num_threads do
    let i, th = Bb_queue.pop receive_threads in
    threads.(i) <- th
  done;
  threads
