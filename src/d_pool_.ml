type domain = Domain_.t

type event =
  | Run of (unit -> unit)  (** Run this function *)
  | Die  (** Nudge the domain, asking it to die *)

(* State for a domain worker. It should not do too much except for starting
    new threads for pools. *)
type worker_state = {
  q: event Bb_queue.t;
  th_count: int Atomic_.t;  (** Number of threads on this *)
  mutable domain: domain option;
}

(** Array of (optional) workers.

    Workers are started/stop on demand. *)
let domains_ : worker_state option Lock.t array =
  (* number of domains we spawn. Note that we spawn n-1 domains
      because there already is the main domain running. *)
  let n = max 1 (Domain_.recommended_number () - 1) in
  Array.init n (fun _ -> Lock.create None)

let work_ (st : worker_state) : unit =
  Dla_.setup_domain ();
  let continue = ref true in
  while !continue do
    match Bb_queue.pop st.q with
    | Run f -> (try f () with _ -> ())
    | Die -> continue := false
  done

let[@inline] n_domains () : int = Array.length domains_

let run_on (i : int) (f : unit -> unit) : unit =
  assert (i < Array.length domains_);
  let w =
    Lock.update_map domains_.(i) (function
      | Some w as st ->
        Atomic_.incr w.th_count;
        st, w
      | None ->
        let w =
          { th_count = Atomic_.make 1; q = Bb_queue.create (); domain = None }
        in
        let worker : domain = Domain_.spawn (fun () -> work_ w) in
        w.domain <- Some worker;
        Some w, w)
  in
  Bb_queue.push w.q (Run f)

let decr_on (i : int) ~(domain_to_join : Domain_.t -> unit) : unit =
  assert (i < Array.length domains_);
  let st_to_kill =
    Lock.update_map domains_.(i) (function
      | None -> assert false
      | Some st ->
        if Atomic_.fetch_and_add st.th_count (-1) = 1 then
          None, Some st
        else
          Some st, None)
  in

  (* prepare for domain termination outside of critical section *)
  match st_to_kill with
  | None -> ()
  | Some st ->
    (* ask the domain to die *)
    Bb_queue.push st.q Die;
    Option.iter domain_to_join st.domain

let run_on_and_wait (i : int) (f : unit -> 'a) : 'a =
  let q = Bb_queue.create () in
  run_on i (fun () ->
      let x = f () in
      Bb_queue.push q x);
  Bb_queue.pop q
