type domain = Domain_.t

type event =
  | Run of (unit -> unit)  (** Run this function *)
  | Decr
      (** decrement number of threads on this domain. If it reaches 0,
    wind down *)

(* State for a domain worker. It should not do too much except for starting
    new threads for pools. *)
type worker_state = {
  q: event Bb_queue.t;
  th_count: int Atomic_.t;  (** Number of threads on this *)
}

(** Array of (optional) workers.

    Workers are started/stop on demand. *)
let domains_ : worker_state option Lock.t array =
  (* number of domains we spawn. Note that we spawn n-1 domains
      because there already is the main domain running. *)
  let n = max 1 (Domain_.recommended_number () - 1) in
  Array.init n (fun _ -> Lock.create None)

let work_ idx (st : worker_state) : unit =
  Dla_.setup_domain ();
  while Atomic_.get st.th_count > 0 do
    match Bb_queue.pop st.q with
    | Run f -> (try f () with _ -> ())
    | Decr ->
      if Atomic_.fetch_and_add st.th_count (-1) = 1 then
        Lock.set domains_.(idx) None
  done

let[@inline] n_domains () : int = Array.length domains_

let run_on (i : int) (f : unit -> unit) : unit =
  assert (i < Array.length domains_);

  Lock.update domains_.(i) (function
    | Some w as st ->
      Atomic_.incr w.th_count;
      Bb_queue.push w.q (Run f);
      st
    | None ->
      let st = { th_count = Atomic_.make 1; q = Bb_queue.create () } in
      let _domain : domain = Domain_.spawn (fun () -> work_ i st) in
      Bb_queue.push st.q (Run f);
      Some st)

let decr_on (i : int) : unit =
  assert (i < Array.length domains_);
  match Lock.get domains_.(i) with
  | None -> ()
  | Some st -> Bb_queue.push st.q Decr

let run_on_and_wait (i : int) (f : unit -> 'a) : 'a =
  let q = Bb_queue.create () in
  run_on i (fun () ->
      let x = f () in
      Bb_queue.push q x);
  Bb_queue.pop q
