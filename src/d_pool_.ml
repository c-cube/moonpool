type domain = Domain_.t

let work_ _i q : unit =
  while true do
    let f = Bb_queue.pop q in
    try f () with _ -> ()
  done

(* A domain level worker. It should not do too much except for starting
    new threads for pools. *)
type worker = { q: (unit -> unit) Bb_queue.t } [@@unboxed]

let domains_ : worker array lazy_t =
  lazy
    ((* number of domains we spawn. Note that we spawn n-1 domains
        because there already is the main domain running. *)
     let n = Domain_.recommended_number () - 1 in
     Array.init n (fun i ->
         let q = Bb_queue.create () in
         let _domain : domain = Domain_.spawn (fun () -> work_ i q) in
         { q }))

let[@inline] n_domains () : int = Array.length (Lazy.force domains_)

let run_on (i : int) (f : unit -> unit) : unit =
  let (lazy arr) = domains_ in
  assert (i < Array.length arr);
  Bb_queue.push arr.(i).q f

let run_on_and_wait (i : int) (f : unit -> 'a) : 'a =
  let q = Bb_queue.create () in
  run_on i (fun () ->
      let x = f () in
      Bb_queue.push q x);
  Bb_queue.pop q
