(* DScheck interleaving tests for [Ws_deque_], the work-stealing deque used
   by every pool. [ws_deque_.ml] here is an unmodified copy of
   [src/private/ws_deque_.ml] (see dune rule); [atomic.ml] shadows
   [Stdlib.Atomic] so it runs against DScheck's traced atomics.

   NOTE: DScheck only tracks races on [Atomic.t] cells ("domains communicate
   through atomic variables only"). It cannot see races on the deque's plain
   backing array, so it would not have caught the cross-thread [steal]
   clearing bug found via the [t_ws_deque.ml] stress test. What it *does*
   check: that the [top]/[bottom]/CAS index protocol never hands out the
   same logical item twice, or drops one, across every reachable
   interleaving of a small push/pop/steal scenario. *)

let dummy = -1

let drain_remaining (q : int Ws_deque_.t) : int list =
  let acc = ref [] in
  let continue = ref true in
  while !continue do
    match Ws_deque_.pop q with
    | Some x -> acc := x :: !acc
    | None -> continue := false
  done;
  !acc

(* one owner (push + pop), one stealer *)
let owner_stealer () =
  Atomic.trace (fun () ->
      let q = Ws_deque_.create ~dummy () in
      let total_items = 4 in

      let popped = ref 0 in
      Atomic.spawn (fun () ->
          for i = 1 to total_items do
            ignore (Ws_deque_.push q i : bool)
          done;
          for _ = 1 to total_items / 2 do
            match Ws_deque_.pop q with
            | Some _ -> incr popped
            | None -> ()
          done);

      let stolen = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 2 do
            match Ws_deque_.steal q with
            | Some _ -> incr stolen
            | None -> ()
          done);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = List.length (drain_remaining q) in
              remaining + !popped + !stolen = total_items)))

(* one owner (push + pop), two stealers *)
let owner_two_stealers () =
  Atomic.trace (fun () ->
      let q = Ws_deque_.create ~dummy () in
      let total_items = 6 in

      let popped = ref 0 in
      Atomic.spawn (fun () ->
          for i = 1 to total_items do
            ignore (Ws_deque_.push q i : bool)
          done;
          for _ = 1 to total_items / 3 do
            match Ws_deque_.pop q with
            | Some _ -> incr popped
            | None -> ()
          done);

      let stolen1 = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 3 do
            match Ws_deque_.steal q with
            | Some _ -> incr stolen1
            | None -> ()
          done);

      let stolen2 = ref 0 in
      Atomic.spawn (fun () ->
          for _ = 1 to total_items / 3 do
            match Ws_deque_.steal q with
            | Some _ -> incr stolen2
            | None -> ()
          done);

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let remaining = List.length (drain_remaining q) in
              remaining + !popped + !stolen1 + !stolen2 = total_items)))

(* queue pre-filled before any domain starts (mirrors a pool handing off a
   full local queue to be stolen from), then only stealers touch it *)
let prefilled_stealers () =
  Atomic.trace (fun () ->
      let q = Ws_deque_.create ~dummy () in
      let total_items = 3 in
      for i = 1 to total_items do
        ignore (Ws_deque_.push q i : bool)
      done;

      let stolen = Array.make total_items 0 in
      for k = 0 to total_items - 1 do
        Atomic.spawn (fun () ->
            match Ws_deque_.steal q with
            | Some _ -> stolen.(k) <- 1
            | None -> ())
      done;

      Atomic.final (fun () ->
          Atomic.check (fun () ->
              let n_stolen = Array.fold_left ( + ) 0 stolen in
              let remaining = List.length (drain_remaining q) in
              n_stolen + remaining = total_items)))

let () =
  owner_stealer ();
  owner_two_stealers ();
  prefilled_stealers ();
  print_endline "ws_deque_dscheck: all interleavings checked ok"
