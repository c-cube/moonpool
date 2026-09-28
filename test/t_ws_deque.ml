module A = Moonpool.Atomic
module D = Moonpool.Private.Ws_deque_

let ( let@ ) = ( @@ )
let dummy = -100

let t_simple () =
  let d = D.create ~dummy () in
  assert (D.steal d = None);
  assert (D.pop d = None);
  assert (D.push d 1);
  assert (D.push d 2);
  assert (D.pop d = Some 2);
  assert (D.steal d = Some 1);
  assert (D.steal d = None);
  assert (D.pop d = None);
  assert (D.push d 3);
  assert (D.pop d = Some 3);
  assert (D.push d 4);
  assert (D.push d 5);
  assert (D.push d 6);
  assert (D.steal d = Some 4);
  assert (D.steal d = Some 5);
  assert (D.pop d = Some 6);
  assert (D.pop d = None);

  Printf.printf "basic tests passed\n";
  ()

(* big heavy test *)
let t_heavy () =
  let sum = A.make 0 in
  let ref_sum = ref 0 in

  let[@inline] add_to_sum x = ignore (A.fetch_and_add sum x : int) in

  let active = A.make true in

  let d = D.create ~dummy () in

  let stealer_loop () =
    Trace.set_thread_name "stealer";
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "stealer" in
    while A.get active do
      match D.steal d with
      | None -> Thread.yield ()
      | Some x -> add_to_sum x
    done
  in

  let main_loop () =
    Trace.set_thread_name "producer";
    for _i = 1 to 100_000 do
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main.outer" in

      (* NOTE: we make sure to push less than 256 elements at once *)
      for j = 1 to 100 do
        ref_sum := !ref_sum + j;
        assert (D.push d j);
        ref_sum := !ref_sum + j;
        assert (D.push d j);

        Option.iter (fun x -> add_to_sum x) (D.pop d);
        Option.iter (fun x -> add_to_sum x) (D.pop d)
      done;

      (* now compete with stealers to pop *)
      let continue = ref true in
      while !continue do
        match D.pop d with
        | Some x -> add_to_sum x
        | None -> continue := false
      done
    done
  in

  let ts =
    Array.init 6 (fun _ -> Moonpool.start_thread_on_some_domain stealer_loop ())
  in
  let t = Moonpool.start_thread_on_some_domain main_loop () in

  (* wait for the producer to finish *before* telling stealers to stop, so
     they actually race against it for the whole run instead of exiting
     almost immediately *)
  Trace.message "joining t";
  Thread.join t;

  (* stop *)
  A.set active false;

  Trace.message "joining stealers";
  Array.iter Thread.join ts;
  Trace.message "done";

  let ref_sum = !ref_sum in
  let sum = A.get sum in

  Printf.printf "ref sum = %d, sum = %d\n%!" ref_sum sum;
  assert (ref_sum = sum);
  ()

(* stolen values must not be retained by the deque *)
let t_gc () =
  let n = 200 in
  let[@inline never] push_and_steal d w =
    for i = 0 to n - 1 do
      let r = ref i in
      Weak.set w i (Some r);
      assert (D.push d r)
    done;
    for _ = 1 to n do
      assert (Option.is_some (D.steal d))
    done
  in
  let check_collected d w =
    Gc.full_major ();
    for i = 0 to n - 1 do
      assert (not (Weak.check w i))
    done;
    ignore (Sys.opaque_identity d : _ D.t)
  in

  (* cleared on [pop] *)
  let d = D.create ~dummy:(ref dummy) () in
  let w = Weak.create n in
  push_and_steal d w;
  assert (D.pop d = None);
  check_collected d w;

  (* cleared on [push] when it needs to refresh [top] *)
  let d = D.create ~dummy:(ref dummy) () in
  let w = Weak.create n in
  push_and_steal d w;
  for i = 1 to 256 - n do
    assert (D.push d (ref i))
  done;
  check_collected d w;

  Printf.printf "gc tests passed\n";
  ()

let () =
  let@ () = Trace_tef.with_setup () in
  t_simple ();
  t_gc ();
  t_heavy ();
  ()
