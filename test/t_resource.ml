open! Moonpool

let ( let@ ) = ( @@ )

let with_pool ~kind () f =
  match kind with
  | `Simple_pool -> Simple_pool.with_ ~min:4 ~per_domain:1 () f
  | `Pool -> Pool.with_ ~min:4 ~per_domain:1 () f

(* test proper resource handling *)
let run ~kind () =
  let@ () = Trace_tef.with_setup () in
  let a = Atomic.make 0 in
  for _i = 1 to 1_000 do
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "loop.step" in
    (* give a chance to domains to die *)
    if _i mod 100 = 0 then Thread.delay 0.8;

    (* allocate a new pool at each iteration *)
    let@ p = with_pool ~kind () in
    Pool.run_wait_block p (fun () -> Atomic.incr a)
  done;
  assert (Atomic.get a = 1_000)

let () =
  run ~kind:`Pool ();
  run ~kind:`Simple_pool ()
