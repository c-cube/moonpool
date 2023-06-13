open Moonpool

let ( let@ ) = ( @@ )

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree Fut.t * 'a tree Fut.t

let rec mk_tree ~pool n : _ tree Fut.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "mk-tree" in
  if n <= 1 then
    Fut.return (Leaf 1)
  else
    let open (val Fut.infix pool) in
    let l =
      Fut.spawn ~on:pool (fun () -> mk_tree ~pool (n - 1)) |> Fut.join ~on:pool
    and r =
      Fut.spawn ~on:pool (fun () -> mk_tree ~pool (n - 1)) |> Fut.join ~on:pool
    in

    Fut.return @@ Node (l, r)

let rec rev ~pool (t : 'a tree Fut.t) : 'a tree Fut.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "rev" in
  let open (val Fut.infix pool) in
  t >>= function
  | Leaf n -> Fut.return (Leaf n)
  | Node (l, r) ->
    let l = rev ~pool l and r = rev ~pool r in
    Fut.spawn ~on:pool (fun () -> Node (r, l))

let rec sum ~pool (t : int tree Fut.t) : int Fut.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "sum" in
  let open (val Fut.infix pool) in
  t >>= function
  | Leaf n -> Fut.return n
  | Node (l, r) ->
    let* l = sum ~pool l and* r = sum ~pool r in
    Fut.spawn ~on:pool (fun () -> l + r)

let run ~pool n : (int * int) Fut.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run" in
  let open (val Fut.infix pool) in
  let t = Fut.return n >>= mk_tree ~pool in
  let t' = rev ~pool t in
  let sum_t = sum ~pool t in
  let sum_t' = sum ~pool t' in
  Fut.both sum_t sum_t'

let default_n = 16

let stat_thread () =
  Moonpool.start_thread_on_some_domain
    (fun () ->
      while true do
        Thread.delay 0.1;
        let stat = Gc.quick_stat () in
        Trace.counter_int "gc.minor" stat.minor_collections;
        Trace.counter_int "gc.major" stat.major_collections;
        Trace.counter_float "gc.minor.words" stat.minor_words
      done)
    ()

let () =
  (*
  Tracy_client_trace.setup ();
   *)
  let pool = Pool.create ~per_domain:1 ~min:2 () in
  ignore (stat_thread () : Thread.t);

  let start = Unix.gettimeofday () in
  let n = try int_of_string (Sys.getenv "N") with _ -> default_n in

  Printf.printf "n=%d\n%!" n;
  let n1, n2 = run ~pool n |> Fut.wait_block_exn in
  Printf.printf "n: %d, n': %d (in %.2fs)\n%!" n1 n2
    (Unix.gettimeofday () -. start);
  assert (n1 = 1 lsl (n - 1));
  assert (n2 = 1 lsl (n - 1));
  ()
