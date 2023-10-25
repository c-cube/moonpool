open Moonpool

let rec fib_direct x =
  if x <= 1 then
    1
  else
    fib_direct (x - 1) + fib_direct (x - 2)

let cutoff = ref 20

let rec fib ~on x : int Fut.t =
  if x <= !cutoff then
    Fut.spawn ~on (fun () -> fib_direct x)
  else
    let open Fut.Infix_local in
    let+ t1 = fib ~on (x - 1) and+ t2 = fib ~on (x - 2) in
    t1 + t2

let () = assert (List.init 10 fib_direct = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])

let create_pool ~psize ~kind () =
  match kind with
  | "simple" -> Simple_pool.create ~min:psize ()
  | "pool" -> Pool.create ~min:psize ()
  | _ -> assert false

let run ~psize ~n ~seq ~niter ~kind () : unit =
  let pool = lazy (create_pool ~kind ~psize ()) in
  for _i = 1 to niter do
    let res =
      if seq then (
        Printf.printf "compute fib %d sequentially\n%!" n;
        fib_direct n
      ) else (
        Printf.printf "compute fib %d with pool size=%d\n%!" n psize;
        fib ~on:(Lazy.force pool) n |> Fut.wait_block_exn
      )
    in
    Printf.printf "fib %d = %d\n%!" n res
  done;
  if not seq then Pool.shutdown (Lazy.force pool)

let () =
  let n = ref 40 in
  let psize = ref 16 in
  let seq = ref false in
  let niter = ref 3 in
  let kind = ref "pool" in
  let opts =
    [
      "-psize", Arg.Set_int psize, " pool size";
      "-n", Arg.Set_int n, " fib <n>";
      "-seq", Arg.Set seq, " sequential";
      "-niter", Arg.Set_int niter, " number of iterations";
      "-cutoff", Arg.Set_int cutoff, " cutoff for sequential computation";
      ( "-kind",
        Arg.Symbol ([ "pool"; "simple" ], ( := ) kind),
        " pick pool implementation" );
    ]
    |> Arg.align
  in

  Arg.parse opts ignore "";
  run ~psize:!psize ~n:!n ~seq:!seq ~niter:!niter ~kind:!kind ()
