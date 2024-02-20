open Moonpool
module Trace = Trace_core
module FJ = Moonpool_forkjoin

let ( let@ ) = ( @@ )

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
    let open Fut.Infix in
    let+ t1 = fib ~on (x - 1) and+ t2 = fib ~on (x - 2) in
    t1 + t2

let fib_fj ~on x : int Fut.t =
  let rec fib_rec x : int =
    if x <= !cutoff then
      fib_direct x
    else (
      let n1, n2 =
        FJ.both (fun () -> fib_rec (x - 1)) (fun () -> fib_rec (x - 2))
      in
      n1 + n2
    )
  in
  Fut.spawn ~on (fun () -> fib_rec x)

let fib_await ~on x : int Fut.t =
  let rec fib_rec x : int Fut.t =
    if x <= !cutoff then
      Fut.spawn ~on (fun () -> fib_direct x)
    else
      Fut.spawn ~on (fun () ->
          let n1 = fib_rec (x - 1) in
          let n2 = fib_rec (x - 2) in
          let n1 = Fut.await n1 in
          let n2 = Fut.await n2 in
          n1 + n2)
  in
  fib_rec x

let rec fib_dl ~pool x : int Domainslib.Task.promise =
  if x <= !cutoff then
    Domainslib.Task.async pool (fun () -> fib_direct x)
  else
    Domainslib.Task.async pool (fun () ->
        let t1 = fib_dl ~pool (x - 1) and t2 = fib_dl ~pool (x - 2) in
        let t1 = Domainslib.Task.await pool t1 in
        let t2 = Domainslib.Task.await pool t2 in
        t1 + t2)

let () = assert (List.init 10 fib_direct = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])

let create_pool ~psize ~kind () =
  match kind with
  | "fifo" -> Fifo_pool.create ?num_threads:psize ()
  | "pool" -> Ws_pool.create ?num_threads:psize ()
  | _ -> assert false

let str_of_int_opt = function
  | None -> "None"
  | Some i -> Printf.sprintf "Some %d" i

let run ~psize ~n ~seq ~dl ~fj ~await ~niter ~kind () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fib.run" in
  let pool = lazy (create_pool ~kind ~psize ()) in
  let dl_pool =
    lazy
      (let n = Domain.recommended_domain_count () in
       Printf.printf "use %d domains\n%!" n;
       Domainslib.Task.setup_pool ~num_domains:n ())
  in
  for _i = 1 to niter do
    let res =
      if seq then (
        Printf.printf "compute fib %d sequentially\n%!" n;
        fib_direct n
      ) else if dl then (
        Printf.printf "compute fib %d with domainslib\n%!" n;
        let (lazy pool) = dl_pool in
        Domainslib.Task.run pool (fun () ->
            Domainslib.Task.await pool @@ fib_dl ~pool n)
      ) else if fj then (
        Printf.printf "compute fib %d using fork-join with pool size=%s\n%!" n
          (str_of_int_opt psize);
        fib_fj ~on:(Lazy.force pool) n |> Fut.wait_block_exn
      ) else if await then (
        Printf.printf "compute fib %d using await with pool size=%s\n%!" n
          (str_of_int_opt psize);
        fib_await ~on:(Lazy.force pool) n |> Fut.wait_block_exn
      ) else (
        Printf.printf "compute fib %d with pool size=%s\n%!" n
          (str_of_int_opt psize);
        fib ~on:(Lazy.force pool) n |> Fut.wait_block_exn
      )
    in
    Printf.printf "fib %d = %d\n%!" n res
  done;

  if seq then
    ()
  else if dl then
    Domainslib.Task.teardown_pool (Lazy.force dl_pool)
  else
    Ws_pool.shutdown (Lazy.force pool)

let () =
  let@ () = Trace_tef.with_setup () in
  let n = ref 40 in
  let psize = ref None in
  let seq = ref false in
  let niter = ref 3 in
  let kind = ref "pool" in
  let dl = ref false in
  let await = ref false in
  let fj = ref false in
  let opts =
    [
      "-psize", Arg.Int (fun i -> psize := Some i), " pool size";
      "-n", Arg.Set_int n, " fib <n>";
      "-seq", Arg.Set seq, " sequential";
      "-dl", Arg.Set dl, " domainslib";
      "-fj", Arg.Set fj, " fork join";
      "-niter", Arg.Set_int niter, " number of iterations";
      "-await", Arg.Set await, " use await";
      "-cutoff", Arg.Set_int cutoff, " cutoff for sequential computation";
      ( "-kind",
        Arg.Symbol ([ "pool"; "fifo" ], ( := ) kind),
        " pick pool implementation" );
    ]
    |> Arg.align
  in

  Arg.parse opts ignore "";
  run ~psize:!psize ~n:!n ~fj:!fj ~seq:!seq ~await:!await ~dl:!dl ~niter:!niter
    ~kind:!kind ()
