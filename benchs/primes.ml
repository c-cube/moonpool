let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let generate' chan =
  for i = 2 to Int.max_int do
    Moonpool.Chan.push chan i
  done

let filter' in_chan out_chan prime =
  let rec loop () =
    let n = Moonpool.Chan.pop in_chan in
    if n mod prime <> 0 then Moonpool.Chan.push out_chan n;
    loop ()
  in
  loop ()

let main ~n ~on_prime () : unit =
  let@ runner = Moonpool.Ws_pool.with_ () in
  let@ () = Moonpool.Ws_pool.run_wait_block runner in
  let primes = ref @@ Moonpool.Chan.create ~max_size:32 () in
  Moonpool.run_async runner
    (let chan = !primes in
     fun () -> generate' chan);

  for _i = 1 to n do
    let prime = Moonpool.Chan.pop !primes in
    on_prime prime;
    let filtered_chan = Moonpool.Chan.create ~max_size:32 () in
    Moonpool.run_async runner
      (let in_chan = !primes in
       fun () -> filter' in_chan filtered_chan prime);
    primes := filtered_chan
  done

let () =
  let n = ref 10_000 in
  let time = ref true in
  let opts =
    [
      "-n", Arg.Set_int n, " number of iterations";
      "--no-time", Arg.Clear time, " do not compute time";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "";
  Printf.printf "computing %d primes\n%!" !n;

  let t_start = Unix.gettimeofday () in

  let n_primes = Atomic.make 0 in
  main ~n:!n ~on_prime:(fun _ -> Atomic.incr n_primes) ();

  let elapsed : float = Unix.gettimeofday () -. t_start in
  Printf.printf "computed %d primes%s\n%!" (Atomic.get n_primes)
    (if !time then
       spf " in %.4fs" elapsed
     else
       "")
