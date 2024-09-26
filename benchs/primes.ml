let ( let@ ) = ( @@ )

let generate' chan =
  for i = 2 to Int.max_int do
    Moonpool.Chan.push chan i;
    Thread.yield ()
  done

let filter' in_chan out_chan prime =
  let rec loop () =
    let n = Moonpool.Chan.pop_await in_chan in
    if n mod prime <> 0 then Moonpool.Chan.push out_chan n;
    loop ()
  in
  loop ()

let () =
  let@ runner = Moonpool.Ws_pool.with_ () in
  let@ () = Moonpool.Ws_pool.run_wait_block runner in
  let primes = ref @@ Moonpool.Chan.create () in
  Moonpool.run_async runner
    (let chan = !primes in
     fun () -> generate' chan);
  for _i = 1 to 10 do
    let prime = Moonpool.Chan.pop_await !primes in
    Format.printf "%d\n@?" prime;
    let filtered_chan = Moonpool.Chan.create () in
    Moonpool.run_async runner
      (let in_chan = !primes in
       fun () -> filter' in_chan filtered_chan prime);
    primes := filtered_chan
  done
