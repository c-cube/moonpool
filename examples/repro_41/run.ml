(* fibo.ml *)
let cutoff = 25
let input = 40

let rec fibo_seq n =
  if n <= 1 then
    n
  else
    fibo_seq (n - 1) + fibo_seq (n - 2)

let rec fibo_domainslib ctx n =
  if n <= cutoff then
    fibo_seq n
  else
    let open Domainslib in
    let fut1 = Task.async ctx (fun () -> fibo_domainslib ctx (n - 1)) in
    let fut2 = Task.async ctx (fun () -> fibo_domainslib ctx (n - 2)) in
    Task.await ctx fut1 + Task.await ctx fut2

let rec fibo_moonpool ctx n =
  if n <= cutoff then
    fibo_seq n
  else
    let open Moonpool in
    let fut1 = Fut.spawn ~on:ctx (fun () -> fibo_moonpool ctx (n - 1)) in
    let fut2 = Fut.spawn ~on:ctx (fun () -> fibo_moonpool ctx (n - 2)) in
    Fut.await fut1 + Fut.await fut2

let usage =
  "fibo.exe <num_domains> [ domainslib | moonpool | moonpool_fifo | seq ]"

let num_domains = try int_of_string Sys.argv.(1) with _ -> failwith usage
let implem = try Sys.argv.(2) with _ -> failwith usage

let () =
  let output =
    match implem with
    | "moonpool" ->
      let open Moonpool in
      let ctx = Ws_pool.create ~num_threads:num_domains () in
      Ws_pool.run_wait_block ctx (fun () -> fibo_moonpool ctx input)
    | "moonpool_fifo" ->
      let open Moonpool in
      let ctx = Fifo_pool.create ~num_threads:num_domains () in
      Ws_pool.run_wait_block ctx (fun () -> fibo_moonpool ctx input)
    | "domainslib" ->
      let open Domainslib in
      let pool = Task.setup_pool ~num_domains () in
      Task.run pool (fun () -> fibo_domainslib pool input)
    | "seq" -> fibo_seq input
    | _ -> failwith usage
  in
  print_int output;
  print_newline ()
