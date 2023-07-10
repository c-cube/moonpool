(* compute Pi *)

open Moonpool

let ( let@ ) = ( @@ )
let j = ref 0
let spf = Printf.sprintf

let run_sequential (num_steps : int) : float =
  let step = 1. /. float num_steps in
  let sum = ref 0. in
  for i = 0 to num_steps - 1 do
    let x = (float i +. 0.5) *. step in
    sum := !sum +. (4. /. (1. +. (x *. x)))
  done;
  let pi = step *. !sum in
  pi

(** Create a pool *)
let with_pool f =
  if !j = 0 then
    Pool.with_ ~per_domain:1 f
  else
    Pool.with_ ~min:!j f

(** Run in parallel using {!Fut.for_} *)
let run_par1 (num_steps : int) : float =
  let@ pool = with_pool () in

  let num_tasks = Pool.size pool in

  let step = 1. /. float num_steps in
  let global_sum = Lock.create 0. in

  (* one chunk of the work *)
  let run_task _idx_task : unit =
    let sum = ref 0. in
    let i = ref 0 in
    while !i < num_steps do
      let x = (float !i +. 0.5) *. step in
      sum := !sum +. (4. /. (1. +. (x *. x)));
      (* next iteration *) i := !i + num_tasks
    done;

    let sum = !sum in
    Lock.update global_sum (fun x -> x +. sum)
  in

  Fut.wait_block_exn @@ Fut.for_ ~on:pool num_tasks run_task;

  let pi = step *. Lock.get global_sum in
  pi

[@@@ifge 5.0]

let run_fork_join num_steps : float =
  let@ pool = with_pool () in

  let num_tasks = Pool.size pool in

  let step = 1. /. float num_steps in
  let global_sum = Lock.create 0. in

  Pool.run_wait_block pool (fun () ->
      Fork_join.for_
        ~chunk_size:(3 + (num_steps / num_tasks))
        num_steps
        (fun range ->
          let sum = ref 0. in
          range (fun i ->
              let x = (float i +. 0.5) *. step in
              sum := !sum +. (4. /. (1. +. (x *. x))));

          let sum = !sum in
          Lock.update global_sum (fun n -> n +. sum)));

  let pi = step *. Lock.get global_sum in
  pi

[@@@else_]

let run_fork_join _ =
  failwith "fork join not available on this version of OCaml"

[@@@endif]

type mode =
  | Sequential
  | Par1
  | Fork_join

let () =
  let mode = ref Sequential in
  let n = ref 1000 in
  let time = ref false in

  let set_mode = function
    | "seq" -> mode := Sequential
    | "par1" -> mode := Par1
    | "forkjoin" -> mode := Fork_join
    | _s -> failwith (spf "unknown mode %S" _s)
  in

  let opts =
    [
      "-n", Arg.Set_int n, " number of steps";
      ( "-mode",
        Arg.Symbol ([ "seq"; "par1"; "forkjoin" ], set_mode),
        " mode of execution" );
      "-j", Arg.Set_int j, " number of threads";
      "-t", Arg.Set time, " printing timing";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "";

  let t_start = Unix.gettimeofday () in
  let res =
    match !mode with
    | Sequential -> run_sequential !n
    | Par1 -> run_par1 !n
    | Fork_join -> run_fork_join !n
  in
  let elapsed : float = Unix.gettimeofday () -. t_start in

  Printf.printf "pi=%.6f (pi=%.6f, diff=%.3f)%s\n%!" res Float.pi
    (abs_float (Float.pi -. res))
    (if !time then
      spf " in %.4fs" elapsed
    else
      "");
  ()
