(* compute Pi *)

open Moonpool
module FJ = Moonpool_forkjoin

let ( let@ ) = ( @@ )
let j = ref 0
let spf = Printf.sprintf

let run_sequential (num_steps : int) : float =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "pi.seq" in
  let step = 1. /. float num_steps in
  let sum = ref 0. in
  for i = 0 to num_steps - 1 do
    let x = (float i +. 0.5) *. step in
    sum := !sum +. (4. /. (1. +. (x *. x)))
  done;
  let pi = step *. !sum in
  pi

(** Create a pool *)
let with_pool ~kind f =
  match kind with
  | "pool" ->
    if !j = 0 then
      Ws_pool.with_ f
    else
      Ws_pool.with_ ~num_threads:!j f
  | "fifo" ->
    if !j = 0 then
      Fifo_pool.with_ f
    else
      Fifo_pool.with_ ~num_threads:!j f
  | _ -> assert false

(** Run in parallel using {!Fut.for_} *)
let run_par1 ~kind (num_steps : int) : float =
  let@ pool = with_pool ~kind () in

  let num_tasks = Ws_pool.size pool in

  let step = 1. /. float num_steps in
  let global_sum = Lock.create 0. in

  (* one chunk of the work *)
  let run_task _idx_task : unit =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__ "pi.slice" ~data:(fun () ->
          [ "i", `Int _idx_task ])
    in

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

let run_fork_join ~kind num_steps : float =
  let@ pool = with_pool ~kind () in

  let num_tasks = Ws_pool.size pool in

  let step = 1. /. float num_steps in
  let global_sum = Lock.create 0. in

  Ws_pool.run_wait_block pool (fun () ->
      FJ.for_
        ~chunk_size:(3 + (num_steps / num_tasks))
        num_steps
        (fun low high ->
          let sum = ref 0. in
          for i = low to high do
            let x = (float i +. 0.5) *. step in
            sum := !sum +. (4. /. (1. +. (x *. x)))
          done;
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
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let mode = ref Sequential in
  let n = ref 1000 in
  let time = ref false in
  let kind = ref "pool" in

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
      ( "-kind",
        Arg.Symbol ([ "pool"; "fifo" ], ( := ) kind),
        " pick pool implementation" );
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "";

  let t_start = Unix.gettimeofday () in
  let res =
    match !mode with
    | Sequential -> run_sequential !n
    | Par1 -> run_par1 ~kind:!kind !n
    | Fork_join -> run_fork_join ~kind:!kind !n
  in
  let elapsed : float = Unix.gettimeofday () -. t_start in

  Printf.printf "pi=%.6f (pi=%.6f, diff=%.3f)%s\n%!" res Float.pi
    (abs_float (Float.pi -. res))
    (if !time then
      spf " in %.4fs" elapsed
    else
      "");
  ()
