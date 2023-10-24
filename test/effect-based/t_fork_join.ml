[@@@ifge 5.0]

let spf = Printf.sprintf
let ( let@ ) = ( @@ )

open! Moonpool

let pool = Pool.create ~min:4 ()

let () =
  let x =
    Pool.run_wait_block pool (fun () ->
        let x, y =
          Fork_join.both
            (fun () ->
              Thread.delay 0.005;
              1)
            (fun () ->
              Thread.delay 0.005;
              2)
        in
        x + y)
  in
  assert (x = 3)

let () =
  try
    Pool.run_wait_block pool (fun () ->
        Fork_join.both_ignore
          (fun () -> Thread.delay 0.005)
          (fun () ->
            Thread.delay 0.02;
            raise Exit));
    failwith "should fail"
  with Exit -> ()

let () =
  let par_sum =
    Pool.run_wait_block pool (fun () ->
        Fork_join.all_init 42 (fun i -> i * i) |> List.fold_left ( + ) 0)
  in
  let exp_sum = List.init 42 (fun x -> x * x) |> List.fold_left ( + ) 0 in
  assert (par_sum = exp_sum)

let () =
  Pool.run_wait_block pool (fun () ->
      Fork_join.for_ 0 (fun _ _ -> assert false));
  ()

let () =
  let total_sum = Atomic.make 0 in

  Pool.run_wait_block pool (fun () ->
      Fork_join.for_ ~chunk_size:5 100 (fun low high ->
          (* iterate on the range sequentially. The range should have 5 items or less. *)
          let local_sum = ref 0 in
          for i = low to high do
            local_sum := !local_sum + i
          done;
          ignore (Atomic.fetch_and_add total_sum !local_sum : int)));
  assert (Atomic.get total_sum = 4950)

let () =
  let total_sum = Atomic.make 0 in

  Pool.run_wait_block pool (fun () ->
      Fork_join.for_ ~chunk_size:1 100 (fun low high ->
          assert (low = high);
          ignore (Atomic.fetch_and_add total_sum low : int)));
  assert (Atomic.get total_sum = 4950)

(* ### prop tests ### *)

let rec fib_direct n =
  if n <= 2 then
    1
  else
    fib_direct (n - 1) + fib_direct (n - 2)

let rec fib_fork_join n =
  if n <= 10 then
    fib_direct n
  else (
    let a, b =
      Fork_join.both
        (fun () -> fib_fork_join (n - 1))
        (fun () -> fib_fork_join (n - 2))
    in
    a + b
  )

module Q = QCheck

module Evaluator = struct
  type fun_ =
    | Add_const of int
    | Neg
    | Mul_by_two

  type reducer =
    | R_add
    | R_max
    | R_add_shift

  type t =
    | Ret of int
    | Comp_fib of int
    | Add of t * t
    | Pipe of t * fun_
    | Map_arr of int * fun_ * t list * reducer

  let show_fun = function
    | Add_const n -> spf "add_const(%d)" n
    | Neg -> "neg"
    | Mul_by_two -> "mul(2)"

  let show_reducer = function
    | R_add -> "r_add"
    | R_max -> "r_max"
    | R_add_shift -> "r_add_shift"

  let rec size = function
    | Ret _ -> 1
    | Comp_fib _ -> 1
    | Add (a, b) -> 1 + size a + size b
    | Pipe (a, _) -> 1 + size a
    | Map_arr (_, _, l, _) -> 1 + List.fold_left (fun n x -> n + size x) 0 l

  let rec show = function
    | Ret x -> spf "ret(%d)" x
    | Comp_fib n -> spf "comp_fib(%d)" n
    | Add (a, b) -> spf "add(%s,%s)" (show a) (show b)
    | Pipe (a, f) -> spf "%s |> %s" (show a) (show_fun f)
    | Map_arr (csize, f, l, r) ->
      spf "map_array(csize=%d, %s, [%s], %s)" csize (show_fun f)
        (String.concat ";" @@ List.map show l)
        (show_reducer r)

  let shrink_fun =
    Q.Iter.(
      function
      | Add_const i ->
        let+ x = Q.Shrink.int i in
        Add_const x
      | Neg | Mul_by_two -> empty)

  let rec shrink =
    Q.Iter.(
      function
      | Ret n ->
        let+ n = Q.Shrink.int n in
        Ret n
      | Comp_fib n ->
        return (Ret n)
        <+> let+ n = Q.Shrink.int n in
            Comp_fib n
      | Pipe (a, f) ->
        (let+ a = shrink a in
         Pipe (a, f))
        <+> let+ f = shrink_fun f in
            Pipe (a, f)
      | Add (a, b) ->
        return a <+> return b
        <+> (let+ a = shrink a in
             Add (a, b))
        <+> let+ b = shrink b in
            Add (a, b)
      | Map_arr (csize, f, l, r) ->
        (let+ l = Q.Shrink.list ~shrink l in
         Map_arr (csize, f, l, r))
        <+> let+ f = shrink_fun f in
            Map_arr (csize, f, l, r))

  let gen_fun =
    Q.Gen.(
      frequency
        [
          ( 2,
            let+ n = 0 -- 100 in
            Add_const n );
          1, return Neg;
          1, return Mul_by_two;
        ])

  let rec gen n : t Q.Gen.t =
    Q.Gen.delay @@ fun () ->
    assert (n >= 0);
    let clamp_if_base x =
      if n <= 1 then
        0
      else
        abs x
    in
    let open Q.Gen in
    frequency
      [
        ( 1,
          let+ x = 1 -- 10000 in
          Ret x );
        ( 4,
          let+ x = 3 -- 16 in
          Comp_fib x );
        ( clamp_if_base 7,
          let+ f = gen_fun and+ a = gen (max 1 (n - 1)) in
          Pipe (a, f) );
        ( clamp_if_base 3,
          let+ a = gen (min 4 (n - 1)) and+ b = gen (min 4 (n - 1)) in
          Add (a, b) );
        ( clamp_if_base 3,
          let+ f = gen_fun
          and+ csize = 1 -- 16
          and+ l = list_size (0 -- 290) (gen 1)
          and+ r = oneofl [ R_add; R_max; R_add_shift ] in
          Map_arr (csize, f, l, r) );
        ( clamp_if_base 2,
          let+ f = gen_fun
          and+ csize = 1 -- 3
          and+ l = list_size (1 -- 7) (gen (min 3 (n - 1)))
          and+ r = oneofl [ R_add; R_max; R_add_shift ] in
          Map_arr (csize, f, l, r) );
      ]

  let arb : t Q.arbitrary =
    Q.make ~print:show ~shrink
      Q.Gen.(
        let* n = 1 -- 16 in
        gen n)

  let apply_fun_seq f o =
    match f with
    | Add_const x -> o + x
    | Neg -> -o
    | Mul_by_two -> 2 * o

  let eval_reducer r l =
    List.fold_left
      (fun acc x ->
        match r with
        | R_add -> acc + x
        | R_max -> max acc x
        | R_add_shift -> (acc * 10) + x)
      0 l

  let rec eval_seq : t -> int = function
    | Ret x -> x
    | Comp_fib n -> fib_direct n
    | Add (a, b) -> eval_seq a + eval_seq b
    | Pipe (a, f) -> eval_seq a |> apply_fun_seq f
    | Map_arr (_, f, a, r) ->
      a |> List.map eval_seq |> List.map (apply_fun_seq f) |> eval_reducer r

  let eval_fork_join ~pool e : int =
    let rec eval = function
      | Ret x -> x
      | Comp_fib n -> fib_fork_join n
      | Add (a, b) ->
        let a, b = Fork_join.both (fun () -> eval a) (fun () -> eval b) in
        a + b
      | Pipe (a, f) -> eval a |> apply_fun_seq f
      | Map_arr (chunk_size, f, a, r) ->
        let tasks = List.map (fun x () -> eval x) a in
        Fork_join.all_list ~chunk_size tasks
        |> Fork_join.map_list ~chunk_size (apply_fun_seq f)
        |> eval_reducer r
    in

    Runner.run_wait_block pool (fun () -> eval e)
end

let t_eval =
  let arb = Q.set_stats [ "size", Evaluator.size ] Evaluator.arb in
  Q.Test.make ~name:"same eval" arb (fun e ->
      let@ pool = Pool.with_ ~min:4 () in
      (* Printf.eprintf "eval %s\n%!" (Evaluator.show e); *)
      let x = Evaluator.eval_seq e in
      let y = Evaluator.eval_fork_join ~pool e in
      if x <> y then Q.Test.fail_reportf "expected %d, got %d" x y;
      true)

let t_for_nested ~min ~chunk_size () =
  let ppl = Q.Print.(list @@ list int) in
  let neg x = -x in
  Q.Test.make
    ~name:(spf "t_for_nested ~min:%d" min)
    Q.(small_list (small_list small_int))
    (fun l ->
      let ref_l1 = List.map (List.map neg) l in
      let ref_l2 = List.map (List.map neg) ref_l1 in

      let l1, l2 =
        let@ pool = Pool.with_ ~min () in
        let@ () = Pool.run_wait_block pool in
        let l1 =
          Fork_join.map_list ~chunk_size (Fork_join.map_list ~chunk_size neg) l
        in
        let l2 =
          Fork_join.map_list ~chunk_size (Fork_join.map_list ~chunk_size neg) l1
        in
        l1, l2
      in

      if l1 <> ref_l1 then
        Q.Test.fail_reportf "l1=%s, ref_l1=%s" (ppl l1) (ppl ref_l1);
      if l2 <> ref_l2 then
        Q.Test.fail_reportf "l1=%s, ref_l1=%s" (ppl l2) (ppl ref_l2);
      true)

let t_map ~chunk_size () =
  let ppa = Q.Print.(array string) in
  Q.Test.make ~name:"map1"
    Q.(small_list small_int |> Q.set_stats [ "len", List.length ])
    (fun l ->
      let@ pool = Pool.with_ ~min:4 () in
      let@ () = Pool.run_wait_block pool in

      let a1 =
        Fork_join.map_list ~chunk_size string_of_int l |> Array.of_list
      in
      let a2 =
        Fork_join.map_array ~chunk_size string_of_int @@ Array.of_list l
      in

      if a1 <> a2 then Q.Test.fail_reportf "a1=%s, a2=%s" (ppa a1) (ppa a2);
      true)

let () =
  QCheck_base_runner.run_tests_main
    [
      t_eval;
      t_map ~chunk_size:1 ();
      t_map ~chunk_size:50 ();
      t_for_nested ~min:1 ~chunk_size:1 ();
      t_for_nested ~min:4 ~chunk_size:1 ();
      t_for_nested ~min:1 ~chunk_size:3 ();
      t_for_nested ~min:4 ~chunk_size:3 ();
      t_for_nested ~min:1 ~chunk_size:100 ();
      t_for_nested ~min:4 ~chunk_size:100 ();
    ]

[@@@endif]
