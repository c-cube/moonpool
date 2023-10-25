module Q = QCheck
open Moonpool

let ( let@ ) = ( @@ )
let tests = ref []
let add_test t = tests := t :: !tests

let with_pool ~kind () f =
  match kind with
  | `Simple_pool -> Simple_pool.with_ ~min:4 ~per_domain:1 () f
  | `Pool -> Pool.with_ ~min:4 ~per_domain:1 () f

let () =
  add_test @@ fun ~kind ->
  let@ pool = with_pool ~kind () in
  Q.Test.make ~name:"map then join_list"
    Q.(small_list small_int)
    (fun l ->
      let l' = List.map (fun x -> Fut.spawn ~on:pool (fun () -> x + 1)) l in
      let l' = Fut.join_list l' |> Fut.wait_block_exn in
      if l' <> List.map succ l then Q.Test.fail_reportf "bad list";
      true)

let () =
  add_test @@ fun ~kind ->
  let@ pool = with_pool ~kind () in
  Q.Test.make ~name:"map bind"
    Q.(small_list small_int)
    (fun l ->
      let open Fut.Infix_local in
      let l' =
        l
        |> List.map (fun x ->
               let* x = Fut.spawn ~on:pool (fun () -> x + 1) in
               let* y = Fut.return (x - 1) in
               let+ z = Fut.spawn ~on:pool (fun () -> string_of_int y) in
               z)
      in

      Fut.wait_list l' |> Fut.wait_block_exn;

      let l_res = List.map Fut.get_or_fail_exn l' in
      if l_res <> List.map string_of_int l then
        Q.Test.fail_reportf "bad list: from %s, to %s"
          Q.Print.(list int l)
          Q.Print.(list string l_res);
      true)

let () =
  let tests =
    List.map (fun t -> [ t ~kind:`Simple_pool; t ~kind:`Pool ]) !tests
    |> List.flatten
  in
  QCheck_base_runner.run_tests_main tests
