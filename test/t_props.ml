module Q = QCheck
open Moonpool

let ( let@ ) = ( @@ )
let tests = ref []
let add_test t = tests := t :: !tests

let with_pool ~kind () f =
  match kind with
  | `Fifo_pool -> Fifo_pool.with_ () f
  | `Ws_pool -> Ws_pool.with_ () f

let () =
  add_test @@ fun ~kind ->
  Q.Test.make ~name:"map then join_list"
    Q.(small_list small_int)
    (fun l ->
      let@ pool = with_pool ~kind () in
      let l' = List.map (fun x -> Fut.spawn ~on:pool (fun () -> x + 1)) l in
      let l' = Fut.join_list l' |> Fut.wait_block_exn in
      if l' <> List.map succ l then Q.Test.fail_reportf "bad list";
      true)

let () =
  add_test @@ fun ~kind ->
  Q.Test.make ~name:"map bind"
    Q.(small_list small_int)
    (fun l ->
      let@ pool = with_pool ~kind () in
      let open Fut.Infix in
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
    List.map (fun t -> [ t ~kind:`Fifo_pool; t ~kind:`Ws_pool ]) !tests
    |> List.flatten
  in
  QCheck_base_runner.run_tests_main tests
