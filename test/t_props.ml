module Q = QCheck
open Moonpool

let tests = ref []
let add_test t = tests := t :: !tests

(* main pool *)
let pool = Pool.create ~min:4 ~per_domain:1 ()

(* pool for future combinators *)
let pool_fut = Pool.create ~min:2 ()

module Fut2 = (val Fut.infix pool_fut)

let () =
  add_test
  @@ Q.Test.make ~name:"map then join_list"
       Q.(small_list small_int)
       (fun l ->
         let l' = List.map (fun x -> Fut.spawn ~on:pool (fun () -> x + 1)) l in
         let l' = Fut.join_list l' |> Fut.wait_block_exn in
         if l' <> List.map succ l then Q.Test.fail_reportf "bad list";
         true)

let () =
  add_test
  @@ Q.Test.make ~name:"map bind"
       Q.(small_list small_int)
       (fun l ->
         let open Fut2 in
         let l' =
           l
           |> List.map (fun x ->
                  let* x = Fut.spawn ~on:pool_fut (fun () -> x + 1) in
                  let* y = Fut.return (x - 1) in
                  let+ z = Fut.spawn ~on:pool_fut (fun () -> string_of_int y) in
                  z)
         in

         Fut.wait_list l' |> Fut.wait_block_exn;

         let l_res = List.map Fut.get_or_fail_exn l' in
         if l_res <> List.map string_of_int l then
           Q.Test.fail_reportf "bad list: from %s, to %s"
             Q.Print.(list int l)
             Q.Print.(list string l_res);
         true)

let () = QCheck_base_runner.run_tests_main !tests
