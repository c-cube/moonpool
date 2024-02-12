open Moonpool

let ( let@ ) = ( @@ )

let k_ints : int list Task_local_storage.key =
  Task_local_storage.new_key ~init:(fun () -> []) ()

let rec task idx i : unit Fut.t =
  let open Fut.Infix in
  if i = 0 then
    Fut.return ()
  else (
    let l = Task_local_storage.get k_ints in
    Task_local_storage.set k_ints @@ (((idx * 100) + i) :: l);
    let* () = task idx (i - 1) in

    let l = Task_local_storage.get k_ints in
    Task_local_storage.set k_ints @@ (((idx * 1000) + i) :: l);
    Fut.return ()
  )

let run ~on idx = Fut.spawn ~on (fun () -> task idx 10)

let () =
  let@ runner = Ws_pool.with_ ~num_threads:4 () in
  let tasks =
    List.init 8 (fun idx ->
        let open Fut.Infix in
        let+ () = run ~on:runner idx |> Fut.join in
        (* return final value of TLS *)
        let l = Task_local_storage.get k_ints in
        l)
  in

  let res = List.map Fut.wait_block_exn tasks in
  List.iteri
    (fun i l ->
      Printf.printf "res(%d)=[%s]\n" i
        (String.concat "," @@ List.map string_of_int l))
    res;

  ()
