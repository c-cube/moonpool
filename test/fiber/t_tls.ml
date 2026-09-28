open! Moonpool

let ( let@ ) = ( @@ )

let () =
  let@ _ = Moonpool.main in
  let k : int Task_local_storage.t = Task_local_storage.create () in
  Task_local_storage.with_value k 42 (fun () ->
      assert (Task_local_storage.get_opt k = Some 42));
  assert (Task_local_storage.get_opt k = None);

  (try Task_local_storage.with_value k 1 (fun () -> failwith "oops")
   with Failure _ -> ());
  assert (Task_local_storage.get_opt k = None)
