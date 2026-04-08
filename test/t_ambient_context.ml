open Moonpool

let () = Ambient_context.set_current_storage Moonpool_ambient_context.storage
let key_a : string Ambient_context.Context.key = Ambient_context.new_key ()
let key_b : int Ambient_context.Context.key = Ambient_context.new_key ()
let get_a () = Ambient_context.get key_a
let get_b () = Ambient_context.get key_b

let () =
  let pool = Ws_pool.create ~num_threads:4 () in

  (* basic: values set in a task are visible within that task *)
  let fut =
    Fut.spawn ~on:pool (fun () ->
        assert (get_a () = None);
        Ambient_context.with_key_bound_to key_a "hello" (fun () ->
            assert (get_a () = Some "hello");
            (* nested: inner binding shadows outer *)
            Ambient_context.with_key_bound_to key_a "world" (fun () ->
                assert (get_a () = Some "world"));
            (* restored after inner scope *)
            assert (get_a () = Some "hello"));
        assert (get_a () = None))
  in
  Fut.wait_block_exn fut;

  (* two keys are independent *)
  let fut =
    Fut.spawn ~on:pool (fun () ->
        Ambient_context.with_key_bound_to key_a "foo" (fun () ->
            Ambient_context.with_key_bound_to key_b 42 (fun () ->
                assert (get_a () = Some "foo");
                assert (get_b () = Some 42));
            assert (get_b () = None);
            assert (get_a () = Some "foo")))
  in
  Fut.wait_block_exn fut;

  (* child tasks do not inherit the parent's ambient context *)
  let fut =
    Fut.spawn ~on:pool (fun () ->
        Ambient_context.with_key_bound_to key_a "parent" (fun () ->
            let child =
              Fut.spawn ~on:pool (fun () ->
                  (* child starts with empty context *)
                  assert (get_a () = None))
            in
            Fut.wait_block_exn child))
  in
  Fut.wait_block_exn fut;

  Ws_pool.shutdown pool;
  Printf.printf "all assertions passed\n%!"
