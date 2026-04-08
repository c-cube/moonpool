open struct
  module TLS = Moonpool.Task_local_storage
end

let storage : Ambient_context.Storage.t =
  {
    name = "moonpool";
    get_context = TLS.get_local_hmap;
    with_context =
      (fun new_hmap f ->
        let old = TLS.get_local_hmap () in
        TLS.set_local_hmap new_hmap;
        match f () with
        | x ->
          TLS.set_local_hmap old;
          x
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          TLS.set_local_hmap old;
          Printexc.raise_with_backtrace exn bt);
  }
