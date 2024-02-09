let num_threads ?num_threads () : int =
  let n_domains = Domain_pool_.n_domains () in

  (* number of threads to run *)
  let num_threads =
    match num_threads with
    | Some j -> max 1 j
    | None -> n_domains
  in

  num_threads
