open Types_
include Runner

let run_async_ ~ls:cur_ls f =
  TLS.get k_cur_storage := Some cur_ls;
  try
    let x = f () in
    TLS.get k_cur_storage := None;
    x
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    TLS.get k_cur_storage := None;
    Printexc.raise_with_backtrace e bt

let runner : t =
  Runner.For_runner_implementors.create
    ~size:(fun () -> 0)
    ~num_tasks:(fun () -> 0)
    ~shutdown:(fun ~wait:_ () -> ())
    ~run_async:run_async_ ()
