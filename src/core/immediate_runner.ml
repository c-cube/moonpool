open Types_
include Runner

(* convenient alias *)
let k_ls = Task_local_storage.Private_.Storage.k_storage

let run_async_ ~ls f =
  let cur_ls = ref ls in
  TLS.set k_ls (Some cur_ls);
  cur_ls := ls;
  try
    let x = f () in
    TLS.set k_ls None;
    x
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    TLS.set k_ls None;
    Printexc.raise_with_backtrace e bt

let runner : t =
  Runner.For_runner_implementors.create
    ~size:(fun () -> 0)
    ~num_tasks:(fun () -> 0)
    ~shutdown:(fun ~wait:_ () -> ())
    ~run_async:run_async_ ()
