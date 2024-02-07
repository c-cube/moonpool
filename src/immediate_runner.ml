include Runner

let run_async_ ~name f =
  let sp = Tracing_.enter_span name in
  try
    let x = f () in
    Tracing_.exit_span sp;
    x
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Tracing_.exit_span sp;
    Printexc.raise_with_backtrace e bt

let runner : t =
  Runner.For_runner_implementors.create
    ~size:(fun () -> 0)
    ~num_tasks:(fun () -> 0)
    ~shutdown:(fun ~wait:_ () -> ())
    ~run_async:run_async_ ()
