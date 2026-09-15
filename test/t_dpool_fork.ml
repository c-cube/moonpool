let child () =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> Unix._exit 124));
  ignore (Unix.alarm 5 : int);
  let result = Moonpool_dpool.run_on_and_wait 1 (fun () -> 42) in
  exit
    (if result = 42 then
       0
     else
       1)

let () =
  assert (Moonpool_dpool.max_number_of_domains () >= 2);
  match Unix.fork () with
  | 0 -> child ()
  | pid ->
    let _, status = Unix.waitpid [] pid in
    (match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n -> failwith (Printf.sprintf "child exited with %d" n)
    | Unix.WSIGNALED n -> failwith (Printf.sprintf "child killed by %d" n)
    | Unix.WSTOPPED n -> failwith (Printf.sprintf "child stopped by %d" n))
