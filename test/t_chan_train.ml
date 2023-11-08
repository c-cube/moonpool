open Moonpool

(* large pool, some of our tasks below are long lived *)
let pool = Ws_pool.create ~num_threads:30 ()

open Fut.Infix

type event =
  | E_int of int
  | E_close

let mk_chan (ic : event Chan.t) : event Chan.t =
  let out = Chan.create () in

  let rec loop () =
    let* ev = Chan.pop ic in
    Chan.push out ev;
    match ev with
    | E_close -> Fut.return ()
    | E_int _x -> loop ()
  in

  ignore (Fut.spawn ~on:pool loop : _ Fut.t);
  out

(* a train of channels connected to one another, with a
   loop pushing events from the input to the output *)
let rec mk_train n ic : _ Chan.t =
  if n = 0 then
    ic
  else (
    let c = mk_chan ic in
    mk_train (n - 1) c
  )

let run () =
  let start = Unix.gettimeofday () in

  let n_trains = 4 in
  let len_train = 80 in
  let n_events = 1_000 in
  let range = 5 in

  (* start trains *)
  let trains =
    List.init n_trains (fun _ ->
        let c = Chan.create () in
        let out = mk_train len_train c in
        c, out)
  in

  let pushers =
    List.map
      (fun (ic, _oc) ->
        Fut.spawn ~on:pool (fun () ->
            for i = 1 to n_events do
              Chan.push ic (E_int (i mod range))
            done;
            Chan.push ic E_close))
      trains
  in

  let gatherers =
    List.map
      (fun (_ic, oc) ->
        let sum = ref 0 in
        try
          while true do
            match Chan.pop_block_exn oc with
            | E_close -> raise Exit
            | E_int x -> sum := !sum + x
          done;
          assert false
        with Exit -> !sum)
      trains
  in

  Fut.wait_block_exn (Fut.wait_list pushers);

  let expected_sum =
    let sum = ref 0 in
    for i = 1 to n_events do
      sum := !sum + (i mod range)
    done;
    !sum
  in

  Printf.printf "got %d events in %d trains (len=%d) in %.2fs\n%!" n_events
    n_trains len_train
    (Unix.gettimeofday () -. start);

  assert (gatherers = List.init n_trains (fun _ -> expected_sum));
  ()

let () = run ()
