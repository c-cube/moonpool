[@@@ifge 5.0]

module Q = QCheck

let spf = Printf.sprintf
let ( let@ ) = ( @@ )
let ppl = Q.Print.(list @@ list int)

open! Moonpool
module FJ = Moonpool_forkjoin

let run ~min () =
  let@ _sp =
    Trace.with_span ~__FILE__ ~__LINE__ "run" ~data:(fun () ->
        [ "min", `Int min ])
  in

  Printf.printf "run with min=%d\n%!" min;
  let neg x = -x in

  let chunk_size = 100 in
  let l = List.init 300 (fun _ -> List.init 15 (fun i -> i)) in

  let ref_l1 = List.map (List.map neg) l in
  let ref_l2 = List.map (List.map neg) ref_l1 in

  for _i = 1 to 800 do
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "step" in

    let l1, l2 =
      let@ pool = Ws_pool.with_ ~num_threads:min () in
      let@ () = Ws_pool.run_wait_block pool in

      let l1, l2 =
        FJ.both
          (fun () ->
            let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fj.left" in
            FJ.map_list ~chunk_size (FJ.map_list ~chunk_size neg) l)
          (fun () ->
            let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fj.right" in
            FJ.map_list ~chunk_size (FJ.map_list ~chunk_size neg) ref_l1)
      in
      l1, l2
    in

    if l1 <> ref_l1 then failwith (spf "l1=%s, ref_l1=%s" (ppl l1) (ppl ref_l1));
    if l2 <> ref_l2 then failwith (spf "l1=%s, ref_l1=%s" (ppl l2) (ppl ref_l2))
  done

let () =
  let@ () = Trace_tef.with_setup () in
  run ~min:4 ();
  run ~min:1 ();
  Printf.printf "done\n%!"

[@@@endif]
