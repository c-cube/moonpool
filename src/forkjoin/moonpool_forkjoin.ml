[@@@ifge 5.0]

module A = Moonpool.Atomic
module Suspend_ = Moonpool.Private.Suspend_
module Domain_ = Moonpool_private.Domain_

module State_ = struct
  type error = exn * Printexc.raw_backtrace
  type 'a or_error = ('a, error) result

  type ('a, 'b) t =
    | Init
    | Left_solved of 'a or_error
    | Right_solved of 'b or_error * Suspend_.suspension
    | Both_solved of 'a or_error * 'b or_error

  let get_exn_ (self : _ t A.t) =
    match A.get self with
    | Both_solved (Ok a, Ok b) -> a, b
    | Both_solved (Error (exn, bt), _) | Both_solved (_, Error (exn, bt)) ->
      Printexc.raise_with_backtrace exn bt
    | _ -> assert false

  let rec set_left_ (self : _ t A.t) (left : _ or_error) =
    let old_st = A.get self in
    match old_st with
    | Init ->
      let new_st = Left_solved left in
      if not (A.compare_and_set self old_st new_st) then (
        Domain_.relax ();
        set_left_ self left
      )
    | Right_solved (right, cont) ->
      let new_st = Both_solved (left, right) in
      if not (A.compare_and_set self old_st new_st) then (
        Domain_.relax ();
        set_left_ self left
      ) else
        cont (Ok ())
    | Left_solved _ | Both_solved _ -> assert false

  let rec set_right_ (self : _ t A.t) (right : _ or_error) : unit =
    let old_st = A.get self in
    match old_st with
    | Left_solved left ->
      let new_st = Both_solved (left, right) in
      if not (A.compare_and_set self old_st new_st) then set_right_ self right
    | Init ->
      (* we are first arrived, we suspend until the left computation is done *)
      Suspend_.suspend
        {
          Suspend_.handle =
            (fun ~name:_ ~run:_ suspension ->
              while
                let old_st = A.get self in
                match old_st with
                | Init ->
                  not
                    (A.compare_and_set self old_st
                       (Right_solved (right, suspension)))
                | Left_solved left ->
                  (* other thread is done, no risk of race condition *)
                  A.set self (Both_solved (left, right));
                  suspension (Ok ());
                  false
                | Right_solved _ | Both_solved _ -> assert false
              do
                ()
              done);
        }
    | Right_solved _ | Both_solved _ -> assert false
end

let both f g : _ * _ =
  let module ST = State_ in
  let st = A.make ST.Init in

  let runner =
    match Runner.get_current_runner () with
    | None -> invalid_arg "Fork_join.both must be run from within a runner"
    | Some r -> r
  in

  (* start computing [f] in the background *)
  Runner.run_async runner (fun () ->
      try
        let res = f () in
        ST.set_left_ st (Ok res)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        ST.set_left_ st (Error (exn, bt)));

  let res_right =
    try Ok (g ())
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Error (exn, bt)
  in

  ST.set_right_ st res_right;
  ST.get_exn_ st

let both_ignore f g = ignore (both f g : _ * _)

let for_ ?chunk_size n (f : int -> int -> unit) : unit =
  if n > 0 then (
    let has_failed = A.make false in
    let missing = A.make n in

    let chunk_size =
      match chunk_size with
      | Some cs -> max 1 (min n cs)
      | None ->
        (* guess: try to have roughly one task per core *)
        max 1 (1 + (n / Moonpool.Private.num_domains ()))
    in

    let start_tasks ~name ~run (suspension : Suspend_.suspension) =
      let task_for ~offset ~len_range =
        match f offset (offset + len_range - 1) with
        | () ->
          if A.fetch_and_add missing (-len_range) = len_range then
            (* all tasks done successfully *)
            run ~name (fun () -> suspension (Ok ()))
        | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          if not (A.exchange has_failed true) then
            (* first one to fail, and [missing] must be >= 2
               because we're not decreasing it. *)
            run ~name (fun () -> suspension (Error (exn, bt)))
      in

      let i = ref 0 in
      while !i < n do
        let offset = !i in

        let len_range = min chunk_size (n - offset) in
        assert (offset + len_range <= n);

        run ~name (fun () -> task_for ~offset ~len_range);
        i := !i + len_range
      done
    in

    Suspend_.suspend
      {
        Suspend_.handle =
          (fun ~name ~run suspension ->
            (* run tasks, then we'll resume [suspension] *)
            start_tasks ~run ~name suspension);
      }
  )

let all_array ?chunk_size (fs : _ array) : _ array =
  let len = Array.length fs in
  let arr = Array.make len None in

  (* parallel for *)
  for_ ?chunk_size len (fun low high ->
      for i = low to high do
        let x = fs.(i) () in
        arr.(i) <- Some x
      done);

  (* get all results *)
  Array.map
    (function
      | None -> assert false
      | Some x -> x)
    arr

let all_list ?chunk_size fs : _ list =
  Array.to_list @@ all_array ?chunk_size @@ Array.of_list fs

let all_init ?chunk_size n f : _ list =
  let arr = Array.make n None in

  for_ ?chunk_size n (fun low high ->
      for i = low to high do
        let x = f i in
        arr.(i) <- Some x
      done);

  (* get all results *)
  List.init n (fun i ->
      match arr.(i) with
      | None -> assert false
      | Some x -> x)

let map_array ?chunk_size f arr : _ array =
  let n = Array.length arr in
  let res = Array.make n None in

  for_ ?chunk_size n (fun low high ->
      for i = low to high do
        res.(i) <- Some (f arr.(i))
      done);

  (* get all results *)
  Array.map
    (function
      | None -> assert false
      | Some x -> x)
    res

let map_list ?chunk_size f (l : _ list) : _ list =
  let arr = Array.of_list l in
  let n = Array.length arr in
  let res = Array.make n None in

  for_ ?chunk_size n (fun low high ->
      for i = low to high do
        res.(i) <- Some (f arr.(i))
      done);

  (* get all results *)
  List.init n (fun i ->
      match res.(i) with
      | None -> assert false
      | Some x -> x)

[@@@endif]
