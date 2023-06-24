module A = Atomic_

module State_ = struct
  type 'a single_res =
    | St_none
    | St_some of 'a
    | St_fail of exn * Printexc.raw_backtrace

  type ('a, 'b) t = {
    mutable suspension:
      ((unit, exn * Printexc.raw_backtrace) result -> unit) option;
        (** suspended caller *)
    left: 'a single_res;
    right: 'b single_res;
  }

  let get_exn (self : _ t A.t) =
    match A.get self with
    | { left = St_fail (e, bt); _ } | { right = St_fail (e, bt); _ } ->
      Printexc.raise_with_backtrace e bt
    | { left = St_some x; right = St_some y; _ } -> x, y
    | _ -> assert false

  let check_if_state_complete_ (self : _ t) : unit =
    match self.left, self.right, self.suspension with
    | St_some _, St_some _, Some f -> f (Ok ())
    | St_fail (e, bt), _, Some f | _, St_fail (e, bt), Some f ->
      f (Error (e, bt))
    | _ -> ()

  let set_left_ (self : _ t A.t) (x : _ single_res) =
    while
      let old_st = A.get self in
      let new_st = { old_st with left = x } in
      if A.compare_and_set self old_st new_st then (
        check_if_state_complete_ new_st;
        false
      ) else
        true
    do
      Domain_.relax ()
    done

  let set_right_ (self : _ t A.t) (y : _ single_res) =
    while
      let old_st = A.get self in
      let new_st = { old_st with right = y } in
      if A.compare_and_set self old_st new_st then (
        check_if_state_complete_ new_st;
        false
      ) else
        true
    do
      Domain_.relax ()
    done
end

let both f g : _ * _ =
  let open State_ in
  let st = A.make { suspension = None; left = St_none; right = St_none } in

  let start_tasks ~run () : unit =
    run ~with_handler:true (fun () ->
        try
          let res = f () in
          set_left_ st (St_some res)
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          set_left_ st (St_fail (e, bt)));

    run ~with_handler:true (fun () ->
        try
          let res = g () in
          set_right_ st (St_some res)
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          set_right_ st (St_fail (e, bt)))
  in

  Suspend_.suspend
    {
      Suspend_types_.handle =
        (fun ~run suspension ->
          (* nothing else is started, no race condition possible *)
          (A.get st).suspension <- Some suspension;
          start_tasks ~run ());
    };
  get_exn st

let both_ignore f g = ignore (both f g : unit * unit)

let all_list fs : _ list =
  let len = List.length fs in
  let arr = Array.make len None in
  let has_failed = A.make false in
  let missing = A.make len in

  let start_tasks ~run (suspension : Suspend_types_.suspension) =
    let task_for i f =
      try
        let x = f () in
        arr.(i) <- Some x;

        if A.fetch_and_add missing (-1) = 1 then
          (* all tasks done successfully *)
          suspension (Ok ())
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        if not (A.exchange has_failed true) then
          (* first one to fail, and [missing] must be >= 2
             because we're not decreasing it. *)
          suspension (Error (exn, bt))
    in

    List.iteri (fun i f -> run ~with_handler:true (fun () -> task_for i f)) fs
  in

  Suspend_.suspend
    {
      Suspend_types_.handle =
        (fun ~run suspension ->
          (* nothing else is started, no race condition possible *)
          start_tasks ~run suspension);
    };

  (* get all results *)
  List.init len (fun i ->
      match arr.(i) with
      | None -> assert false
      | Some x -> x)

let all_init n f = all_list @@ List.init n (fun i () -> f i)
