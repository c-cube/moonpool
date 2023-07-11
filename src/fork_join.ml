[@@@ifge 5.0]

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
      Suspend_.handle =
        (fun ~run suspension ->
          (* nothing else is started, no race condition possible *)
          (A.get st).suspension <- Some suspension;
          start_tasks ~run ());
    };
  get_exn st

let both_ignore f g = ignore (both f g : _ * _)

let for_ ?chunk_size n (f : int -> int -> unit) : unit =
  let has_failed = A.make false in
  let missing = A.make n in

  let chunk_size =
    match chunk_size with
    | Some cs -> max 1 (min n cs)
    | None ->
      (* guess: try to have roughly one task per core *)
      max 1 (1 + (n / D_pool_.n_domains ()))
  in

  let start_tasks ~run (suspension : Suspend_.suspension) =
    let task_for ~offset ~len_range =
      match f offset (offset + len_range - 1) with
      | () ->
        if A.fetch_and_add missing (-len_range) = len_range then
          (* all tasks done successfully *)
          suspension (Ok ())
      | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        if not (A.exchange has_failed true) then
          (* first one to fail, and [missing] must be >= 2
             because we're not decreasing it. *)
          suspension (Error (exn, bt))
    in

    let i = ref 0 in
    while !i < n do
      let offset = !i in

      let len_range = min chunk_size (n - offset) in
      assert (offset + len_range <= n);

      run ~with_handler:true (fun () -> task_for ~offset ~len_range);
      i := !i + len_range
    done
  in

  Suspend_.suspend
    {
      Suspend_.handle =
        (fun ~run suspension ->
          (* run tasks, then we'll resume [suspension] *)
          start_tasks ~run suspension);
    };
  ()

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

type 'a commutative_monoid = {
  neutral: unit -> 'a;  (** Neutral element *)
  combine: 'a -> 'a -> 'a;  (** Combine two items. *)
}

let map_reduce_commutative ?chunk_size ~gen ~map
    ~(reduce : 'b commutative_monoid) n : 'b =
  let res = Lock.create (reduce.neutral ()) in

  for_ ?chunk_size n (fun low high ->
      let local_acc = ref (reduce.neutral ()) in
      for i = low to high do
        let x = gen i in
        let y = map x in
        local_acc := reduce.combine !local_acc y
      done;

      Lock.update res (fun res -> reduce.combine res !local_acc));
  Lock.get res

[@@@endif]
