module A = Atomic_
module C = Picos.Computation

type 'a or_error = ('a, Exn_bt.t) result
type 'a waiter = 'a or_error -> unit
type 'a t = { st: 'a C.t } [@@unboxed]
type 'a promise = 'a t

let[@inline] make_ () : _ t =
  let fut = { st = C.create ~mode:`LIFO () } in
  fut

let make () =
  let fut = make_ () in
  fut, fut

let[@inline] return x : _ t = { st = C.returned x }

let[@inline] fail_exn_bt ebt =
  let st = C.create () in
  C.cancel st ebt;
  { st }

let[@inline] fail exn bt : _ t = fail_exn_bt { Exn_bt.exn; bt }

let[@inline] of_result = function
  | Ok x -> return x
  | Error ebt -> fail_exn_bt ebt

let[@inline] is_resolved self : bool = not (C.is_running self.st)
let is_done = is_resolved
let[@inline] peek self : _ option = C.peek self.st
let[@inline] raise_if_failed self : unit = C.check self.st

let[@inline] is_success self =
  match C.peek_exn self.st with
  | _ -> true
  | exception _ -> false

let[@inline] is_failed self = C.is_canceled self.st

exception Not_ready

let[@inline] get_or_fail self =
  match C.peek self.st with
  | Some x -> x
  | None -> raise Not_ready

let[@inline] get_or_fail_exn self =
  match C.peek_exn self.st with
  | x -> x
  | exception C.Running -> raise Not_ready

let[@inline] peek_or_assert_ (self : 'a t) : 'a =
  match C.peek_exn self.st with
  | x -> x
  | exception C.Running -> assert false

let on_result_cb_ _tr f self : unit =
  match peek_or_assert_ self with
  | x -> f (Ok x)
  | exception exn ->
    let ebt = Exn_bt.get exn in
    f (Error ebt)

let on_result (self : _ t) (f : _ waiter) : unit =
  let trigger =
    (Trigger.from_action f self on_result_cb_ [@alert "-handler"])
  in
  ignore (C.try_attach self.st trigger : bool)

let[@inline] fulfill_idempotent self r =
  match r with
  | Ok x -> C.return self.st x
  | Error ebt -> C.cancel self.st ebt

exception Already_fulfilled

let fulfill (self : _ t) (r : _ result) : unit =
  let ok =
    match r with
    | Ok x -> C.try_return self.st x
    | Error ebt -> C.try_cancel self.st ebt
  in
  if not ok then raise Already_fulfilled

(* ### combinators ### *)

let spawn ~on f : _ t =
  let fut = make_ () in

  let task () =
    try
      let res = f () in
      C.return fut.st res
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let ebt = { Exn_bt.exn; bt } in
      C.cancel fut.st ebt
  in

  Runner.run_async on task;
  fut

let spawn_on_current_runner f : _ t =
  match Runner.get_current_runner () with
  | None -> failwith "Fut.spawn_on_current_runner: not running on a runner"
  | Some on -> spawn ~on f

let reify_error (f : 'a t) : 'a or_error t =
  match peek f with
  | Some res -> return res
  | None ->
    let fut = make_ () in
    on_result f (fun r -> fulfill fut (Ok r));
    fut

let[@inline] get_runner_ ?on () : Runner.t option =
  match on with
  | Some _ as r -> r
  | None -> Runner.get_current_runner ()

let map ?on ~f fut : _ t =
  let map_immediate_ r : _ result =
    match r with
    | Ok x ->
      (try Ok (f x)
       with exn ->
         let bt = Printexc.get_raw_backtrace () in
         Error { Exn_bt.exn; bt })
    | Error e_bt -> Error e_bt
  in

  match peek fut, get_runner_ ?on () with
  | Some res, None -> of_result @@ map_immediate_ res
  | Some res, Some runner ->
    let fut2, promise = make () in
    Runner.run_async runner (fun () -> fulfill promise @@ map_immediate_ res);
    fut2
  | None, None ->
    let fut2, promise = make () in
    on_result fut (fun res -> fulfill promise @@ map_immediate_ res);
    fut2
  | None, Some runner ->
    let fut2, promise = make () in
    on_result fut (fun res ->
        Runner.run_async runner (fun () ->
            fulfill promise @@ map_immediate_ res));
    fut2

let join (fut : 'a t t) : 'a t =
  match peek fut with
  | Some (Ok f) -> f
  | Some (Error ebt) -> fail_exn_bt ebt
  | None ->
    let fut2, promise = make () in
    on_result fut (function
      | Ok sub_fut -> on_result sub_fut (fulfill promise)
      | Error _ as e -> fulfill promise e);
    fut2

let bind ?on ~f fut : _ t =
  let apply_f_to_res r : _ t =
    match r with
    | Ok x ->
      (try f x
       with e ->
         let bt = Printexc.get_raw_backtrace () in
         fail e bt)
    | Error ebt -> fail_exn_bt ebt
  in

  let bind_and_fulfill (r : _ result) promise () : unit =
    let f_res_fut = apply_f_to_res r in
    (* forward result *)
    on_result f_res_fut (fun r -> fulfill promise r)
  in

  match peek fut, get_runner_ ?on () with
  | Some res, Some runner ->
    let fut2, promise = make () in
    Runner.run_async runner (bind_and_fulfill res promise);
    fut2
  | Some res, None -> apply_f_to_res res
  | None, Some runner ->
    let fut2, promise = make () in
    on_result fut (fun r ->
        Runner.run_async runner (bind_and_fulfill r promise));
    fut2
  | None, None ->
    let fut2, promise = make () in
    on_result fut (fun res -> bind_and_fulfill res promise ());
    fut2

let[@inline] bind_reify_error ?on ~f fut : _ t = bind ?on ~f (reify_error fut)

let update_atomic_ (st : 'a A.t) f : 'a =
  let rec loop () =
    let x = A.get st in
    let y = f x in
    if A.compare_and_set st x y then
      y
    else (
      Domain_.relax ();
      loop ()
    )
  in
  loop ()

let both a b : _ t =
  match peek a, peek b with
  | Some (Ok x), Some (Ok y) -> return (x, y)
  | Some (Error ebt), _ | _, Some (Error ebt) -> fail_exn_bt ebt
  | _ ->
    let fut, promise = make () in

    let st = A.make `Neither in
    on_result a (function
      | Error err -> fulfill_idempotent promise (Error err)
      | Ok x ->
        (match
           update_atomic_ st (function
             | `Neither -> `Left x
             | `Right y -> `Both (x, y)
             | _ -> assert false)
         with
        | `Both (x, y) -> fulfill promise (Ok (x, y))
        | _ -> ()));
    on_result b (function
      | Error err -> fulfill_idempotent promise (Error err)
      | Ok y ->
        (match
           update_atomic_ st (function
             | `Left x -> `Both (x, y)
             | `Neither -> `Right y
             | _ -> assert false)
         with
        | `Both (x, y) -> fulfill promise (Ok (x, y))
        | _ -> ()));
    fut

let choose a b : _ t =
  match peek a, peek b with
  | Some (Ok x), _ -> return (Either.Left x)
  | _, Some (Ok y) -> return (Either.Right y)
  | Some (Error ebt), Some (Error _) -> fail_exn_bt ebt
  | _ ->
    let fut, promise = make () in

    let one_failure = A.make false in
    on_result a (function
      | Error err ->
        if A.exchange one_failure true then
          (* the other one failed already *)
          fulfill_idempotent promise (Error err)
      | Ok x -> fulfill_idempotent promise (Ok (Either.Left x)));
    on_result b (function
      | Error err ->
        if A.exchange one_failure true then
          (* the other one failed already *)
          fulfill_idempotent promise (Error err)
      | Ok y -> fulfill_idempotent promise (Ok (Either.Right y)));
    fut

let choose_same a b : _ t =
  match peek a, peek b with
  | Some (Ok x), _ -> return x
  | _, Some (Ok y) -> return y
  | Some (Error ebt), Some (Error _) -> fail_exn_bt ebt
  | _ ->
    let fut, promise = make () in

    let one_failure = A.make false in
    on_result a (function
      | Error err ->
        if A.exchange one_failure true then
          fulfill_idempotent promise (Error err)
      | Ok x -> fulfill_idempotent promise (Ok x));
    on_result b (function
      | Error err ->
        if A.exchange one_failure true then
          fulfill_idempotent promise (Error err)
      | Ok y -> fulfill_idempotent promise (Ok y));
    fut

let barrier_on_abstract_container_of_futures ~iter ~len ~aggregate_results cont
    : _ t =
  let n_items = len cont in
  if n_items = 0 then (
    (* no items, return now. *)
    let cont_empty = aggregate_results (fun _ -> assert false) cont in
    return cont_empty
  ) else (
    let fut, promise = make () in
    let missing = A.make n_items in

    (* callback called when a future in [a] is resolved *)
    let on_res = function
      | Ok _ ->
        let n = A.fetch_and_add missing (-1) in
        if n = 1 then (
          (* last future, we know they all succeeded, so resolve [fut] *)
          let res = aggregate_results peek_or_assert_ cont in
          fulfill promise (Ok res)
        )
      | Error e_bt ->
        (* immediately cancel all other [on_res] *)
        let n = A.exchange missing 0 in
        if n > 0 then
          (* we're the only one to set to 0, so we can fulfill [fut]
             with an error. *)
          fulfill promise (Error e_bt)
    in

    iter (fun fut -> on_result fut on_res) cont;
    fut
  )

module Advanced = struct
  let barrier_on_abstract_container_of_futures =
    barrier_on_abstract_container_of_futures
end

let join_array (a : _ t array) : _ array t =
  match Array.length a with
  | 0 -> return [||]
  | 1 -> map ?on:None a.(0) ~f:(fun x -> [| x |])
  | _ ->
    barrier_on_abstract_container_of_futures ~len:Array.length
      ~aggregate_results:Array.map ~iter:Array.iter a

let join_list (l : _ t list) : _ list t =
  match l with
  | [] -> return []
  | [ x ] -> map ?on:None x ~f:(fun x -> [ x ])
  | _ ->
    barrier_on_abstract_container_of_futures ~len:List.length
      ~aggregate_results:List.map ~iter:List.iter l

let[@inline] map_list ~f l : _ list t = List.map f l |> join_list

let wait_array (a : _ t array) : unit t =
  barrier_on_abstract_container_of_futures a ~iter:Array.iter ~len:Array.length
    ~aggregate_results:(fun _f _ -> ())

let wait_list (a : _ t list) : unit t =
  barrier_on_abstract_container_of_futures a ~iter:List.iter ~len:List.length
    ~aggregate_results:(fun _f _ -> ())

let for_ ~on n f : unit t =
  barrier_on_abstract_container_of_futures
    ~len:(fun () -> n)
    ~iter:(fun yield () ->
      for i = 0 to n - 1 do
        yield (spawn ~on (fun () -> f i))
      done)
    ~aggregate_results:(fun _f () -> ())
    ()

let for_array ~on arr f : unit t =
  for_ ~on (Array.length arr) (fun i -> f i arr.(i))

let for_list ~on l f : unit t =
  let futs = List.rev_map (fun x -> spawn ~on (fun () -> f x)) l in
  wait_list futs

(* ### blocking ### *)

let push_queue_ _tr q () = Bb_queue.push q ()

let wait_block_exn (self : 'a t) : 'a =
  match C.peek_exn self.st with
  | x -> x (* fast path *)
  | exception C.Running ->
    let real_block () =
      (* use queue only once *)
      let q = Bb_queue.create () in

      let trigger = Trigger.create () in
      let attached =
        (Trigger.on_signal trigger q () push_queue_ [@alert "-handler"])
      in
      assert attached;

      (* blockingly wait for trigger if computation didn't complete in the mean time *)
      if C.try_attach self.st trigger then Bb_queue.pop q;

      (* trigger was signaled! computation must be done*)
      peek_or_assert_ self
    in

    (* TODO: use backoff? *)
    (* a bit of spinning before we block *)
    let rec loop i =
      if i = 0 then
        real_block ()
      else (
        match C.peek_exn self.st with
        | x -> x
        | exception C.Running ->
          Domain_.relax ();
          (loop [@tailcall]) (i - 1)
      )
    in
    loop 50

let wait_block self =
  match wait_block_exn self with
  | x -> Ok x
  | exception exn ->
    let bt = Printexc.get_raw_backtrace () in
    Error { Exn_bt.exn; bt }

[@@@ifge 5.0]

let await (self : 'a t) : 'a =
  (* fast path: peek *)
  match C.peek_exn self.st with
  | res -> res
  | exception C.Running ->
    let trigger = Trigger.create () in
    (* suspend until the future is resolved *)
    if C.try_attach self.st trigger then
      Option.iter Exn_bt.raise @@ Trigger.await trigger;

    (* un-suspended: we should have a result! *)
    get_or_fail_exn self

[@@@endif]

module Infix = struct
  let[@inline] ( >|= ) x f = map ~f x
  let[@inline] ( >>= ) x f = bind ~f x
  let ( let+ ) = ( >|= )
  let ( let* ) = ( >>= )
  let ( and+ ) = both
  let ( and* ) = both
end

include Infix
module Infix_local = Infix [@@deprecated "use Infix"]

module Private_ = struct
  let[@inline] unsafe_promise_of_fut x = x
  let[@inline] as_computation self = self.st
end
