module A = Atomic_

type 'a or_error = ('a, Exn_bt.t) result
type 'a waiter = 'a or_error -> unit

type 'a state =
  | Done of 'a or_error
  | Waiting of {
      waiters: 'a waiter list;
      name: string;
    }

type 'a t = { st: 'a state A.t } [@@unboxed]
type 'a promise = 'a t

let[@inline] get_name_ (self : _ t) =
  match A.get self.st with
  | Done _ -> ""
  | Waiting { name; _ } -> name

let make ?(name = "") () =
  let fut = { st = A.make (Waiting { waiters = []; name }) } in
  fut, fut

let[@inline] of_result x : _ t = { st = A.make (Done x) }
let[@inline] return x : _ t = of_result (Ok x)
let[@inline] fail e bt : _ t = of_result (Error (e, bt))
let[@inline] fail_exn_bt ebt = of_result (Error ebt)

let[@inline] is_resolved self : bool =
  match A.get self.st with
  | Done _ -> true
  | Waiting _ -> false

let[@inline] peek self : _ option =
  match A.get self.st with
  | Done x -> Some x
  | Waiting _ -> None

let[@inline] is_done self : bool =
  match A.get self.st with
  | Done _ -> true
  | Waiting _ -> false

let[@inline] is_success self =
  match A.get self.st with
  | Done (Ok _) -> true
  | _ -> false

let[@inline] is_failed self =
  match A.get self.st with
  | Done (Error _) -> true
  | _ -> false

exception Not_ready

let[@inline] get_or_fail self =
  match A.get self.st with
  | Done x -> x
  | Waiting _ -> raise Not_ready

let[@inline] get_or_fail_exn self =
  match A.get self.st with
  | Done (Ok x) -> x
  | Done (Error (exn, bt)) -> Printexc.raise_with_backtrace exn bt
  | Waiting _ -> raise Not_ready

let on_result (self : _ t) (f : _ waiter) : unit =
  while
    let st = A.get self.st in
    match st with
    | Done x ->
      f x;
      false
    | Waiting { waiters = l; name } ->
      not (A.compare_and_set self.st st (Waiting { waiters = f :: l; name }))
  do
    Domain_.relax ()
  done

exception Already_fulfilled

let fulfill (self : _ t) (r : _ result) : unit =
  let fs = ref [] in
  while
    let st = A.get self.st in
    match st with
    | Done _ -> raise Already_fulfilled
    | Waiting { waiters = l; name = _ } ->
      let did_swap = A.compare_and_set self.st st (Done r) in
      if did_swap then (
        (* success, now call all the waiters *)
        fs := l;
        false
      ) else
        true
  do
    Domain_.relax ()
  done;
  List.iter (fun f -> try f r with _ -> ()) !fs;
  ()

let[@inline] fulfill_idempotent self r =
  try fulfill self r with Already_fulfilled -> ()

(* ### combinators ### *)

let spawn ?name ~on f : _ t =
  let fut, promise = make () in

  let task () =
    let res =
      try Ok (f ())
      with e ->
        let bt = Printexc.get_raw_backtrace () in
        Error (e, bt)
    in
    fulfill promise res
  in

  Runner.run_async ?name on task;
  fut

let spawn_on_current_runner ?name f : _ t =
  match Runner.get_current_runner () with
  | None -> failwith "Fut.spawn_on_current_runner: not running on a runner"
  | Some on -> spawn ?name ~on f

let reify_error (f : 'a t) : 'a or_error t =
  match peek f with
  | Some res -> return res
  | None ->
    let fut, promise = make () in
    on_result f (fun r -> fulfill promise (Ok r));
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
       with e ->
         let bt = Printexc.get_raw_backtrace () in
         Error (e, bt))
    | Error e_bt -> Error e_bt
  in

  let name = get_name_ fut in
  match peek fut, get_runner_ ?on () with
  | Some res, None -> of_result @@ map_immediate_ res
  | Some res, Some runner ->
    let fut2, promise = make ~name () in
    Runner.run_async ~name runner (fun () ->
        fulfill promise @@ map_immediate_ res);
    fut2
  | None, None ->
    let fut2, promise = make ~name () in
    on_result fut (fun res -> fulfill promise @@ map_immediate_ res);
    fut2
  | None, Some runner ->
    let fut2, promise = make ~name () in
    on_result fut (fun res ->
        Runner.run_async ~name runner (fun () ->
            fulfill promise @@ map_immediate_ res));
    fut2

let join (fut : 'a t t) : 'a t =
  match peek fut with
  | Some (Ok f) -> f
  | Some (Error (e, bt)) -> fail e bt
  | None ->
    let fut2, promise = make ~name:(get_name_ fut) () in
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
    | Error (e, bt) -> fail e bt
  in

  let bind_and_fulfill (r : _ result) promise () : unit =
    let f_res_fut = apply_f_to_res r in
    (* forward result *)
    on_result f_res_fut (fun r -> fulfill promise r)
  in

  let name = get_name_ fut in
  match peek fut, get_runner_ ?on () with
  | Some res, Some runner ->
    let fut2, promise = make ~name () in
    Runner.run_async ~name runner (bind_and_fulfill res promise);
    fut2
  | Some res, None -> apply_f_to_res res
  | None, Some runner ->
    let fut2, promise = make ~name () in
    on_result fut (fun r ->
        Runner.run_async ~name runner (bind_and_fulfill r promise));
    fut2
  | None, None ->
    let fut2, promise = make ~name () in
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
  | Some (Error (e, bt)), _ | _, Some (Error (e, bt)) -> fail e bt
  | _ ->
    let fut, promise = make ~name:(get_name_ a) () in

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
  | Some (Error (e, bt)), Some (Error _) -> fail e bt
  | _ ->
    let fut, promise = make ~name:(get_name_ a) () in

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
  | Some (Error (e, bt)), Some (Error _) -> fail e bt
  | _ ->
    let fut, promise = make ~name:(get_name_ a) () in

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

let peek_ok_assert_ (self : 'a t) : 'a =
  match A.get self.st with
  | Done (Ok x) -> x
  | _ -> assert false

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
          let res = aggregate_results peek_ok_assert_ cont in
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

let wait_block (self : 'a t) : 'a or_error =
  match A.get self.st with
  | Done x -> x (* fast path *)
  | Waiting _ ->
    let real_block () =
      (* use queue only once *)
      let q = Bb_queue.create () in
      on_result self (fun r -> Bb_queue.push q r);
      Bb_queue.pop q
    in

    (* a bit of spinning before we block *)
    let rec loop i =
      if i = 0 then
        real_block ()
      else (
        match A.get self.st with
        | Done x -> x
        | Waiting _ ->
          Domain_.relax ();
          (loop [@tailcall]) (i - 1)
      )
    in
    loop 50

let wait_block_exn self =
  match wait_block self with
  | Ok x -> x
  | Error (e, bt) -> Printexc.raise_with_backtrace e bt

[@@@ifge 5.0]

let await (fut : 'a t) : 'a =
  match peek fut with
  | Some res ->
    (* fast path: peek *)
    (match res with
    | Ok x -> x
    | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)
  | None ->
    (* suspend until the future is resolved *)
    Suspend_.suspend
      {
        Suspend_.handle =
          (fun ~ls ~run:_ ~resume k ->
            on_result fut (function
              | Ok _ ->
                (* schedule continuation with the same name *)
                resume ~ls k (Ok ())
              | Error (exn, bt) ->
                (* fail continuation immediately *)
                resume ~ls k (Error (exn, bt))));
      };
    (* un-suspended: we should have a result! *)
    get_or_fail_exn fut

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
end
