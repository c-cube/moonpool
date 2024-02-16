module A = Atomic
module FM = Handle.Map

type 'a callback = 'a Exn_bt.result -> unit
(** Callbacks that are called when a fiber is done. *)

type cancel_callback = Exn_bt.t -> unit

let prom_of_fut : 'a Fut.t -> 'a Fut.promise =
  Fut.Private_.unsafe_promise_of_fut

type 'a t = {
  id: Handle.t;  (** unique identifier for this fiber *)
  state: 'a state A.t;  (** Current state in the lifetime of the fiber *)
  res: 'a Fut.t;
  runner: Runner.t;
}

and 'a state =
  | Alive of {
      children: children;
      on_cancel: cancel_callback list;
    }
  | Terminating_or_done of 'a Exn_bt.result A.t

and children = any FM.t
and any = Any : _ t -> any [@@unboxed]

let[@inline] res self = self.res
let[@inline] peek self = Fut.peek self.res
let[@inline] is_done self = Fut.is_done self.res
let[@inline] is_success self = Fut.is_success self.res
let[@inline] is_cancelled self = Fut.is_failed self.res
let[@inline] on_result (self : _ t) f = Fut.on_result self.res f

(** Resolve [promise] once [children] are all done *)
let resolve_once_children_are_done_ ~children ~promise
    (res : 'a Exn_bt.result A.t) : unit =
  let n_children = FM.cardinal children in
  if n_children > 0 then (
    (* wait for all children to be done *)
    let n_waiting = A.make (FM.cardinal children) in
    let on_child_finish (r : _ result) =
      (* make sure the parent fails if any child fails *)
      (match r with
      | Ok _ -> ()
      | Error ebt -> A.set res (Error ebt));

      (* if we're the last to finish, resolve the parent fiber's [res] *)
      if A.fetch_and_add n_waiting (-1) = 1 then (
        let res = A.get res in
        Fut.fulfill promise res
      )
    in
    FM.iter (fun _ (Any f) -> Fut.on_result f.res on_child_finish) children
  ) else
    Fut.fulfill promise @@ A.get res

let rec resolve_as_failed_ : type a. a t -> Exn_bt.t -> unit =
 fun self ebt ->
  let promise = prom_of_fut self.res in
  while
    match A.get self.state with
    | Alive { children; on_cancel } as old ->
      let new_st = Terminating_or_done (A.make @@ Error ebt) in
      if A.compare_and_set self.state old new_st then (
        (* here, unlike in {!resolve_fiber}, we immediately cancel children *)
        cancel_children_ ~children ebt;
        List.iter (fun cb -> cb ebt) on_cancel;
        resolve_once_children_are_done_ ~children ~promise (A.make @@ Error ebt);
        false
      ) else
        true
    | Terminating_or_done _ -> false
  do
    ()
  done

(** Cancel eagerly all children *)
and cancel_children_ ebt ~children : unit =
  FM.iter (fun _ (Any f) -> resolve_as_failed_ f ebt) children

(** Successfully resolve the fiber *)
let resolve_ok_ (self : 'a t) (r : 'a) : unit =
  let r = A.make @@ Ok r in
  let promise = prom_of_fut self.res in
  while
    match A.get self.state with
    | Alive { children; on_cancel = _ } as old ->
      let new_st = Terminating_or_done r in
      if A.compare_and_set self.state old new_st then (
        resolve_once_children_are_done_ ~children ~promise r;
        false
      ) else
        true
    | Terminating_or_done _ -> false
  do
    ()
  done

let remove_child_ (self : _ t) (child : _ t) =
  while
    match A.get self.state with
    | Alive { children; on_cancel } as old ->
      let new_st =
        Alive { children = FM.remove child.id children; on_cancel }
      in
      not (A.compare_and_set self.state old new_st)
    | _ -> false
  do
    ()
  done

(** Add a child to [self].
    @param protected if true, the child's failure will not affect [self]. *)
let add_child_ ~protect (self : _ t) (child : _ t) =
  while
    match A.get self.state with
    | Alive { children; on_cancel } as old ->
      let new_st =
        Alive { children = FM.add child.id (Any child) children; on_cancel }
      in

      if A.compare_and_set self.state old new_st then (
        (* make sure to remove [child] from [self.children] once it's done;
           fail [self] is [child] failed and [protect=false] *)
        Fut.on_result child.res (function
          | Ok _ -> remove_child_ self child
          | Error ebt ->
            (* child failed, we must fail too *)
            remove_child_ self child;
            if not protect then resolve_as_failed_ self ebt);
        false
      ) else
        true
    | Terminating_or_done r ->
      (match A.get r with
      | Error ebt ->
        (* cancel child immediately *)
        resolve_as_failed_ child ebt
      | Ok _ -> ());
      false
  do
    ()
  done

(** Key to access the current fiber. *)
let k_current_fiber : any option Task_local_storage.key =
  Task_local_storage.new_key ~init:(fun () -> None) ()

let spawn_ ?name ~on (f : _ -> 'a) : 'a t =
  let id = Handle.generate_fresh () in
  let res, _promise = Fut.make ?name () in
  let fib =
    {
      state = A.make @@ Alive { children = FM.empty; on_cancel = [] };
      id;
      res;
      runner = on;
    }
  in

  let run () =
    (* make sure the fiber is accessible from inside itself *)
    Task_local_storage.set k_current_fiber (Some (Any fib));
    try
      let res = f () in
      resolve_ok_ fib res
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let ebt = Exn_bt.make exn bt in
      resolve_as_failed_ fib ebt
  in

  Runner.run_async on ?name run;

  fib

let[@inline] spawn_top ?name ~on f : _ t = spawn_ ?name ~on f

let spawn_link ?name ~protect f : _ t =
  match Task_local_storage.get k_current_fiber with
  | None -> failwith "Fiber.spawn_link: must be run from inside a fiber."
  | Some (Any parent) ->
    let child = spawn_ ?name ~on:parent.runner f in
    add_child_ ~protect parent child;
    child

let add_cancel_cb_ (self : _ t) cb =
  while
    match A.get self.state with
    | Alive { children; on_cancel } as old ->
      let new_st = Alive { children; on_cancel = cb :: on_cancel } in
      not (A.compare_and_set self.state old new_st)
    | Terminating_or_done r ->
      (match A.get r with
      | Error ebt -> cb ebt
      | Ok _ -> ());
      false
  do
    ()
  done

let remove_top_cancel_cb_ (self : _ t) =
  while
    match A.get self.state with
    | Alive { on_cancel = []; _ } -> assert false
    | Alive { children; on_cancel = _ :: tl } as old ->
      let new_st = Alive { children; on_cancel = tl } in
      not (A.compare_and_set self.state old new_st)
    | Terminating_or_done _ -> false
  do
    ()
  done

let with_cancel_callback (self : _ t) cb (k : unit -> 'a) : 'a =
  add_cancel_cb_ self cb;
  Fun.protect k ~finally:(fun () -> remove_top_cancel_cb_ self)

let[@inline] await self = Fut.await self.res

module Suspend_ = Moonpool.Private.Suspend_

let check_if_cancelled () =
  match Task_local_storage.get k_current_fiber with
  | None ->
    failwith "Fiber.check_if_cancelled: must be run from inside a fiber."
  | Some (Any self) ->
    (match peek self with
    | Some (Error ebt) -> Exn_bt.raise ebt
    | _ -> ())

let[@inline] yield () : unit =
  check_if_cancelled ();
  Suspend_.yield ()
