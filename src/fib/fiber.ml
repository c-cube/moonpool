module A = Atomic
module FM = Handle.Map
module Int_map = Map.Make (Int)

type 'a callback = 'a Exn_bt.result -> unit
(** Callbacks that are called when a fiber is done. *)

type cancel_callback = Exn_bt.t -> unit

let prom_of_fut : 'a Fut.t -> 'a Fut.promise =
  Fut.Private_.unsafe_promise_of_fut

module Private_ = struct
  type 'a t = {
    id: Handle.t;  (** unique identifier for this fiber *)
    state: 'a state A.t;  (** Current state in the lifetime of the fiber *)
    res: 'a Fut.t;
    runner: Runner.t;
    ls: Task_local_storage.storage ref;
  }

  and 'a state =
    | Alive of {
        children: children;
        on_cancel: cancel_callback Int_map.t;
        cancel_id: int;
      }
    | Terminating_or_done of 'a Exn_bt.result A.t

  and children = any FM.t
  and any = Any : _ t -> any [@@unboxed]
  and nursery = Nursery : _ t -> nursery [@@unboxed]

  (** Key to access the current fiber. *)
  let k_current_fiber : any option Task_local_storage.key =
    Task_local_storage.new_key ~init:(fun () -> None) ()

  let[@inline] get_cur () : any option = Task_local_storage.get k_current_fiber

  let[@inline] is_closed (self : _ t) =
    match A.get self.state with
    | Alive _ -> false
    | Terminating_or_done _ -> true
end

include Private_

let[@inline] res self = self.res
let[@inline] peek self = Fut.peek self.res
let[@inline] is_done self = Fut.is_done self.res
let[@inline] is_success self = Fut.is_success self.res
let[@inline] is_cancelled self = Fut.is_failed self.res
let[@inline] on_result (self : _ t) f = Fut.on_result self.res f
let[@inline] await self = Fut.await self.res
let[@inline] wait_block self = Fut.wait_block self.res
let[@inline] wait_block_exn self = Fut.wait_block_exn self.res

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
    | Alive { children; cancel_id = _; on_cancel } as old ->
      let new_st = Terminating_or_done (A.make @@ Error ebt) in
      if A.compare_and_set self.state old new_st then (
        (* here, unlike in {!resolve_fiber}, we immediately cancel children *)
        cancel_children_ ~children ebt;
        Int_map.iter (fun _ cb -> cb ebt) on_cancel;
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

type cancel_handle = int

let add_on_cancel (self : _ t) cb : cancel_handle =
  let h = ref 0 in
  while
    match A.get self.state with
    | Alive { children; cancel_id; on_cancel } as old ->
      let new_st =
        Alive
          {
            children;
            cancel_id = cancel_id + 1;
            on_cancel = Int_map.add cancel_id cb on_cancel;
          }
      in
      if A.compare_and_set self.state old new_st then (
        h := cancel_id;
        false
      ) else
        true
    | Terminating_or_done r ->
      (match A.get r with
      | Error ebt -> cb ebt
      | Ok _ -> ());
      false
  do
    ()
  done;
  !h

let remove_on_cancel (self : _ t) h =
  while
    match A.get self.state with
    | Alive ({ on_cancel; _ } as alive) as old ->
      let new_st =
        Alive { alive with on_cancel = Int_map.remove h on_cancel }
      in
      not (A.compare_and_set self.state old new_st)
    | Terminating_or_done _ -> false
  do
    ()
  done

let with_cancel_callback (self : _ t) cb (k : unit -> 'a) : 'a =
  let h = add_on_cancel self cb in
  Fun.protect k ~finally:(fun () -> remove_on_cancel self h)

(** Successfully resolve the fiber *)
let resolve_ok_ (self : 'a t) (r : 'a) : unit =
  let r = A.make @@ Ok r in
  let promise = prom_of_fut self.res in
  while
    match A.get self.state with
    | Alive { children; _ } as old ->
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
    | Alive ({ children; _ } as alive) as old ->
      let new_st =
        Alive { alive with children = FM.remove child.id children }
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
    | Alive ({ children; _ } as alive) as old ->
      let new_st =
        Alive { alive with children = FM.add child.id (Any child) children }
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

let create_ ~ls ~runner () : 'a t =
  let id = Handle.generate_fresh () in
  let res, _promise = Fut.make () in
  {
    state =
      A.make
      @@ Alive { children = FM.empty; on_cancel = Int_map.empty; cancel_id = 0 };
    id;
    res;
    runner;
    ls;
  }

let spawn_ ~ls (Nursery n) (f : nursery -> 'a) : 'a t =
  if is_closed n then failwith "spawn: nursery is closed";
  let fib = create_ ~ls ~runner:n.runner () in

  let run () =
    (* make sure the fiber is accessible from inside itself *)
    Task_local_storage.set k_current_fiber (Some (Any fib));
    try
      let res = f (Nursery fib) in
      resolve_ok_ fib res
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let ebt = Exn_bt.make exn bt in
      resolve_as_failed_ fib ebt
  in

  Runner.run_async ~ls n.runner run;

  fib

let spawn (Nursery n) ?(protect = true) f : _ t =
  (* spawn [f()] with a copy of our local storage *)
  let ls = ref (Task_local_storage.Private_.Storage.copy !(n.ls)) in
  let child = spawn_ ~ls (Nursery n) f in
  add_child_ ~protect n child;
  child

let[@inline] spawn_ignore n ?protect f : unit =
  ignore (spawn n ?protect f : _ t)

module Nursery = struct
  type t = nursery

  let[@inline] await (Nursery n) : unit =
    ignore (await n);
    ()

  let cancel_with (Nursery n) ebt : unit = resolve_as_failed_ n ebt

  let with_create_top ~on () f =
    let n =
      create_
        ~ls:(ref @@ Task_local_storage.Private_.Storage.create ())
        ~runner:on ()
    in
    Fun.protect ~finally:(fun () -> resolve_ok_ n ()) (fun () -> f (Nursery n))

  let with_create_sub ~protect (Nursery parent : t) f =
    let n =
      create_
        ~ls:(ref @@ Task_local_storage.Private_.Storage.copy !(parent.ls))
        ~runner:parent.runner ()
    in
    add_child_ ~protect parent n;
    Fun.protect ~finally:(fun () -> resolve_ok_ n ()) (fun () -> f (Nursery n))

  let[@inline] with_cancel_callback (Nursery self) cb f =
    with_cancel_callback self cb f
end

let[@inline] self () : any =
  match Task_local_storage.get k_current_fiber with
  | None -> failwith "Fiber.self: must be run from inside a fiber."
  | Some f -> f

let[@inline] cur_nursery () =
  let (Any f) = self () in
  Nursery f

let with_self_cancel_callback cb (k : unit -> 'a) : 'a =
  let (Any self) = self () in
  let h = add_on_cancel self cb in
  Fun.protect k ~finally:(fun () -> remove_on_cancel self h)

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
