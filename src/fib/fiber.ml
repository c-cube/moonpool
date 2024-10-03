open Moonpool.Private.Types_
module A = Atomic
module FM = Handle.Map
module Int_map = Map.Make (Int)
module PF = Picos.Fiber
module FLS = Picos.Fiber.FLS

type 'a callback = 'a Exn_bt.result -> unit
(** Callbacks that are called when a fiber is done. *)

type cancel_callback = Exn_bt.t -> unit

let prom_of_fut : 'a Fut.t -> 'a Fut.promise =
  Fut.Private_.unsafe_promise_of_fut

(* TODO: replace with picos structured at some point? *)
module Private_ = struct
  type pfiber = PF.t

  type 'a t = {
    id: Handle.t;  (** unique identifier for this fiber *)
    state: 'a state A.t;  (** Current state in the lifetime of the fiber *)
    res: 'a Fut.t;
    runner: Runner.t;
    pfiber: pfiber;  (** Associated picos fiber *)
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

  (** Key to access the current moonpool.fiber. *)
  let k_current_fiber : any FLS.t = FLS.create ()

  exception Not_set = FLS.Not_set

  let[@inline] get_cur_from_exn (pfiber : pfiber) : any =
    FLS.get_exn pfiber k_current_fiber

  let[@inline] get_cur_exn () : any =
    get_cur_from_exn @@ get_current_fiber_exn ()

  let[@inline] get_cur_opt () = try Some (get_cur_exn ()) with _ -> None

  let[@inline] is_closed (self : _ t) =
    match A.get self.state with
    | Alive _ -> false
    | Terminating_or_done _ -> true
end

include Private_

let create_ ~pfiber ~runner ~res () : 'a t =
  let id = Handle.generate_fresh () in
  {
    state =
      A.make
      @@ Alive { children = FM.empty; on_cancel = Int_map.empty; cancel_id = 0 };
    id;
    res;
    runner;
    pfiber;
  }

let create_done_ ~res () : _ t =
  let id = Handle.generate_fresh () in
  {
    state =
      A.make
      @@ Alive { children = FM.empty; on_cancel = Int_map.empty; cancel_id = 0 };
    id;
    res;
    runner = Runner.dummy;
    pfiber = Moonpool.Private.Types_._dummy_fiber;
  }

let[@inline] return x = create_done_ ~res:(Fut.return x) ()
let[@inline] fail ebt = create_done_ ~res:(Fut.fail_exn_bt ebt) ()
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

let with_on_cancel (self : _ t) cb (k : unit -> 'a) : 'a =
  let h = add_on_cancel self cb in
  Fun.protect k ~finally:(fun () -> remove_on_cancel self h)

(** Successfully resolve the fiber. This might still fail if
    some children failed. *)
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

let spawn_ ~parent ~runner (f : unit -> 'a) : 'a t =
  let res, _ = Fut.make () in
  let pfiber = PF.create ~forbid:false (Fut.Private_.as_computation res) in

  (* copy local hmap from parent, if present *)
  Option.iter
    (fun (p : _ t) -> Fls.Private_hmap_ls_.copy_fls p.pfiber pfiber)
    parent;

  (match parent with
  | Some p when is_closed p -> failwith "spawn: nursery is closed"
  | _ -> ());
  let fib = create_ ~pfiber ~runner ~res () in

  let run () =
    (* make sure the fiber is accessible from inside itself *)
    FLS.set pfiber k_current_fiber (Any fib);
    try
      let res = f () in
      resolve_ok_ fib res
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let ebt = Exn_bt.make exn bt in
      resolve_as_failed_ fib ebt
  in

  Runner.run_async ~fiber:pfiber runner run;

  fib

let spawn_top ~on f : _ t = spawn_ ~runner:on ~parent:None f

let spawn ?on ?(protect = true) f : _ t =
  (* spawn [f()] with a copy of our local storage *)
  let (Any p) =
    try get_cur_exn ()
    with Not_set ->
      failwith "Fiber.spawn: must be run from within another fiber."
  in

  let runner =
    match on with
    | Some r -> r
    | None -> p.runner
  in
  let child = spawn_ ~parent:(Some p) ~runner f in
  add_child_ ~protect p child;
  child

let[@inline] spawn_ignore ?protect f : unit = ignore (spawn ?protect f : _ t)
let[@inline] spawn_top_ignore ~on f : unit = ignore (spawn_top ~on f : _ t)

let[@inline] self () : any =
  match get_cur_exn () with
  | exception Not_set -> failwith "Fiber.self: must be run from inside a fiber."
  | f -> f

let with_on_self_cancel cb (k : unit -> 'a) : 'a =
  let (Any self) = self () in
  let h = add_on_cancel self cb in
  Fun.protect k ~finally:(fun () -> remove_on_cancel self h)

let[@inline] check_if_cancelled_ (self : _ t) = PF.check self.pfiber

let check_if_cancelled () =
  match get_cur_exn () with
  | exception Not_set ->
    failwith "Fiber.check_if_cancelled: must be run from inside a fiber."
  | Any self -> check_if_cancelled_ self

let yield () : unit =
  match get_cur_exn () with
  | exception Not_set ->
    failwith "Fiber.yield: must be run from inside a fiber."
  | Any self ->
    check_if_cancelled_ self;
    PF.yield ();
    check_if_cancelled_ self
