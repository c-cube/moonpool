module A = Atomic_

type 'a or_error = ('a, exn * Printexc.raw_backtrace) result
type 'a waiter = 'a or_error -> unit

type 'a state =
  | Done of 'a or_error
  | Waiting of 'a waiter list

type 'a t = { st: 'a state A.t } [@@unboxed]
type 'a promise = 'a t

let make () =
  let fut = { st = A.make (Waiting []) } in
  fut, fut

let of_result x : _ t = { st = A.make (Done x) }
let[@inline] return x : _ t = of_result (Ok x)
let[@inline] fail e bt : _ t = of_result (Error (e, bt))

let[@inline] is_resolved self : bool =
  match A.get self.st with
  | Done _ -> true
  | Waiting _ -> false

let[@inline] peek self : _ option =
  match A.get self.st with
  | Done x -> Some x
  | Waiting _ -> None

let on_result (self : _ t) (f : _ waiter) : unit =
  while
    let st = A.get self.st in
    match st with
    | Done x ->
      f x;
      false
    | Waiting l ->
      let must_retry = not (A.compare_and_set self.st st (Waiting (f :: l))) in
      must_retry
  do
    ()
  done

exception Already_fulfilled

let fulfill (self : _ t) (r : _ result) : unit =
  while
    let st = A.get self.st in
    match st with
    | Done _ -> raise Already_fulfilled
    | Waiting l ->
      let did_swap = A.compare_and_set self.st st (Done r) in
      if did_swap then (
        (* success, now call all the waiters *)
        List.iter (fun f -> try f r with _ -> ()) l;
        false
      ) else
        true
  do
    ()
  done

let[@inline] fulfill_idempotent self r =
  try fulfill self r with Already_fulfilled -> ()

(* ### combinators ### *)

let spawn ~on f : _ t =
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

  Pool.run on task;
  fut

let map ?on ~f fut : _ t =
  let map_res r =
    match r with
    | Ok x ->
      (try Ok (f x)
       with e ->
         let bt = Printexc.get_raw_backtrace () in
         Error (e, bt))
    | Error e_bt -> Error e_bt
  in

  match peek fut with
  | Some r -> of_result (map_res r)
  | None ->
    let fut2, promise = make () in
    on_result fut (fun r ->
        let map_and_fulfill () =
          let res = map_res r in
          fulfill promise res
        in

        match on with
        | None -> map_and_fulfill ()
        | Some on -> Pool.run on map_and_fulfill);

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

  match peek fut with
  | Some r -> apply_f_to_res r
  | None ->
    let fut2, promise = make () in
    on_result fut (fun r ->
        let bind_and_fulfill () =
          let f_res_fut = apply_f_to_res r in
          (* forward result *)
          on_result f_res_fut (fun r -> fulfill promise r)
        in

        match on with
        | None -> bind_and_fulfill ()
        | Some on -> Pool.run on bind_and_fulfill);

    fut2

let rec update_ (st : 'a A.t) f : 'a =
  let x = A.get st in
  let y = f x in
  if A.compare_and_set st x y then
    y
  else
    update_ st f

let both a b : _ t =
  match peek a, peek b with
  | Some (Ok x), Some (Ok y) -> return (x, y)
  | Some (Error (e, bt)), _ | _, Some (Error (e, bt)) -> fail e bt
  | _ ->
    let fut, promise = make () in

    let st = A.make `Neither in
    on_result a (function
      | Error err -> fulfill_idempotent promise (Error err)
      | Ok x ->
        (match
           update_ st (function
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
           update_ st (function
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
  | Some (Error (e, bt)), Some (Error _) -> fail e bt
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

let peek_ok_assert_ (self : 'a t) : 'a =
  match A.get self.st with
  | Done (Ok x) -> x
  | _ -> assert false

let join_container_ ~iter ~map ~len cont : _ t =
  let fut, promise = make () in
  let missing = A.make (len cont) in

  (* callback called when a future in [a] is resolved *)
  let on_res = function
    | Ok _ ->
      let n = A.fetch_and_add missing (-1) in
      if n = 1 then (
        (* last future, we know they all succeeded, so resolve [fut] *)
        let res = map peek_ok_assert_ cont in
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

let join_array (a : _ t array) : _ array t =
  match Array.length a with
  | 0 -> return [||]
  | 1 -> map ?on:None a.(1) ~f:(fun x -> [| x |])
  | _ -> join_container_ ~len:Array.length ~map:Array.map ~iter:Array.iter a

let join_list (l : _ t list) : _ list t =
  match l with
  | [] -> return []
  | [ x ] -> map ?on:None x ~f:(fun x -> [ x ])
  | _ -> join_container_ ~len:List.length ~map:List.map ~iter:List.iter l

let wait_array (a : _ t array) : unit t =
  join_container_ a ~iter:Array.iter ~len:Array.length ~map:(fun _f _ -> ())

let wait_list (a : _ t list) : unit t =
  join_container_ a ~iter:List.iter ~len:List.length ~map:(fun _f _ -> ())

let for_ ~on n f : unit t =
  let futs = Array.init n (fun i -> spawn ~on (fun () -> f i)) in
  join_container_
    ~len:(fun () -> n)
    ~iter:(fun f () -> Array.iter f futs)
    ~map:(fun _f () -> ())
    ()

(* ### blocking ### *)

let wait_block (self : 'a t) : 'a or_error =
  match A.get self.st with
  | Done x -> x (* fast path *)
  | Waiting _ ->
    let real_block () =
      (* use queue only once *)
      let q = S_queue.create () in
      on_result self (fun r -> S_queue.push q r);
      S_queue.pop q
    in

    (* a bit of spinlock *)
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

module type INFIX = sig
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module Infix_ (X : sig
  val pool : Pool.t option
end) : INFIX = struct
  let[@inline] ( >|= ) x f = map ?on:X.pool ~f x
  let[@inline] ( >>= ) x f = bind ?on:X.pool ~f x
  let ( let+ ) = ( >|= )
  let ( let* ) = ( >>= )
  let ( and+ ) = both
  let ( and* ) = both
end

include Infix_ (struct
  let pool = None
end)

module Infix (X : sig
  val pool : Pool.t
end) =
Infix_ (struct
  let pool = Some X.pool
end)
