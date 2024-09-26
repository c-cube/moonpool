module A = Atomic_

type 'a or_error = 'a Fut.or_error
type 'a pop_waiter = 'a Fut.promise
type 'a push_waiter = 'a * unit Fut.promise

let[@inline] list_is_empty_ = function
  | [] -> true
  | _ :: _ -> false

(** Simple functional queue *)
module Q : sig
  type 'a t

  val empty : 'a t
  val return : 'a -> 'a t
  val is_empty : _ t -> bool

  exception Empty

  val pop_exn : 'a t -> 'a * 'a t
  val push : 'a t -> 'a -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
end = struct
  type 'a t = {
    hd: 'a list;
    tl: 'a list;
  }
  (** Queue containing elements of type 'a.

      invariant: if hd=[], then tl=[] *)

  let empty = { hd = []; tl = [] }
  let[@inline] return x : _ t = { hd = [ x ]; tl = [] }

  let[@inline] make_ hd tl =
    match hd with
    | [] -> { hd = List.rev tl; tl = [] }
    | _ :: _ -> { hd; tl }

  let[@inline] is_empty self = list_is_empty_ self.hd
  let[@inline] push self x : _ t = make_ self.hd (x :: self.tl)

  let iter f (self : _ t) : unit =
    List.iter f self.hd;
    List.iter f self.tl

  exception Empty

  let pop_exn self =
    match self.hd with
    | [] ->
      assert (list_is_empty_ self.tl);
      raise Empty
    | x :: hd' ->
      let self' = make_ hd' self.tl in
      x, self'
end

exception Closed

type 'a t = {
  q: 'a Queue.t;
  mutex: Mutex.t;  (** protects critical section *)
  mutable closed: bool;
  max_size: int;
  push_waiters: Trigger.t Queue.t;
  pop_waiters: Trigger.t Queue.t;
}

let create ~max_size () : _ t =
  if max_size < 0 then invalid_arg "Chan: max_size < 0";
  {
    max_size;
    mutex = Mutex.create ();
    closed = false;
    q = Queue.create ();
    push_waiters = Queue.create ();
    pop_waiters = Queue.create ();
  }

let try_push (self : _ t) x : bool =
  let res = ref false in
  if Mutex.try_lock self.mutex then (
    if self.closed then (
      Mutex.unlock self.mutex;
      raise Closed
    );

    match Queue.length self.q with
    | 0 ->
      let to_awake = Queue.create () in
      Queue.push x self.q;
      Queue.transfer self.pop_waiters to_awake;
      res := true;
      Mutex.unlock self.mutex;
      (* wake up pop triggers if needed. Be careful to do that
         outside the critical section*)
      Queue.iter Trigger.signal to_awake
    | n when n < self.max_size ->
      Queue.push x self.q;
      Mutex.unlock self.mutex
    | _ -> Mutex.unlock self.mutex
  );
  !res

let try_pop (type elt) self : elt option =
  let res = ref None in
  if Mutex.try_lock self.mutex then (
    (match Queue.pop self.q with
    | exception Queue.Empty ->
      if self.closed then (
        Mutex.unlock self.mutex;
        raise Closed
      )
    | x -> res := Some x);
    Mutex.unlock self.mutex
  );
  !res

let close (self : _ t) : unit =
  let q = Queue.create () in
  Mutex.lock self.mutex;
  if not self.closed then (
    self.closed <- true;
    Queue.transfer self.pop_waiters q;
    Queue.transfer self.push_waiters q
  );
  Mutex.unlock self.mutex;
  Queue.iter Trigger.signal q

[@@@ifge 5.0]

let rec push (self : _ t) x : unit =
  Mutex.lock self.mutex;

  if self.closed then (
    Mutex.unlock self.mutex;
    raise Closed
  );

  match Queue.length self.q with
  | 0 ->
    Queue.push x self.q;
    let to_wakeup = Queue.create () in
    Queue.transfer self.pop_waiters to_wakeup;
    Mutex.unlock self.mutex;
    Queue.iter Trigger.signal to_wakeup
  | n when n < self.max_size ->
    Queue.push x self.q;
    Mutex.unlock self.mutex
  | _ ->
    let tr = Trigger.create () in
    Queue.push tr self.push_waiters;
    Mutex.unlock self.mutex;
    Trigger.await_exn tr;
    push self x

let rec pop (self : 'a t) : 'a =
  Mutex.lock self.mutex;
  match Queue.pop self.q with
  | x ->
    if Queue.is_empty self.q then (
      let to_wakeup = Queue.create () in
      Queue.transfer self.push_waiters to_wakeup;
      Mutex.unlock self.mutex;
      Queue.iter Trigger.signal to_wakeup
    ) else
      Mutex.unlock self.mutex;
    x
  | exception Queue.Empty ->
    if self.closed then (
      Mutex.unlock self.mutex;
      raise Closed
    );

    let tr = Trigger.create () in
    Queue.push tr self.pop_waiters;
    Mutex.unlock self.mutex;
    Trigger.await_exn tr;
    pop self

[@@@endif]

(* TODO: remove
   (** A waiter queue, somewhat similar to a condition. *)
   module Waiters_ = struct
     type t = { waiters: Trigger.t Q.t A.t } [@@unboxed]

     let create () : t = { waiters = A.make Q.empty }

     let add_waiter self (tr : Trigger.t) : unit =
       while
         let q = A.get self.waiters in
         not (A.compare_and_set self.waiters q (Q.push q tr))
       do
         Domain_.relax ()
       done

     let wait_await (self : t) : unit =
       let tr = Trigger.create () in
       add_waiter self tr;
       Trigger.await_exn tr

     exception Empty = Q.Empty

     let rec pop_waiter (self : t) : Trigger.t =
       let q = A.get self.waiters in
       let x, q' = Q.pop_exn q in
       if A.compare_and_set self.waiters q q' then
         x
       else (
         Domain_.relax ();
         pop_waiter self
       )

     let rec pop_all (self : t) : Trigger.t Q.t =
       let q = A.get self.waiters in
       if A.compare_and_set self.waiters q Q.empty then
         q
       else (
         Domain_.relax ();
         pop_all self
       )

     let signal (self : t) : unit =
       match pop_waiter self with
       | exception Empty -> ()
       | tr -> Trigger.signal tr

     let broadcast (self : t) : unit =
       let waiters = pop_all self in
       Q.iter Trigger.signal waiters
   end
*)
