type 'a t = {
  max_size: int;
  q: 'a Queue.t;
  mutex: Mutex.t;
  cond_push: Condition.t;
  cond_pop: Condition.t;
  mutable closed: bool;
}

exception Closed

let create ~max_size () : _ t =
  if max_size < 1 then invalid_arg "Bounded_queue.create";
  {
    max_size;
    mutex = Mutex.create ();
    cond_push = Condition.create ();
    cond_pop = Condition.create ();
    q = Queue.create ();
    closed = false;
  }

let close (self : _ t) =
  Mutex.lock self.mutex;
  if not self.closed then (
    self.closed <- true;
    (* awake waiters so they fail *)
    Condition.broadcast self.cond_push;
    Condition.broadcast self.cond_pop
  );
  Mutex.unlock self.mutex

(** Check if the queue is full. Precondition: [self.mutex] is acquired. *)
let[@inline] is_full_ (self : _ t) : bool = Queue.length self.q >= self.max_size

let push (self : _ t) x : unit =
  let continue = ref true in
  Mutex.lock self.mutex;
  while !continue do
    if self.closed then (
      (* push always fails on a closed queue *)
      Mutex.unlock self.mutex;
      raise Closed
    ) else if is_full_ self then
      Condition.wait self.cond_push self.mutex
    else (
      let was_empty = Queue.is_empty self.q in
      Queue.push x self.q;
      if was_empty then Condition.broadcast self.cond_pop;

      (* exit loop *)
      continue := false;
      Mutex.unlock self.mutex
    )
  done

let pop (self : 'a t) : 'a =
  Mutex.lock self.mutex;
  let rec loop () =
    if Queue.is_empty self.q then (
      if self.closed then (
        (* pop fails on a closed queue if it's also empty,
           otherwise it still returns the remaining elements *)
        Mutex.unlock self.mutex;
        raise Closed
      );

      Condition.wait self.cond_pop self.mutex;
      (loop [@tailcall]) ()
    ) else (
      let was_full = is_full_ self in
      let x = Queue.pop self.q in
      (* wakeup pushers that were blocked *)
      if was_full then Condition.broadcast self.cond_push;
      Mutex.unlock self.mutex;
      x
    )
  in
  loop ()

let try_pop ~force_lock (self : _ t) : _ option =
  let has_lock =
    if force_lock then (
      Mutex.lock self.mutex;
      true
    ) else
      Mutex.try_lock self.mutex
  in
  if has_lock then (
    if self.closed then (
      Mutex.unlock self.mutex;
      raise Closed
    );
    let was_full_before_pop = is_full_ self in
    match Queue.pop self.q with
    | x ->
      (* wakeup pushers that are blocked *)
      if was_full_before_pop then Condition.broadcast self.cond_push;
      Mutex.unlock self.mutex;
      Some x
    | exception Queue.Empty ->
      Mutex.unlock self.mutex;
      None
  ) else
    None

let try_push ~force_lock (self : _ t) x : bool =
  let has_lock =
    if force_lock then (
      Mutex.lock self.mutex;
      true
    ) else
      Mutex.try_lock self.mutex
  in
  if has_lock then (
    if self.closed then (
      Mutex.unlock self.mutex;
      raise Closed
    );

    if is_full_ self then (
      Mutex.unlock self.mutex;
      false
    ) else (
      let was_empty = Queue.is_empty self.q in
      Queue.push x self.q;
      if was_empty then Condition.broadcast self.cond_pop;
      Mutex.unlock self.mutex;
      true
    )
  ) else
    false

let[@inline] max_size self = self.max_size

let size (self : _ t) : int =
  Mutex.lock self.mutex;
  let n = Queue.length self.q in
  Mutex.unlock self.mutex;
  n

let transfer (self : 'a t) q2 : unit =
  Mutex.lock self.mutex;
  let continue = ref true in
  while !continue do
    if Queue.is_empty self.q then (
      if self.closed then (
        Mutex.unlock self.mutex;
        raise Closed
      );
      Condition.wait self.cond_pop self.mutex
    ) else (
      let was_full = is_full_ self in
      Queue.transfer self.q q2;
      if was_full then Condition.broadcast self.cond_push;
      continue := false;
      Mutex.unlock self.mutex
    )
  done

type 'a gen = unit -> 'a option
type 'a iter = ('a -> unit) -> unit

let to_iter self k =
  try
    while true do
      let x = pop self in
      k x
    done
  with Closed -> ()

let to_gen self : _ gen =
 fun () ->
  match pop self with
  | exception Closed -> None
  | x -> Some x

let rec to_seq self : _ Seq.t =
 fun () ->
  match pop self with
  | exception Closed -> Seq.Nil
  | x -> Seq.Cons (x, to_seq self)
