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
