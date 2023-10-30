type 'a t = {
  mutex: Mutex.t;
  cond: Condition.t;
  q: 'a Queue.t;
  mutable closed: bool;
}

exception Closed

let create () : _ t =
  {
    mutex = Mutex.create ();
    cond = Condition.create ();
    q = Queue.create ();
    closed = false;
  }

let close (self : _ t) =
  Mutex.lock self.mutex;
  if not self.closed then (
    self.closed <- true;
    Condition.broadcast self.cond (* awake waiters so they fail  *)
  );
  Mutex.unlock self.mutex

let push (self : _ t) x : unit =
  Mutex.lock self.mutex;
  if self.closed then (
    Mutex.unlock self.mutex;
    raise Closed
  ) else (
    let was_empty = Queue.is_empty self.q in
    Queue.push x self.q;
    if was_empty then Condition.broadcast self.cond;
    Mutex.unlock self.mutex
  )

let pop (self : 'a t) : 'a =
  Mutex.lock self.mutex;
  let rec loop () =
    if Queue.is_empty self.q then (
      if self.closed then (
        Mutex.unlock self.mutex;
        raise Closed
      );

      Condition.wait self.cond self.mutex;
      (loop [@tailcall]) ()
    ) else (
      let x = Queue.pop self.q in
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
    match Queue.pop self.q with
    | x ->
      Mutex.unlock self.mutex;
      Some x
    | exception Queue.Empty ->
      Mutex.unlock self.mutex;
      None
  ) else
    None

let try_push (self : _ t) x : bool =
  if Mutex.try_lock self.mutex then (
    if self.closed then (
      Mutex.unlock self.mutex;
      raise Closed
    );

    let was_empty = Queue.is_empty self.q in
    Queue.push x self.q;
    if was_empty then Condition.broadcast self.cond;
    Mutex.unlock self.mutex;
    true
  ) else
    false

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
      Condition.wait self.cond self.mutex
    ) else (
      Queue.transfer self.q q2;
      Mutex.unlock self.mutex;
      continue := false
    )
  done

let transfer_into q (self : _ t) : unit =
  if not (Queue.is_empty q) then (
    Mutex.lock self.mutex;

    if self.closed then (
      Mutex.unlock self.mutex;
      raise Closed
    );

    let was_empty = Queue.is_empty self.q in
    Queue.transfer q self.q;
    if was_empty then Condition.broadcast self.cond;
    Mutex.unlock self.mutex
  )

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
