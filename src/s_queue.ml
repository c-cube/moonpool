type 'a t = {
  mutex: Mutex.t;
  cond: Condition.t;
  q: 'a Queue.t;
}

let create () : _ t =
  { mutex = Mutex.create (); cond = Condition.create (); q = Queue.create () }

let push (self : _ t) x : unit =
  Mutex.lock self.mutex;
  Queue.push x self.q;
  Condition.signal self.cond;
  Mutex.unlock self.mutex

let pop (self : 'a t) : 'a =
  Mutex.lock self.mutex;
  let rec loop () =
    if Queue.is_empty self.q then (
      Condition.wait self.cond self.mutex;
      (loop [@tailcall]) ()
    ) else (
      let x = Queue.pop self.q in
      Mutex.unlock self.mutex;
      x
    )
  in
  loop ()
