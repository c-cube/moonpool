(** Level-triggered wakeup: an [unpark] before [park] is not lost.

    Inspired by WTF https://webkit.org/blog/6161/locking-in-webkit/ and parking
    lot. *)

type t = {
  m: Mutex.t;
  c: Condition.t;
  mutable permit: bool;  (** when true, parked thread can wake up *)
}

let create () : t =
  { m = Mutex.create (); c = Condition.create (); permit = false }

(** Wait until there's a permit, then consume it. *)
let park (self : t) : unit =
  Mutex.lock self.m;
  while not self.permit do
    Condition.wait self.c self.m
  done;
  self.permit <- false;
  Mutex.unlock self.m

let unpark (self : t) : unit =
  Mutex.lock self.m;
  self.permit <- true;
  Condition.signal self.c;
  Mutex.unlock self.m
