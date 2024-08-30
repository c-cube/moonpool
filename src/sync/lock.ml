module Mutex = Picos_sync.Mutex

type 'a t = {
  mutex: Mutex.t;
  mutable content: 'a;
}

let create content : _ t = { mutex = Mutex.create (); content }

let with_ (self : _ t) f =
  Mutex.lock self.mutex;
  try
    let x = f self.content in
    Mutex.unlock self.mutex;
    x
  with e ->
    Mutex.unlock self.mutex;
    raise e

let[@inline] mutex self = self.mutex
let[@inline] update self f = with_ self (fun x -> self.content <- f x)

let[@inline] update_map l f =
  with_ l (fun x ->
      let x', y = f x in
      l.content <- x';
      y)

let get l =
  Mutex.lock l.mutex;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let set l x =
  Mutex.lock l.mutex;
  l.content <- x;
  Mutex.unlock l.mutex
