type 'a t = {
  mutex: Mutex.t;
  mutable content: 'a;
}

let create content : _ t = { mutex = Mutex.create (); content }

let[@inline never] with_ (self : _ t) f =
  Mutex.lock self.mutex;
  match f self.content with
  | x ->
    Mutex.unlock self.mutex;
    x
  | exception e ->
    Mutex.unlock self.mutex;
    raise e

let[@inline] mutex self = self.mutex
let[@inline] update self f = with_ self (fun x -> self.content <- f x)

let[@inline] update_map l f =
  with_ l (fun x ->
      let x', y = f x in
      l.content <- x';
      y)

let[@inline never] get l =
  Mutex.lock l.mutex;
  let x = l.content in
  Mutex.unlock l.mutex;
  x

let[@inline never] set l x =
  Mutex.lock l.mutex;
  l.content <- x;
  Mutex.unlock l.mutex
