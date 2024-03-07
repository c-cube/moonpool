(** A handle to cancel something *)
type t =
  | Cancel1 : ('a -> unit) * 'a -> t
  | Cancel2 : ('a -> 'b -> unit) * 'a * 'b -> t

let cancel = function
  | Cancel1 (f, x) -> f x
  | Cancel2 (f, x, y) -> f x y
