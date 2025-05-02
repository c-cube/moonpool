val fut_of_moonpool : 'a Moonpool.Fut.t -> 'a Eio.Promise.or_exn

val await_moonpool : 'a Moonpool.Fut.t -> 'a
(** Await a moonpool future *)
