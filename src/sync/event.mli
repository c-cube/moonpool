include module type of struct
  include Picos_std_event.Event
end

val of_fut : 'a Moonpool.Fut.t -> 'a t

module Infix : sig
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

include module type of Infix
