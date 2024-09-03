include module type of struct
  include Picos_std_event.Event
end

val of_fut : 'a Moonpool.Fut.t -> 'a t
