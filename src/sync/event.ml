include Picos_std_event.Event

let[@inline] of_fut (fut : _ Moonpool.Fut.t) : _ t =
  from_computation (Moonpool.Fut.Private_.as_computation fut)
