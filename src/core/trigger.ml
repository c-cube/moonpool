(** Triggers from picos
    @since 0.7 *)

include Picos.Trigger

let[@inline] await_exn (self : t) = await self |> Option.iter Exn_bt.raise
