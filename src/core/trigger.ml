(** Triggers from picos
    @since NEXT_RELEASE *)

include Picos.Trigger

let[@inline] await_exn (self : t) = await self |> Option.iter Exn_bt.raise
