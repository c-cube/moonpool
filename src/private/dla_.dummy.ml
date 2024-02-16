(** Interface to Domain-local-await.

    This is used to handle the presence or absence of DLA. *)

type t = {
  release: unit -> unit;
  await: unit -> unit;
}

let using : prepare_for_await:(unit -> t) -> while_running:(unit -> 'a) -> 'a =
 fun ~prepare_for_await:_ ~while_running -> while_running ()

let setup_domain () = ()
