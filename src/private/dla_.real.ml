type t = Domain_local_await.t = {
  release: unit -> unit;
  await: unit -> unit;
}

let using : prepare_for_await:(unit -> t) -> while_running:(unit -> 'a) -> 'a =
  Domain_local_await.using

let setup_domain () = Domain_local_await.per_thread (module Thread)
