let fut_of_moonpool (fut : 'a Moonpool.Fut.t) : 'a Eio.Promise.or_exn =
  match Moonpool.Fut.peek fut with
  | Some (Ok r) -> Eio.Promise.create_resolved @@ Ok r
  | Some (Error (exn, _bt)) ->
    let prom, resolver = Eio.Promise.create () in
    Eio.Promise.resolve_error resolver exn;
    prom
  | None ->
    let prom, resolver = Eio.Promise.create () in
    Moonpool.Fut.on_result fut (function
      | Ok r -> Eio.Promise.resolve_ok resolver r
      | Error (exn, _) -> Eio.Promise.resolve_error resolver exn);
    prom

let await_moonpool (fut : 'a Moonpool.Fut.t) : 'a =
  match Moonpool.Fut.peek fut with
  | Some (Ok r) -> r
  | Some (Error (exn, bt)) -> Printexc.raise_with_backtrace exn bt
  | None ->
    let prom, resolver = Eio.Promise.create () in
    Moonpool.Fut.on_result fut (function
      | Ok r -> Eio.Promise.resolve_ok resolver r
      | Error (exn, _) -> Eio.Promise.resolve_error resolver exn);
    Eio.Promise.await_exn prom
