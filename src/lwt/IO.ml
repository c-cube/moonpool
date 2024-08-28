open Base

let await_readable fd : unit =
  let trigger = Trigger.create () in
  Perform_action_in_lwt.schedule
  @@ Action.Wait_readable
       ( fd,
         fun cancel ->
           Trigger.signal trigger;
           Lwt_engine.stop_event cancel );
  Trigger.await_exn trigger

let rec read fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.read fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      await_readable fd;
      read fd buf i len
    | n -> n
  )

let await_writable fd =
  let trigger = Trigger.create () in
  Perform_action_in_lwt.schedule
  @@ Action.Wait_writable
       ( fd,
         fun cancel ->
           Trigger.signal trigger;
           Lwt_engine.stop_event cancel );
  Trigger.await_exn trigger

let rec write_once fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.write fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      await_writable fd;
      write_once fd buf i len
    | n -> n
  )

let write fd buf i len : unit =
  let i = ref i in
  let len = ref len in
  while !len > 0 do
    let n = write_once fd buf !i !len in
    i := !i + n;
    len := !len - n
  done

(** Sleep for the given amount of seconds *)
let sleep_s (f : float) : unit =
  if f > 0. then (
    let trigger = Trigger.create () in
    Perform_action_in_lwt.schedule
    @@ Action.Sleep
         ( f,
           false,
           fun cancel ->
             Trigger.signal trigger;
             Lwt_engine.stop_event cancel );
    Trigger.await_exn trigger
  )
