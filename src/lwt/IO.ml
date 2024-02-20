open Base

let await_readable fd : unit =
  Moonpool.Private.Suspend_.suspend
    {
      handle =
        (fun ~run:_ ~resume sus ->
          Perform_action_in_lwt.schedule
          @@ Action.Wait_readable
               ( fd,
                 fun cancel ->
                   resume sus @@ Ok ();
                   Lwt_engine.stop_event cancel ));
    }

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
  Moonpool.Private.Suspend_.suspend
    {
      handle =
        (fun ~run:_ ~resume sus ->
          Perform_action_in_lwt.schedule
          @@ Action.Wait_writable
               ( fd,
                 fun cancel ->
                   resume sus @@ Ok ();
                   Lwt_engine.stop_event cancel ));
    }

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
  if f > 0. then
    Moonpool.Private.Suspend_.suspend
      {
        handle =
          (fun ~run:_ ~resume sus ->
            Perform_action_in_lwt.schedule
            @@ Action.Sleep
                 ( f,
                   false,
                   fun cancel ->
                     resume sus @@ Ok ();
                     Lwt_engine.stop_event cancel ));
      }
