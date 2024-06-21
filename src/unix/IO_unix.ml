open Common_

type file_descr = Unix.file_descr

(** Non blocking read *)
let rec read fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.read fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      (* wait for FD to be ready *)
      let cancel = Cancel_handle.create () in
      let@ () =
        Fiber.with_on_self_cancel (fun _ -> Cancel_handle.cancel cancel)
      in
      Moonpool.Private.Suspend_.suspend
        {
          handle =
            (fun ~run:_ ~resume sus ->
              Ev_loop.wait_readable fd cancel (fun cancel ->
                  resume sus @@ Ok ();
                  Cancel_handle.cancel cancel));
        };
      read fd buf i len
    | n -> n
  )

(** Non blocking write *)
let rec write_once fd buf i len : int =
  if len = 0 then
    0
  else (
    match Unix.write fd buf i len with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      (* wait for FD to be ready *)
      let cancel = Cancel_handle.create () in
      let@ () =
        Fiber.with_on_self_cancel (fun _ -> Cancel_handle.cancel cancel)
      in
      Moonpool.Private.Suspend_.suspend
        {
          handle =
            (fun ~run:_ ~resume sus ->
              Ev_loop.wait_writable fd cancel (fun cancel ->
                  resume sus @@ Ok ();
                  Cancel_handle.cancel cancel));
        };
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
