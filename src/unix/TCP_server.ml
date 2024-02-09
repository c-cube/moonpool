open Common_
module A = M.Atomic

type t = {
  active: bool A.t;
  cancel: Cancel_handle.t;
  port: int;
  fd: Unix.file_descr;
  runner: M.Runner.t;
}

let port self = self.port

let with_server ?(addr = Unix.inet_addr_any) ?(port = 0) ?(after_init = ignore)
    ~runner ~handle_client () : unit =
  let active = A.make true in
  let cancel = Cancel_handle.create_with (fun () -> A.set active false) in
  let server_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind server_sock (Unix.ADDR_INET (addr, port));
  Unix.set_nonblock server_sock;
  Unix.listen server_sock 16;

  (* now get the real port *)
  let port =
    match Unix.getsockname server_sock with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> assert false
  in

  (* Unix.setsockopt sock *)
  let finally () = Unix.close server_sock in
  let@ () = Fun.protect ~finally in

  let self = { active; cancel; port; fd = server_sock; runner } in
  after_init self;

  while A.get self.active do
    (* accept new client *)
    match Unix.accept server_sock with
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      (* wait for [sock] to be ready *)
      let@ _sp = Tracing_.with_span "tcp-server.suspend" in
      Moonpool.Private.Suspend_.suspend
        {
          handle =
            (fun ~ls ~run:_ ~resume sus ->
              Ev_loop.wait_readable server_sock cancel (fun _ ->
                  resume ~ls sus @@ Ok ()));
        }
    | client_sock, client_addr ->
      (* handle client *)
      Unix.setsockopt client_sock Unix.TCP_NODELAY true;
      Unix.set_nonblock client_sock;

      let ic = IO_in.of_unix_fd client_sock in
      let oc = IO_out.of_unix_fd client_sock in

      M.Runner.run_async runner ~name:"tcp.handle-client" (fun () ->
          let@ () = Fun.protect ~finally:(fun () -> Unix.close client_sock) in
          handle_client self client_addr ic oc)
  done
