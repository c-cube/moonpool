(* regression test for #45 *)

open Moonpool

let ( let@ ) = ( @@ )

let rec fib_direct x =
  if x <= 1 then
    1
  else
    fib_direct (x - 1) + fib_direct (x - 2)

let cutoff = 8

let rec fib_await ~on x : int Fut.t =
  if x <= cutoff then
    Fut.spawn ~on (fun () -> fib_direct x)
  else
    Fut.spawn ~on (fun () ->
        let n1 = fib_await ~on (x - 1) in
        let n2 = fib_await ~on (x - 2) in
        let n1 = Fut.await n1 in
        let n2 = Fut.await n2 in
        n1 + n2)

(** Read VmHWM (peak RSS in kB) from /proc/self/status. *)
let get_vmhwm_kb () : int option =
  let path = "/proc/self/status" in
  match In_channel.with_open_bin path In_channel.input_all with
  | exception Sys_error _ -> None
  | content ->
    let lines = String.split_on_char '\n' content in
    List.find_map
      (fun line -> Scanf.sscanf_opt line "VmHWM: %d kB" Fun.id)
      lines

let max_rss_bytes = 150_000_000

let () =
  let@ pool = Ws_pool.with_ ~num_threads:4 () in
  let result = fib_await ~on:pool 40 |> Fut.wait_block_exn in
  assert (result = 165580141);
  match get_vmhwm_kb () with
  | None ->
    Printf.printf "fib 40 = %d (skip RSS check: no /proc/self/status)\n%!"
      result
  | Some hwm_kb ->
    let hwm_bytes = hwm_kb * 1024 in
    Printf.printf "fib 40 = %d, peak RSS = %d bytes\n%!" result hwm_bytes;
    if hwm_bytes > max_rss_bytes then (
      Printf.eprintf "FAIL: peak RSS %d bytes exceeds limit %d bytes\n%!"
        hwm_bytes max_rss_bytes;
      exit 1
    )
