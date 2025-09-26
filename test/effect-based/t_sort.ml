open Moonpool
module FJ = Moonpool_forkjoin

let rec select_sort arr i len =
  if len >= 2 then (
    let idx = ref i in
    for j = i + 1 to i + len - 1 do
      if arr.(j) < arr.(!idx) then idx := j
    done;
    let tmp = arr.(!idx) in
    arr.(!idx) <- arr.(i);
    arr.(i) <- tmp;
    select_sort arr (i + 1) (len - 1)
  )

let sorted arr =
  try
    for i = 0 to Array.length arr - 2 do
      if arr.(i) > arr.(i + 1) then (
        Printf.printf "not sorted at %d\n%!" i;
        raise Exit
      )
    done;
    true
  with Exit -> false

let () =
  let arr = [| 4; 2; 1; 5; 1; 10; 3 |] in
  select_sort arr 0 (Array.length arr);
  assert (sorted arr)

let rec quicksort arr i len : unit =
  if len <= 10 then
    select_sort arr i len
  else (
    let pivot = arr.(i + (len / 2)) in
    let low = ref (i - 1) in
    let high = ref (i + len) in

    while !low < !high do
      incr low;
      decr high;
      while arr.(!low) < pivot do
        incr low
      done;
      while arr.(!high) > pivot do
        decr high
      done;
      if !low < !high then (
        let tmp = arr.(!low) in
        arr.(!low) <- arr.(!high);
        arr.(!high) <- tmp
      )
    done;

    FJ.both_ignore
      (fun () -> quicksort arr i (!low - i))
      (fun () -> quicksort arr !low (len - (!low - i)))
  )

let pool = Moonpool.Ws_pool.create ~num_threads:8 ()

let () =
  let arr = Array.init 400_000 (fun _ -> Random.int 300_000) in
  Fut.spawn ~on:pool (fun () -> quicksort arr 0 (Array.length arr))
  |> Fut.wait_block_exn;
  (* Printf.printf "arr: [%s]\n%!" *)
  (*   (String.concat ", " @@ List.map string_of_int @@ Array.to_list arr); *)
  assert (sorted arr)
