module BQ = Moonpool.Bounded_queue
module Bb_queue = Moonpool.Blocking_queue
module A = Moonpool.Atomic

let spawn f = ignore (Moonpool.start_thread_on_some_domain f () : Thread.t)

let () =
  let bq = BQ.create ~max_size:3 () in
  BQ.push bq 1;
  BQ.push bq 2;
  assert (BQ.size bq = 2);
  assert (BQ.pop bq = 1);
  assert (BQ.pop bq = 2);

  assert (BQ.try_pop ~force_lock:true bq = None);
  spawn (fun () -> BQ.push bq 3);
  assert (BQ.pop bq = 3)

let () =
  (* cannot create with size 0 *)
  assert (
    try
      ignore (BQ.create ~max_size:0 ());
      false
    with _ -> true)

let () =
  let bq = BQ.create ~max_size:3 () in
  BQ.push bq 1;
  BQ.push bq 2;
  assert (BQ.size bq = 2);
  assert (BQ.pop bq = 1);

  BQ.close bq;
  assert (BQ.pop bq = 2);
  assert (
    try
      ignore (BQ.pop bq);
      false
    with BQ.Closed -> true);
  assert (
    try
      ignore (BQ.push bq 42);
      false
    with BQ.Closed -> true)

let () =
  let bq = BQ.create ~max_size:2 () in
  let side_q = Bb_queue.create () in
  BQ.push bq 1;
  BQ.push bq 2;

  spawn (fun () ->
      for i = 3 to 10 do
        BQ.push bq i;
        Bb_queue.push side_q (`Pushed i)
      done);

  (* make space for new element *)
  assert (BQ.pop bq = 1);
  assert (Bb_queue.pop side_q = `Pushed 3);
  assert (BQ.pop bq = 2);
  assert (BQ.pop bq = 3);
  for j = 4 to 10 do
    assert (BQ.pop bq = j);
    assert (Bb_queue.pop side_q = `Pushed j)
  done;
  assert (BQ.size bq = 0);
  ()

let () =
  let bq = BQ.create ~max_size:5 () in

  let bq1 = BQ.create ~max_size:10 () in
  let bq2 = BQ.create ~max_size:10 () in

  let bq_res = BQ.create ~max_size:2 () in

  (* diamond:
       bq -------> bq1
        |           |
        |           |
        v           v
        bq2 -----> bq_res *)
  spawn (fun () ->
      BQ.to_iter bq (BQ.push bq1);
      BQ.close bq1);
  spawn (fun () ->
      BQ.to_iter bq (BQ.push bq2);
      BQ.close bq2);
  spawn (fun () -> BQ.to_iter bq1 (BQ.push bq_res));
  spawn (fun () -> BQ.to_iter bq2 (BQ.push bq_res));

  let n = 100_000 in

  (* push into [bq] *)
  let sum = A.make 0 in
  spawn (fun () ->
      for i = 1 to n do
        ignore (A.fetch_and_add sum i : int);
        BQ.push bq i
      done;
      BQ.close bq);

  let sum' = ref 0 in
  for _j = 1 to n do
    let x = BQ.pop bq_res in
    sum' := x + !sum'
  done;
  assert (BQ.size bq_res = 0);
  assert (A.get sum = !sum')
