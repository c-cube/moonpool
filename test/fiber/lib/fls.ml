open! Moonpool
module A = Atomic
module F = Moonpool_fib.Fiber
module FLS = Moonpool_fib.Fls

(* ### dummy little tracing system with local storage *)

type span_id = int

let k_parent : span_id Hmap.key = Hmap.Key.create ()
let ( let@ ) = ( @@ )
let spf = Printf.sprintf

module Span = struct
  let new_id_ : unit -> span_id =
    let n = A.make 0 in
    fun () -> A.fetch_and_add n 1

  type t = {
    id: span_id;
    parent: span_id option;
    msg: string;
  }
end

module Tracer = struct
  type t = { spans: Span.t list A.t }

  let create () : t = { spans = A.make [] }
  let get self = A.get self.spans

  let add (self : t) span =
    while
      let old = A.get self.spans in
      not (A.compare_and_set self.spans old (span :: old))
    do
      ()
    done

  let with_span self name f =
    let id = Span.new_id_ () in
    let parent = FLS.get_in_local_hmap_opt k_parent in
    let span = { Span.id; parent; msg = name } in
    add self span;
    FLS.with_in_local_hmap k_parent id f
end

module Render = struct
  type span_tree = {
    msg: string;  (** message of the span at the root *)
    children: span_tree list;
  }

  type t = { roots: span_tree list }

  let build (tracer : Tracer.t) : t =
    let tops : (span_id, Span.t) Hashtbl.t = Hashtbl.create 16 in
    let children : (span_id, Span.t list) Hashtbl.t = Hashtbl.create 16 in

    (* everyone is a root at first *)
    let all_spans = Tracer.get tracer in
    List.iter (fun (sp : Span.t) -> Hashtbl.add tops sp.id sp) all_spans;

    (* now consider the parenting relationships *)
    let add_span_to_parent (span : Span.t) =
      match span.parent with
      | None -> ()
      | Some p ->
        Hashtbl.remove tops span.id;
        let l = try Hashtbl.find children p with Not_found -> [] in
        Hashtbl.replace children p (span :: l)
    in
    List.iter add_span_to_parent all_spans;

    (* build the tree *)
    let rec build_tree (sp : Span.t) : span_tree =
      let children = try Hashtbl.find children sp.id with Not_found -> [] in
      let children = List.map build_tree children |> List.sort Stdlib.compare in
      { msg = sp.msg; children }
    in

    let roots =
      Hashtbl.fold (fun _ sp l -> build_tree sp :: l) tops []
      |> List.sort Stdlib.compare
    in

    { roots }

  let pp (oc : out_channel) (self : t) : unit =
    let rec pp_tree indent out (t : span_tree) =
      let prefix = String.make indent ' ' in
      Printf.fprintf out "%s%S\n" prefix t.msg;
      List.iter (pp_tree (indent + 2) out) t.children
    in
    List.iter (pp_tree 2 oc) self.roots
end

let run ~pool ~pool_name () =
  let tracer = Tracer.create () in

  let sub_sub_child ~idx ~idx_child ~idx_sub ~idx_sub_sub () =
    let@ () =
      Tracer.with_span tracer
        (spf "child_%d.%d.%d.%d" idx idx_child idx_sub idx_sub_sub)
    in

    for j = 1 to 5 do
      let@ () = Tracer.with_span tracer (spf "iter.loop %d" j) in
      F.yield ()
    done
  in

  let sub_child ~idx ~idx_child ~idx_sub () =
    let@ () =
      Tracer.with_span tracer (spf "child_%d.%d.%d" idx idx_child idx_sub)
    in

    for i = 1 to 10 do
      let@ () = Tracer.with_span tracer (spf "iter.loop %02d" i) in
      F.yield ()
    done;

    let subs =
      List.init 2 (fun idx_sub_sub ->
          F.spawn ~protect:true (fun () ->
              sub_sub_child ~idx ~idx_child ~idx_sub ~idx_sub_sub ()))
    in
    List.iter F.await subs
  in

  let top_child ~idx ~idx_child () =
    let@ () = Tracer.with_span tracer (spf "child.%d.%d" idx idx_child) in

    let subs =
      List.init 2 (fun k ->
          F.spawn ~protect:true @@ fun () ->
          sub_child ~idx ~idx_child ~idx_sub:k ())
    in

    let@ () =
      Tracer.with_span tracer
        (spf "child.%d.%d.99.await_children" idx idx_child)
    in
    List.iter F.await subs
  in

  let top idx =
    let@ () = Tracer.with_span tracer (spf "top_%d" idx) in

    let subs =
      List.init 5 (fun j ->
          F.spawn ~protect:true @@ fun () -> top_child ~idx ~idx_child:j ())
    in

    List.iter F.await subs
  in

  Printf.printf "run test on pool = %s\n" pool_name;
  let fibs =
    List.init 8 (fun idx -> F.spawn_top ~on:pool (fun () -> top idx))
  in
  List.iter F.await fibs;

  Printf.printf "tracing complete\n";
  Printf.printf "spans:\n";
  let tree = Render.build tracer in
  Render.pp stdout tree;
  Printf.printf "done\n%!";
  ()
