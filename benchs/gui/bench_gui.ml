(* Tk front-end for the pi / fib_rec benchmarks (see `make bench-pi`,
   `make bench-fib`). Runs hyperfine once per selected configuration and draws
   one bar per result. This is a labltk port of benchs/bench_gui.tcl and
   behaves the same.

   Usage: make bench-gui *)

open Tk

let spf = Printf.sprintf
let top = openTk ()

(* ---- raw Tcl --------------------------------------------------------------

   labltk only wraps the classic Tk widgets: the ttk ones (frame, label,
   button, spinbox, panedwindow, treeview, ...) and the <MouseWheel> event
   have no typed binding, so they are driven through Tcl directly. *)

(* call a Tcl command, each element of [args] being one word *)
let tk (args : string list) : string =
  Protocol.tkEval (Array.of_list (List.map (fun s -> Protocol.TkToken s) args))

let tk_ args = ignore (tk args)

(* evaluate a Tcl script *)
let tcl_ (script : string) : unit = tk_ [ "eval"; script ]

(* build a Tcl list *)
let tcl_list (l : string list) : string = tk ("list" :: l)

(* register [f] as a Tcl command called [name] *)
let tcl_proc name (f : unit -> unit) : unit =
  Tk.register name ~callback:(fun _ -> f ())

(* create a ttk widget; it can then be used with the typed pack/place/bind *)
let ttk kind path opts : Widget.any Widget.widget =
  tk_ ((spf "ttk::%s" kind) :: path :: opts);
  Widget.get_atom path

let atom = Widget.get_atom

(* Tcl variable, for -variable / -textvariable *)
let var init =
  let v = Textvariable.create () in
  Textvariable.set v init;
  v

let vname = Textvariable.name
let get = Textvariable.get

(* ---- configurations ------------------------------------------------------ *)

(* [flag] is "" when the config takes no thread count; an empty [def] means
   the flag is omitted unless the user fills it in. *)
type config = { label : string; args : string; flag : string; def : string }

let c label args flag def = { label; args; flag; def }

let configs = function
  | "pi" ->
    [|
      c "seq" "-mode=seq" "" "";
      c "par1 pool" "-mode par1 -kind=pool" "-j" "8";
      c "par1 fifo" "-mode par1 -kind=fifo" "-j" "8";
      c "forkjoin pool" "-mode forkjoin -kind=pool" "-j" "16";
      c "forkjoin pool" "-mode forkjoin -kind=pool" "-j" "20";
    |]
  | _ ->
    [|
      c "seq" "-seq" "" "";
      c "domainslib" "-dl" "" "";
      c "pool fj" "-kind=pool -fj" "-psize" "";
      c "pool fj" "-kind=pool -fj" "-psize" "20";
      c "pool await" "-kind=pool -await" "-psize" "20";
      c "fifo" "-kind=fifo" "-psize" "4";
      c "pool" "-kind=pool" "-psize" "4";
      c "fifo" "-kind=fifo" "-psize" "8";
      c "pool" "-kind=pool" "-psize" "16";
    |]

let exe = function
  | "pi" -> "benchs/pi.exe"
  | _ -> "benchs/fib_rec.exe"

(* form values *)
let bench = var "pi"
let form_pi_n = var "100_000_000"
let form_fib_n = var "40"
let form_fib_niter = var "2"
let form_fib_cutoff = var "20"
let form_warmup = var "1"
let form_runs = var ""
let form_perf = var "1"
let form_perf_r = var "3"

let tmpdir = Stdlib.Option.value (Sys.getenv_opt "TMPDIR") ~default:"/tmp"
let tmpcsv = Filename.concat tmpdir (spf "bench_gui.%d.csv" (Unix.getpid ()))
let tmpperf = Filename.concat tmpdir (spf "bench_gui.%d.perf" (Unix.getpid ()))

type stats = {
  mean : float;
  stddev : float;
  user : float;
  system : float;
  min : float;
  max : float;
}

type result = Failed | Done of stats

(* one `perf stat` counter (see parse_perf) *)
type counter = {
  event : string;
  value : string;
  unit_ : string;
  var : string;
  pct : string;
  mval : string;
  munit : string;
}

type perf = { runs : int; counters : counter list }

let results : (int, result) Hashtbl.t = Hashtbl.create 16
let perf : (int, perf) Hashtbl.t = Hashtbl.create 16

(* config whose perf data is shown in the detail pane *)
let selected : int option ref = ref None

(* (i, y0, y1) per drawn row, in canvas coordinates, for click handling *)
let rowgeom : (int * int * int) list ref = ref []
let running = ref false

(* the child process we are reading from: output fd, pid *)
let chan : (Unix.file_descr * int) option ref = ref None
let queue : int list ref = ref []

(* per-config checkbox and thread count, rebuilt by build_configs *)
let enabled : Textvariable.textVariable array ref = ref [||]
let threads : Textvariable.textVariable array ref = ref [||]

(* ---- colors -------------------------------------------------------------- *)

let col_bar = "#4a78c2"
let col_fast = "#2e9d5b"
let col_user = "#e0a030"
let col_sys = "#c0504d"
let col_range = "#222222"
let col_sigma = "#111111"
let col_grid = "#dddddd"
let col_text = "#222222"
let col_dim = "#888888"
let col_sel = "#e6eefa"

(* ---- UI ------------------------------------------------------------------ *)

let () =
  Wm.title_set top "moonpool benchmarks";
  Wm.geometry_set top "1100x750"

(* The top bar is a flow layout: groups are placed left to right and wrap
   onto a new line when the window is too narrow (see flow_top). *)
let top_bar = ttk "frame" ".top" []
let () = pack ~side:`Top ~fill:`X [ top_bar ]

let () =
  ignore (ttk "frame" ".top.bench" []);
  let l = ttk "label" ".top.bench.l" [ "-text"; "Benchmark:" ] in
  let radio path value =
    ttk "radiobutton" path
      [ "-text"; value; "-variable"; vname bench; "-value"; value;
        "-command"; "on_bench_change" ]
  in
  let pi = radio ".top.bench.pi" "pi" in
  let fib = radio ".top.bench.fib" "fib" in
  pack ~side:`Left ~padx:3 [ l; pi; fib ]

let () = ignore (ttk "frame" ".top.params" [])

let () =
  ignore (ttk "frame" ".top.hf" []);
  let wl = ttk "label" ".top.hf.wl" [ "-text"; "warmup" ] in
  let w =
    ttk "spinbox" ".top.hf.w"
      [ "-from"; "0"; "-to"; "100"; "-width"; "4"; "-textvariable"; vname form_warmup ]
  in
  let rl = ttk "label" ".top.hf.rl" [ "-text"; "runs (empty=auto)" ] in
  let r =
    ttk "spinbox" ".top.hf.r"
      [ "-from"; "2"; "-to"; "1000"; "-width"; "5"; "-textvariable"; vname form_runs ]
  in
  pack ~side:`Left ~padx:3 [ wl; w; rl; r ]

let () =
  ignore (ttk "frame" ".top.perf" []);
  let cb =
    ttk "checkbutton" ".top.perf.cb" [ "-text"; "perf stat"; "-variable"; vname form_perf ]
  in
  let rl = ttk "label" ".top.perf.rl" [ "-text"; "-r" ] in
  let r =
    ttk "spinbox" ".top.perf.r"
      [ "-from"; "1"; "-to"; "100"; "-width"; "4"; "-textvariable"; vname form_perf_r ]
  in
  pack ~side:`Left ~padx:3 [ cb; rl; r ]

let () =
  ignore (ttk "frame" ".top.btn" []);
  let run = ttk "button" ".top.btn.run" [ "-text"; "Run"; "-command"; "start_run" ] in
  let stop =
    ttk "button" ".top.btn.stop"
      [ "-text"; "Stop"; "-command"; "stop_run"; "-state"; "disabled" ]
  in
  pack ~side:`Left ~padx:3 [ run; stop ]

let flow_top () =
  let pad = 6 and gap = 18 in
  let w_top = Winfo.width top_bar in
  let w_top = if w_top <= 1 then Winfo.reqwidth top else w_top in
  let x = ref pad and y = ref pad and lineh = ref 0 in
  List.iter
    (fun g ->
      let g = atom g in
      let w = Winfo.reqwidth g and h = Winfo.reqheight g in
      if !x > pad && !x + w + pad > w_top then (
        x := pad;
        y := !y + !lineh + pad;
        lineh := 0);
      place ~x:!x ~y:!y g;
      x := !x + w + gap;
      if h > !lineh then lineh := h)
    [ ".top.bench"; ".top.params"; ".top.hf"; ".top.perf"; ".top.btn" ];
  let total = !y + !lineh + pad in
  if Winfo.height top_bar <> total then
    tk_ [ ".top"; "configure"; "-height"; string_of_int total ]

let () =
  tcl_proc "flow_top" flow_top;
  bind ~events:[ `Configure ] ~action:(fun _ -> flow_top ()) top_bar

let status = var "idle"

let () =
  let l =
    ttk "label" ".status"
      [ "-textvariable"; vname status; "-anchor"; "w"; "-padding"; "6 0" ]
  in
  pack ~side:`Top ~fill:`X [ l ]

let () =
  let pw = ttk "panedwindow" ".pw" [ "-orient"; "vertical" ] in
  pack ~side:`Top ~fill:`Both ~expand:true [ pw ];
  ignore (ttk "frame" ".main" []);
  tk_ [ ".pw"; "add"; ".main"; "-weight"; "4" ];
  let cfg = ttk "labelframe" ".main.cfg" [ "-text"; "Configurations"; "-padding"; "4" ] in
  pack ~side:`Left ~fill:`Y ~padx:4 ~pady:4 [ cfg ]

let canvas =
  Canvas.create ~name:"c" ~background:`White ~highlightthickness:0
    ~yscrollcommand:(fun ~first ~last ->
      tk_ [ ".main.s"; "set"; string_of_float first; string_of_float last ])
    (atom ".main")

let () =
  let s = ttk "scrollbar" ".main.s" [ "-command"; ".main.c yview" ] in
  pack ~side:`Right ~fill:`Y ~pady:4 [ s ];
  pack ~side:`Left ~fill:`Both ~expand:true ~padx:4 ~pady:4 [ canvas ];
  (* labltk has no <MouseWheel> event *)
  tcl_ "bind .main.c <MouseWheel> {.main.c yview scroll [expr {%D > 0 ? -1 : 1}] units}";
  bind ~events:[ `ButtonPressDetail 4 ]
    ~action:(fun _ -> Canvas.yview canvas ~scroll:(`Unit (-1)))
    canvas;
  bind ~events:[ `ButtonPressDetail 5 ]
    ~action:(fun _ -> Canvas.yview canvas ~scroll:(`Unit 1))
    canvas

(* detail pane: perf stat counters of the selected config *)
let () =
  let detail = ttk "frame" ".detail" [] in
  tk_ [ ".pw"; "add"; ".detail"; "-weight"; "2" ];
  let hdr =
    ttk "label" ".detail.hdr"
      [ "-text"; "Click a result to see its perf stat counters."; "-anchor"; "w";
        "-padding"; "6 4"; "-wraplength"; "800" ]
  in
  bind ~events:[ `Configure ] ~fields:[ `Width ]
    ~action:(fun ev ->
      tk_ [ ".detail.hdr"; "configure"; "-wraplength"; string_of_int (ev.ev_Width - 12) ])
    detail;
  let tv =
    ttk "treeview" ".detail.tv"
      [ "-show"; "headings"; "-selectmode"; "none";
        "-columns"; "event value unit var metric counted";
        "-yscrollcommand"; ".detail.s set"; "-xscrollcommand"; ".detail.xs set" ]
  in
  let s = ttk "scrollbar" ".detail.s" [ "-command"; ".detail.tv yview" ] in
  let xs =
    ttk "scrollbar" ".detail.xs" [ "-orient"; "horizontal"; "-command"; ".detail.tv xview" ]
  in
  List.iter
    (fun (c, title, w, anchor) ->
      tk_ [ ".detail.tv"; "heading"; c; "-text"; title; "-anchor"; anchor ];
      tk_
        [ ".detail.tv"; "column"; c; "-width"; w; "-minwidth"; w; "-anchor"; anchor;
          "-stretch"; (if c = "metric" then "1" else "0") ])
    [ ("event", "counter", "180", "w"); ("value", "value", "110", "e");
      ("unit", "unit", "45", "w"); ("var", "± (runs)", "70", "e");
      ("metric", "derived metric", "240", "w"); ("counted", "counted %", "80", "e") ];
  tk_ [ ".detail.tv"; "tag"; "configure"; "missing"; "-foreground"; col_dim ];
  pack ~side:`Top ~fill:`X [ hdr ];
  pack ~side:`Bottom ~fill:`X [ xs ];
  pack ~side:`Right ~fill:`Y [ s ];
  pack ~side:`Left ~fill:`Both ~expand:true [ tv ]

let () =
  ignore (ttk "frame" ".logf" []);
  tk_ [ ".pw"; "add"; ".logf"; "-weight"; "1" ]

let logt =
  Text.create ~name:"t" ~height:10 ~wrap:`None ~font:"TkFixedFont"
    ~yscrollcommand:(fun ~first ~last ->
      tk_ [ ".logf.s"; "set"; string_of_float first; string_of_float last ])
    (atom ".logf")

let () =
  let s = ttk "scrollbar" ".logf.s" [ "-command"; ".logf.t yview" ] in
  pack ~side:`Right ~fill:`Y [ s ];
  pack ~side:`Left ~fill:`Both ~expand:true [ logt ]

let log msg =
  Text.insert ~index:(`End, []) ~text:msg logt;
  Text.see logt ~index:(`End, [])

let build_params () =
  List.iter destroy (Winfo.children (atom ".top.params"));
  let fields =
    if get bench = "pi" then [ (form_pi_n, "steps (-n)", "14") ]
    else
      [ (form_fib_n, "fib (-n)", "5"); (form_fib_niter, "-niter", "5");
        (form_fib_cutoff, "-cutoff", "5") ]
  in
  List.iteri
    (fun i (v, lbl, width) ->
      let l = ttk "label" (spf ".top.params.l%d" i) [ "-text"; lbl ] in
      let e =
        ttk "entry" (spf ".top.params.e%d" i) [ "-width"; width; "-textvariable"; vname v ]
      in
      pack ~side:`Left ~padx:3 [ l; e ])
    fields;
  tk_ [ "after"; "idle"; "flow_top" ]

let build_configs () =
  List.iter destroy (Winfo.children (atom ".main.cfg"));
  let cfgs = configs (get bench) in
  enabled := Array.map (fun _ -> var "1") cfgs;
  threads := Array.map (fun c -> var c.def) cfgs;
  Array.iteri
    (fun i c ->
      let cb = spf ".main.cfg.cb%d" i in
      ignore
        (ttk "checkbutton" cb [ "-text"; c.label; "-variable"; vname !enabled.(i) ]);
      tk_ [ "grid"; cb; "-row"; string_of_int i; "-column"; "0"; "-sticky"; "w" ];
      if c.flag <> "" then (
        let fl = spf ".main.cfg.fl%d" i and sp = spf ".main.cfg.sp%d" i in
        ignore (ttk "label" fl [ "-text"; c.flag; "-foreground"; col_dim ]);
        ignore
          (ttk "spinbox" sp
             [ "-from"; "1"; "-to"; "256"; "-width"; "4"; "-textvariable";
               vname !threads.(i) ]);
        tk_
          [ "grid"; fl; "-row"; string_of_int i; "-column"; "1"; "-sticky"; "e";
            "-padx"; "8 2" ];
        tk_ [ "grid"; sp; "-row"; string_of_int i; "-column"; "2"; "-sticky"; "w" ]))
    cfgs

let thread_count i = String.trim (get !threads.(i))

(* full command line for config [i] *)
let command_for i =
  let b = get bench in
  let c = (configs b).(i) in
  let buf = Buffer.create 128 in
  Buffer.add_string buf ("./_build/default/" ^ exe b);
  if b = "pi" then Buffer.add_string buf (" -n " ^ get form_pi_n)
  else
    Buffer.add_string buf
      (spf " -n %s -cutoff %s -niter %s" (get form_fib_n) (get form_fib_cutoff)
         (get form_fib_niter));
  if c.flag <> "" && thread_count i <> "" then
    Buffer.add_string buf (spf " %s %s" c.flag (thread_count i));
  Buffer.add_string buf (" " ^ c.args);
  Buffer.contents buf

let row_label i =
  let c = (configs (get bench)).(i) in
  if c.flag <> "" && thread_count i <> "" then
    spf "%s %s=%s" c.label
      (String.sub c.flag 1 (String.length c.flag - 1))
      (thread_count i)
  else c.label

(* ---- drawing ------------------------------------------------------------- *)

let fmt_time s =
  if s >= 1.0 then spf "%.3f s" s
  else if s >= 1e-3 then spf "%.1f ms" (s *. 1e3)
  else spf "%.1f µs" (s *. 1e6)

(* "nice" tick step for range [0, max] with ~n ticks *)
let nice_step max n =
  let raw = max /. float n in
  let mag = Float.pow 10. (Float.floor (log10 raw)) in
  match List.find_opt (fun m -> m *. mag >= raw) [ 1.; 2.; 2.5; 5.; 10. ] with
  | Some m -> m *. mag
  | None -> 10. *. mag

let font = "TkDefaultFont"

let () =
  ignore
    (Font.create ~name:"BarBold" ~family:(Font.actual_family font)
       ~size:(Font.actual_size font) ~weight:`Bold ())

let measure f s = Font.measure f s
let px f = int_of_float (Float.round f)

let text ?(font = font) ?(fill = col_text) ~anchor x y s =
  ignore (Canvas.create_text ~x ~y ~text:s ~anchor ~fill:(`Color fill) ~font canvas)

let line ?(width = 1) ?tags ~fill xys =
  ignore (Canvas.create_line ~xys ~fill:(`Color fill) ~width ?tags canvas)

let rect ?tags ~fill x1 y1 x2 y2 =
  ignore
    (Canvas.create_rectangle ~x1 ~y1 ~x2 ~y2 ~fill:(`Color fill) ~outline:(`Color "")
       ?tags canvas)

(* Each row: a text line (label, cpu info, mean ± σ, ratio), then the wall bar
   and the thinner cpu bar underneath, spanning the whole canvas width. *)
let redraw () =
  Canvas.delete canvas [ `Tag "all" ];
  let w_c = Winfo.width canvas in
  if w_c >= 50 then begin
    let cfgs = configs (get bench) in
    let rows =
      List.filter
        (fun i -> Hashtbl.mem results i || get !enabled.(i) = "1")
        (List.init (Array.length cfgs) Fun.id)
    in
    let scale_max = ref 0.0 and fastest = ref None in
    List.iter
      (fun i ->
        match Hashtbl.find_opt results i with
        | Some (Done r) ->
          List.iter
            (fun v -> if v > !scale_max then scale_max := v)
            [ r.max; r.mean +. r.stddev; r.user +. r.system ];
          (match !fastest with
          | Some f when f <= r.mean -> ()
          | _ -> fastest := Some r.mean)
        | _ -> ())
      rows;
    (* a little headroom so the longest bar doesn't touch the edge *)
    let scale_max = !scale_max *. 1.02 in
    let fastest = Stdlib.Option.value !fastest ~default:0. in

    let fh = Font.metrics font `Linespace in
    let margin = 10 in
    let x0 = margin in
    let x1 = w_c - margin in
    let plotw = x1 - x0 in

    (* legend, wrapping like the top bar *)
    let lx = ref x0 and ly = ref (margin + (fh / 2)) in
    List.iter
      (fun (kind, name) ->
        let w = 32 + measure font name in
        if !lx > x0 && !lx + w > x1 then (
          lx := x0;
          ly := !ly + fh + 4);
        let x = !lx and y = !ly in
        (match kind with
        | `Range -> line ~fill:col_range [ (x, y); (x + 14, y) ]
        | `Sigma ->
          line ~fill:col_sigma ~width:2 [ (x, y); (x + 14, y) ];
          line ~fill:col_sigma ~width:2 [ (x, y - 5); (x, y + 5) ];
          line ~fill:col_sigma ~width:2 [ (x + 14, y - 5); (x + 14, y + 5) ]
        | `Box c -> rect ~fill:c x (y - 6) (x + 14) (y + 6));
        text ~anchor:`W (x + 18) y name;
        lx := x + w + 12)
      [ (`Box col_bar, "wall mean"); (`Box col_user, "user CPU");
        (`Box col_sys, "sys CPU"); (`Range, "min–max"); (`Sigma, "±σ") ];

    let barh = 14 and cpuh = 6 in
    let rowh = fh + 2 + barh + 2 + cpuh + 10 in
    let top = !ly + fh in

    rowgeom := [];
    let y = ref top in
    List.iter
      (fun i ->
        let y0 = !y in
        let rh = ref rowh in
        let ty = y0 + (fh / 2) in
        text ~font:"BarBold" ~anchor:`W x0 ty (row_label i);
        let labelw = measure "BarBold" (row_label i) in
        (match Hashtbl.find_opt results i with
        | None ->
          let msg =
            if !running && Some i = List.nth_opt !queue 0 then "running…"
            else if !running then "pending"
            else ""
          in
          text ~fill:col_dim ~anchor:`E x1 ty msg
        | Some Failed -> text ~fill:col_sys ~anchor:`E x1 ty "failed (see log)"
        | Some (Done r) ->
          (* right side: "mean ± σ   ratio", ratio in bold when fastest *)
          let ratio, rfont =
            if r.mean = fastest then ("fastest", "BarBold")
            else (spf "%.2f× slower" (r.mean /. fastest), font)
          in
          let stats = spf "%s ± %s" (fmt_time r.mean) (fmt_time r.stddev) in
          let statsw = measure font stats + 12 + measure rfont ratio in
          (* too narrow for label and stats on one line: stats go on a second line *)
          let sty =
            if labelw + 12 + statsw > plotw then (
              rh := !rh + fh;
              ty + fh)
            else ty
          in
          text ~font:rfont ~anchor:`E x1 sty ratio;
          let sx1 = x1 - measure rfont ratio - 12 in
          text ~anchor:`E sx1 sty stats;
          (* middle: cpu info, only if it fits *)
          let cpu =
            spf "cpu %s (%.1f× wall)"
              (fmt_time (r.user +. r.system))
              ((r.user +. r.system) /. r.mean)
          in
          let cx = x0 + labelw + 12 in
          if cx + measure font cpu + 12 < sx1 - measure font stats then
            text ~fill:col_dim ~anchor:`W cx ty cpu;

          let sx = float plotw /. scale_max in
          let at t = px (float x0 +. (t *. sx)) in
          let by0 = sty + (fh / 2) + 2 in
          let by1 = by0 + barh in
          let bc = (by0 + by1) / 2 in
          rect ~fill:(if r.mean = fastest then col_fast else col_bar) x0 by0 (at r.mean) by1;
          line ~fill:col_range [ (at r.min, bc); (at r.max, bc) ];
          let sl = at (Float.max 0. (r.mean -. r.stddev)) in
          let sr = at (r.mean +. r.stddev) in
          line ~fill:col_sigma ~width:2 [ (sl, bc); (sr, bc) ];
          line ~fill:col_sigma ~width:2 [ (sl, by0 + 3); (sl, by1 - 3) ];
          line ~fill:col_sigma ~width:2 [ (sr, by0 + 3); (sr, by1 - 3) ];
          let cy0 = by1 + 2 in
          let cy1 = cy0 + cpuh in
          let ux = at r.user in
          rect ~fill:col_user x0 cy0 ux cy1;
          rect ~fill:col_sys ux cy0 (px (float ux +. (r.system *. sx))) cy1);
        rowgeom := !rowgeom @ [ (i, y0 - 4, y0 + !rh - 6) ];
        if Some i = !selected then
          rect ~tags:[ "sel" ] ~fill:col_sel 0 (y0 - 4) w_c (y0 + !rh - 6);
        y := y0 + !rh)
      rows;
    let bottom = !y in

    (* grid + axis, roughly one tick per 90px *)
    if scale_max > 0. then begin
      let step = nice_step scale_max (Stdlib.max 2 (plotw / 90)) in
      let t = ref 0.0 in
      while !t <= scale_max do
        let xf = float x0 +. (!t /. scale_max *. float plotw) in
        let x = px xf in
        line ~tags:[ "grid" ] ~fill:col_grid [ (x, top); (x, bottom) ];
        let lbl = if !t = 0. then "0" else fmt_time !t in
        let half = measure font lbl / 2 in
        let anchor =
          if !t = 0. then `Nw
          else if xf +. float half > float (x1 + margin) then `Ne
          else `N
        in
        text ~fill:col_dim ~anchor x (bottom + 2) lbl;
        t := !t +. step
      done
    end;

    Canvas.lower canvas (`Tag "grid");
    Canvas.lower canvas (`Tag "sel");
    let _, _, _, bb_y1 = Canvas.bbox canvas [ `Tag "all" ] in
    Canvas.configure canvas ~scrollregion:(0, 0, w_c, bb_y1 + margin)
  end

(* ---- detail pane --------------------------------------------------------- *)

let row_at wy =
  let y = Canvas.canvasy canvas ~y:wy in
  List.find_map
    (fun (i, y0, y1) -> if y >= float y0 && y < float y1 then Some i else None)
    !rowgeom

(* 12345678 -> 12,345,678 *)
let group_digits v =
  let is_digit c = c >= '0' && c <= '9' in
  let int, frac =
    match String.index_opt v '.' with
    | Some k -> (String.sub v 0 k, String.sub v k (String.length v - k))
    | None -> (v, "")
  in
  let frac_ok =
    frac = ""
    || String.length frac > 1
       && String.for_all is_digit (String.sub frac 1 (String.length frac - 1))
  in
  if int = "" || (not (String.for_all is_digit int)) || not frac_ok then v
  else begin
    let n = String.length int in
    let b = Buffer.create (n + (n / 3)) in
    String.iteri
      (fun k c ->
        if k > 0 && (n - k) mod 3 = 0 then Buffer.add_char b ',';
        Buffer.add_char b c)
      int;
    Buffer.contents b ^ frac
  end

let words s = String.split_on_char ' ' s |> List.filter (( <> ) "")

let show_detail i =
  let tv = ".detail.tv" in
  tk_ [ tv; "delete"; tk [ tv; "children"; "" ] ];
  let set_hdr s = tk_ [ ".detail.hdr"; "configure"; "-text"; s ] in
  match i with
  | None -> set_hdr "Click a result to see its perf stat counters."
  | Some i ->
    let hdr = Buffer.create 256 in
    Buffer.add_string hdr (spf "%s:  %s" (row_label i) (command_for i));
    (match Hashtbl.find_opt results i with
    | Some (Done r) ->
      Buffer.add_string hdr
        (spf "\nwall %s ± %s,  user %s, sys %s" (fmt_time r.mean) (fmt_time r.stddev)
           (fmt_time r.user) (fmt_time r.system))
    | _ -> ());
    (match Hashtbl.find_opt perf i with
    | None ->
      Buffer.add_string hdr
        (if get form_perf = "1" then "\n(no perf data yet)" else "\n(perf stat disabled)");
      set_hdr (Buffer.contents hdr)
    | Some p ->
      Buffer.add_string hdr
        (spf "\nperf stat, mean of %d run%s," p.runs (if p.runs > 1 then "s" else ""));
      Buffer.add_string hdr " user space only (:u)";
      set_hdr (Buffer.contents hdr);
      List.iter
        (fun cnt ->
          let metric =
            if cnt.mval <> "" && cnt.munit <> "" then begin
              (* "GHz  cycles_frequency" -> "cycles frequency = 1.3 GHz"; perf's
                 unit is only kept when it adds something ("instructions" doesn't) *)
              let parts = words cnt.munit in
              let rev = List.rev parts in
              let name = String.map (fun c -> if c = '_' then ' ' else c) (List.hd rev) in
              let human = String.concat " " (List.rev (List.tl rev)) in
              let m = spf "%s = %s" name cnt.mval in
              if human = "%" || human = "GHz" || String.ends_with ~suffix:"/sec" human
              then m ^ " " ^ human
              else m
            end
            else ""
          in
          let tags = if String.starts_with ~prefix:"<" cnt.value then "missing" else "" in
          tk_
            [ tv; "insert"; ""; "end"; "-values";
              tcl_list
                [ cnt.event; group_digits cnt.value; cnt.unit_; cnt.var; metric;
                  (if cnt.pct = "" then "" else cnt.pct ^ " %") ];
              "-tags"; tags ])
        p.counters)

let select_row i =
  selected := i;
  redraw ();
  show_detail i

(* ---- tooltips for the detail pane ---------------------------------------- *)

let perf_doc =
  [
    ( "task-clock",
      "CPU time summed over all threads (ms). Derived: CPUs utilized = task-clock / \
       wall time, i.e. the effective parallelism." );
    ( "context-switches",
      "Times the kernel switched a thread off its CPU (blocking, sleeping, \
       preemption). High when workers park and wake up a lot." );
    ( "cpu-migrations",
      "Times a thread was moved to another core. Each one costs cache locality." );
    ( "page-faults",
      "First touches of memory pages (or pages swapped in). Mostly heap growth: minor \
       heap, major heap, domain stacks." );
    ( "cycles",
      "Core clock cycles spent running the program. Derived: GHz = effective clock \
       rate while running." );
    ( "cpu-cycles",
      "Core clock cycles spent running the program. Derived: GHz = effective clock \
       rate while running." );
    ( "instructions",
      "Instructions retired. Derived: IPC (instructions per cycle). Around 2-4 is \
       compute-bound and healthy; below 1 usually means stalls on memory or branches."
    );
    ("branches", "Branch instructions executed. Derived: rate in millions per second.");
    ( "branch-misses",
      "Mispredicted branches, each ~15-20 cycles lost. Derived: % of all branches." );
    ( "stalled-cycles-frontend",
      "Cycles where fetch/decode delivered nothing (i-cache misses, branch \
       mispredictions). Derived: % of cycles idle." );
    ( "stalled-cycles-backend",
      "Cycles waiting on execution units, usually on memory loads. Derived: % of \
       cycles idle." );
    ("L1-dcache-loads", "Loads served by the L1 data cache.");
    ( "L1-dcache-load-misses",
      "L1 data loads that had to go to L2 or further. Derived: miss rate. Contention \
       on shared data (atomics, queues) shows up here." );
    ("LLC-loads", "Loads that reached the last-level (L3) cache.");
    ("LLC-load-misses", "Last-level cache misses: the load went to RAM (~100 ns each).");
    ("cache-references", "Accesses to the last-level cache.");
    ("cache-misses", "Last-level cache misses (went to RAM).");
  ]

let column_doc =
  [
    ( "event",
      "perf event name. ':u' means user-space only (kernel.perf_event_paranoid=2 \
       hides kernel activity)." );
    ("value", "Counter value, averaged over the perf -r runs.");
    ("unit", "Unit of the value (empty = plain count).");
    ("var", "Relative standard deviation of the value across the -r runs.");
    ("metric", "Metric perf derives from this counter (IPC, miss rate, GHz, ...).");
    ( "counted",
      "Share of the run this counter was actually counting. Below 100% the hardware \
       counters were multiplexed and the value is scaled up (estimated)." );
  ]

let tip = Toplevel.create ~name:"tip" ~background:(`Color "#333333") ~borderwidth:0 top

let tip_label =
  Label.create ~name:"l" ~background:(`Color "#ffffe0") ~foreground:(`Color "#222222")
    ~justify:`Left ~wraplength:380 ~padx:6 ~pady:4 ~borderwidth:1 ~relief:`Solid
    tip

let () =
  Wm.overrideredirect_set tip true;
  Wm.withdraw tip;
  pack [ tip_label ]

let tip_key = ref ""

let tip_hide () =
  tip_key := "";
  Wm.withdraw tip

(* strip a trailing ":u"-style modifier from an event name *)
let event_base ev =
  match String.rindex_opt ev ':' with
  | Some k
    when k + 1 < String.length ev
         && String.for_all
              (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
              (String.sub ev (k + 1) (String.length ev - k - 1)) ->
    String.sub ev 0 k
  | _ -> ev

let tip_motion x y rx ry =
  let tv = ".detail.tv" in
  let x = string_of_int x and y = string_of_int y in
  let key, text =
    match tk [ tv; "identify"; "region"; x; y ] with
    | "heading" ->
      let col = tk [ tv; "column"; tk [ tv; "identify"; "column"; x; y ]; "-id" ] in
      ("h:" ^ col, Stdlib.Option.value (List.assoc_opt col column_doc) ~default:"")
    | "cell" ->
      let item = tk [ tv; "identify"; "item"; x; y ] in
      let ev =
        match Protocol.splitlist (tk [ tv; "item"; item; "-values" ]) with
        | ev :: _ -> ev
        | [] -> ""
      in
      ( "r:" ^ ev,
        match List.assoc_opt (event_base ev) perf_doc with
        | Some doc -> ev ^ "\n" ^ doc
        | None -> "" )
    | _ -> ("", "")
  in
  if text = "" then tip_hide ()
  else begin
    if key <> !tip_key then begin
      tip_key := key;
      Label.configure tip_label ~text;
      Wm.deiconify tip;
      raise_window tip
    end;
    Wm.geometry_set tip (spf "+%d+%d" (rx + 14) (ry + 16))
  end

let () =
  let tv = atom ".detail.tv" in
  bind ~events:[ `Motion ]
    ~fields:[ `MouseX; `MouseY; `RootX; `RootY ]
    ~action:(fun ev -> tip_motion ev.ev_MouseX ev.ev_MouseY ev.ev_RootX ev.ev_RootY)
    tv;
  bind ~events:[ `Leave ] ~action:(fun _ -> tip_hide ()) tv

(* ---- runner -------------------------------------------------------------- *)

let set_running r =
  running := r;
  let st = if r then "disabled" else "normal" in
  List.iter
    (fun w -> tk_ [ w; "configure"; "-state"; st ])
    [ ".top.btn.run"; ".top.bench.pi"; ".top.bench.fib" ];
  tk_ [ ".top.btn.stop"; "configure"; "-state"; (if r then "normal" else "disabled") ]

type exit_code = Exited of int | Killed

let string_of_code = function
  | Exited n -> string_of_int n
  | Killed -> "killed"

(* length of the longest prefix of [s] that doesn't end in the middle of a
   UTF-8 sequence, so a multi-byte character split across two reads is not
   handed to Tk in two halves *)
let utf8_complete_prefix s =
  let n = String.length s in
  let rec back k =
    if k < 0 || n - k > 4 then n
    else
      let c = Char.code s.[k] in
      if c land 0xC0 = 0x80 then back (k - 1)
      else
        let len =
          if c land 0x80 = 0 then 1
          else if c land 0xE0 = 0xC0 then 2
          else if c land 0xF0 = 0xE0 then 3
          else 4
        in
        if k + len <= n then n else k
  in
  back (n - 1)

(* like Tcl's -translation auto *)
let normalize_newlines s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let k = ref 0 in
  while !k < n do
    (match s.[!k] with
    | '\r' ->
      Buffer.add_char b '\n';
      if !k + 1 < n && s.[!k + 1] = '\n' then incr k
    | c -> Buffer.add_char b c);
    incr k
  done;
  Buffer.contents b

let buf = Bytes.create 65536

(* run [cmd] asynchronously, streaming output to the log; call [k] with the
   exit code *)
let spawn (cmd : string list) (k : exit_code -> unit) =
  log ("$ " ^ String.concat " " cmd ^ "\n");
  match
    let rd, wr = Unix.pipe ~cloexec:true () in
    match Unix.create_process (List.hd cmd) (Array.of_list cmd) Unix.stdin wr wr with
    | pid ->
      Unix.close wr;
      (rd, pid)
    | exception e ->
      Unix.close rd;
      Unix.close wr;
      raise e
  with
  | exception Unix.Unix_error (e, _, _) ->
    log (spf "error: couldn't execute \"%s\": %s\n" (List.hd cmd) (Unix.error_message e));
    k (Exited 1)
  | rd, pid ->
    chan := Some (rd, pid);
    let pending = ref "" in
    Fileevent.add_fileinput ~fd:rd ~callback:(fun () ->
        let n = try Unix.read rd buf 0 (Bytes.length buf) with Unix.Unix_error _ -> 0 in
        let data = !pending ^ Bytes.sub_string buf 0 n in
        let cut = if n = 0 then String.length data else utf8_complete_prefix data in
        pending := String.sub data cut (String.length data - cut);
        if cut > 0 then log (normalize_newlines (String.sub data 0 cut));
        if n = 0 then begin
          Fileevent.remove_fileinput ~fd:rd;
          Unix.close rd;
          let code =
            match snd (Unix.waitpid [] pid) with
            | Unix.WEXITED c -> Exited c
            | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Killed
          in
          chan := None;
          k code
        end)

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* hyperfine CSV: command,mean,stddev,median,user,system,min,max (seconds).
   Parse numbers from the end so the command field can't confuse us. *)
let parse_csv path =
  let lines = String.split_on_char '\n' (String.trim (read_file path)) in
  let row = Array.of_list (String.split_on_char ',' (List.nth lines 1)) in
  let n = Array.length row in
  let f k =
    let v = row.(n - 7 + k) in
    match float_of_string_opt v with
    | Some x -> x
    | None -> failwith (spf "bad value '%s'" v)
  in
  (* f 2 is the median, which we don't show *)
  ignore (f 2);
  { mean = f 0; stddev = f 1; user = f 3; system = f 4; min = f 5; max = f 6 }

(* perf stat -x, lines: value,unit,event,[variance,]runtime,pct,metric,metric-unit
   where metric-unit is "<human unit>  <metric_name>". *)
let parse_perf path =
  let counters =
    String.split_on_char '\n' (read_file path)
    |> List.filter_map (fun line ->
           if String.trim line = "" || String.starts_with ~prefix:"#" line then None
           else
             match String.split_on_char ',' line with
             | value :: unit_ :: event :: rest when List.length rest >= 2 ->
               let var, rest =
                 match rest with
                 | v :: rest when String.ends_with ~suffix:"%" v -> (v, rest)
                 | _ -> ("", rest)
               in
               let nth k = Stdlib.Option.value (List.nth_opt rest k) ~default:"" in
               Some
                 { event; value; unit_; var; pct = nth 1; mval = nth 2;
                   munit = String.trim (nth 3) }
             | _ -> None)
  in
  if counters = [] then failwith ("no counters in " ^ path);
  counters

let rec run_next () =
  if !running then
    match !queue with
    | [] ->
      Textvariable.set status "done";
      set_running false
    | i :: _ ->
      let cmd =
        [ "hyperfine"; "--style"; "basic"; "--warmup"; get form_warmup; "--export-csv";
          tmpcsv ]
        @ (let r = String.trim (get form_runs) in
           if r <> "" then [ "--runs"; r ] else [])
        @ [ command_for i ]
      in
      Textvariable.set status
        (spf "running %s…  (%d left)" (row_label i) (List.length !queue));
      (try Sys.remove tmpcsv with Sys_error _ -> ());
      spawn cmd (after_bench i)

and after_bench i code =
  if !running then begin
    let ok =
      match code with
      | Exited 0 -> (
        match parse_csv tmpcsv with
        | r ->
          Hashtbl.replace results i (Done r);
          true
        | exception e ->
          log (spf "could not parse hyperfine CSV: %s\n" (Printexc.to_string e));
          false)
      | code ->
        log (spf "hyperfine exited with %s\n" (string_of_code code));
        Hashtbl.replace results i Failed;
        false
    in
    redraw ();
    if ok && get form_perf = "1" then run_perf i else next_config ()
  end

and next_config () =
  queue := (match !queue with [] -> [] | _ :: tl -> tl);
  run_next ()

(* run the config under `perf stat` (separately from hyperfine, so the
   timings above are not affected by perf's overhead) *)
and run_perf i =
  let r =
    match int_of_string_opt (String.trim (get form_perf_r)) with
    | Some r when r >= 1 -> r
    | _ -> 1
  in
  let cmd =
    [ "perf"; "stat"; "-d"; "-r"; string_of_int r; "-x"; ","; "-o"; tmpperf; "--" ]
    @ words (command_for i)
  in
  Textvariable.set status
    (spf "perf stat %s…  (%d left)" (row_label i) (List.length !queue));
  (try Sys.remove tmpperf with Sys_error _ -> ());
  spawn cmd (after_perf i r)

and after_perf i r code =
  if !running then begin
    (match code with
    | Exited 0 -> (
      match parse_perf tmpperf with
      | counters -> Hashtbl.replace perf i { runs = r; counters }
      | exception e -> log (spf "could not parse perf output: %s\n" (Printexc.to_string e)))
    | code -> log (spf "perf stat exited with %s\n" (string_of_code code)));
    if !selected = Some i then show_detail (Some i);
    next_config ()
  end

let after_build code =
  if !running then
    match code with
    | Exited 0 -> run_next ()
    | code ->
      Textvariable.set status (spf "build failed (%s)" (string_of_code code));
      set_running false

let start_run () =
  Hashtbl.reset results;
  Hashtbl.reset perf;
  queue := [];
  Array.iteri (fun i v -> if get v = "1" then queue := !queue @ [ i ]) !enabled;
  if !queue = [] then Textvariable.set status "nothing selected"
  else begin
    redraw ();
    set_running true;
    Text.delete logt ~start:(`Linechar (1, 0), []) ~stop:(`End, []);
    let e = exe (get bench) in
    Textvariable.set status (spf "building %s…" e);
    spawn [ "dune"; "build"; "--profile=release"; e ] after_build
  end

let rec kill_tree pid =
  (try
     let ic = Unix.open_process_args_in "pgrep" [| "pgrep"; "-P"; string_of_int pid |] in
     let out = In_channel.input_all ic in
     ignore (Unix.close_process_in ic);
     List.iter
       (fun s -> Stdlib.Option.iter kill_tree (int_of_string_opt s))
       (String.split_on_char '\n' out)
   with _ -> ());
  try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ()

let stop_run () =
  running := false;
  (match !chan with
  | Some (fd, pid) ->
    kill_tree pid;
    Fileevent.remove_fileinput ~fd;
    (try Unix.close fd with Unix.Unix_error _ -> ());
    (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
    chan := None
  | None -> ());
  log "\n-- stopped --\n";
  Textvariable.set status "stopped";
  set_running false

let on_bench_change () =
  if not !running then begin
    Hashtbl.reset results;
    Hashtbl.reset perf;
    build_params ();
    build_configs ();
    select_row None
  end

let () =
  bind ~events:[ `Configure ] ~action:(fun _ -> redraw ()) canvas;
  bind ~events:[ `ButtonPressDetail 1 ] ~fields:[ `MouseY ]
    ~action:(fun ev -> select_row (row_at ev.ev_MouseY))
    canvas;
  bind ~events:[ `Motion ] ~fields:[ `MouseY ]
    ~action:(fun ev ->
      tk_
        [ ".main.c"; "configure"; "-cursor";
          (if row_at ev.ev_MouseY <> None then "hand2" else "") ])
    canvas;
  tcl_proc "on_bench_change" on_bench_change;
  tcl_proc "start_run" start_run;
  tcl_proc "stop_run" stop_run

(* run from the moonpool root, like the Makefile targets, wherever the
   executable lives (normally _build/default/benchs/gui/) *)
let () =
  let rec find dir =
    if Sys.file_exists (Filename.concat dir "benchs/pi.ml")
       && Sys.file_exists (Filename.concat dir "_build")
    then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else find parent
  in
  let exe_dir =
    Filename.dirname
      (if Filename.is_relative Sys.executable_name then
         Filename.concat (Sys.getcwd ()) Sys.executable_name
       else Sys.executable_name)
  in
  Stdlib.Option.iter Sys.chdir (find exe_dir)

let () =
  on_bench_change ();
  mainLoop ()
