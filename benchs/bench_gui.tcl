# Tk front-end for the pi / fib_rec benchmarks (see `make bench-pi`, `make bench-fib`).
# Runs hyperfine once per selected configuration and draws one bar per result.
#
# Usage: make bench-gui   (or: tclsh benchs/bench_gui.tcl)

if {[catch {package require Tk} err]} {
    puts stderr "Tk not available ($err). Install it with: sudo dnf install tk"
    exit 1
}

cd [file dirname [file dirname [file normalize [info script]]]]

# ---- configurations -------------------------------------------------------

# Each config: {label args thread_flag default_threads}
# thread_flag is "" when the config takes no thread count; an empty
# default_threads means the flag is omitted unless the user fills it in.
set configs(pi) {
    {"seq"                  "-mode=seq"             ""       ""}
    {"par1 pool"            "-mode par1 -kind=pool" "-j"     8}
    {"par1 fifo"            "-mode par1 -kind=fifo" "-j"     8}
    {"forkjoin pool"        "-mode forkjoin -kind=pool" "-j" 16}
    {"forkjoin pool"        "-mode forkjoin -kind=pool" "-j" 20}
}
set configs(fib) {
    {"seq"                  "-seq"                  ""       ""}
    {"domainslib"           "-dl"                   ""       ""}
    {"pool fj"              "-kind=pool -fj"        "-psize" ""}
    {"pool fj"              "-kind=pool -fj"        "-psize" 20}
    {"pool await"           "-kind=pool -await"     "-psize" 20}
    {"fifo"                 "-kind=fifo"            "-psize" 4}
    {"pool"                 "-kind=pool"            "-psize" 4}
    {"fifo"                 "-kind=fifo"            "-psize" 8}
    {"pool"                 "-kind=pool"            "-psize" 16}
}
set exe(pi)  benchs/pi.exe
set exe(fib) benchs/fib_rec.exe

# form values
set bench pi
set form(pi_n)      100_000_000
set form(fib_n)     40
set form(fib_niter) 2
set form(fib_cutoff) 20
set form(warmup)    1
set form(runs)      ""
set form(perf)      1
set form(perf_r)    3

set tmpdir [expr {[info exists ::env(TMPDIR)] ? $::env(TMPDIR) : "/tmp"}]
set tmpcsv  [file join $tmpdir "bench_gui.[pid].csv"]
set tmpperf [file join $tmpdir "bench_gui.[pid].perf"]

# results(i) = dict with mean stddev user system min max
array set results {}
# perf(i) = list of dicts, one per `perf stat` counter (see parse_perf)
array set perf {}
# config whose perf data is shown in the detail pane
set selected ""
# {i y0 y1} per drawn row, in canvas coordinates, for click handling
set rowgeom {}
set running 0
set chan ""
set queue {}

# ---- colors ---------------------------------------------------------------

set col(bar)    "#4a78c2"
set col(fast)   "#2e9d5b"
set col(user)   "#e0a030"
set col(sys)    "#c0504d"
set col(range)  "#222222"
set col(sigma)  "#111111"
set col(grid)   "#dddddd"
set col(text)   "#222222"
set col(dim)    "#888888"
set col(sel)    "#e6eefa"

# ---- UI -------------------------------------------------------------------

wm title . "moonpool benchmarks"
wm geometry . 1100x750

# The top bar is a flow layout: groups are placed left to right and wrap
# onto a new line when the window is too narrow (see flow_top).
ttk::frame .top
pack .top -side top -fill x

ttk::frame .top.bench
ttk::label .top.bench.l -text "Benchmark:"
ttk::radiobutton .top.bench.pi  -text pi  -variable bench -value pi  -command on_bench_change
ttk::radiobutton .top.bench.fib -text fib -variable bench -value fib -command on_bench_change
pack .top.bench.l .top.bench.pi .top.bench.fib -side left -padx 3

ttk::frame .top.params

ttk::frame .top.hf
ttk::label .top.hf.wl -text "warmup"
ttk::spinbox .top.hf.w -from 0 -to 100 -width 4 -textvariable form(warmup)
ttk::label .top.hf.rl -text "runs (empty=auto)"
ttk::spinbox .top.hf.r -from 2 -to 1000 -width 5 -textvariable form(runs)
pack .top.hf.wl .top.hf.w .top.hf.rl .top.hf.r -side left -padx 3

ttk::frame .top.perf
ttk::checkbutton .top.perf.cb -text "perf stat" -variable form(perf)
ttk::label .top.perf.rl -text "-r"
ttk::spinbox .top.perf.r -from 1 -to 100 -width 4 -textvariable form(perf_r)
pack .top.perf.cb .top.perf.rl .top.perf.r -side left -padx 3

ttk::frame .top.btn
ttk::button .top.btn.run  -text "Run"  -command start_run
ttk::button .top.btn.stop -text "Stop" -command stop_run -state disabled
pack .top.btn.run .top.btn.stop -side left -padx 3

proc flow_top {} {
    set pad 6
    set gap 18
    set W [winfo width .top]
    if {$W <= 1} { set W [winfo reqwidth .] }
    set x $pad
    set y $pad
    set lineh 0
    foreach g {.top.bench .top.params .top.hf .top.perf .top.btn} {
        set w [winfo reqwidth $g]
        set h [winfo reqheight $g]
        if {$x > $pad && $x + $w + $pad > $W} {
            set x $pad
            incr y [expr {$lineh + $pad}]
            set lineh 0
        }
        place $g -x $x -y $y
        incr x [expr {$w + $gap}]
        if {$h > $lineh} { set lineh $h }
    }
    set total [expr {$y + $lineh + $pad}]
    if {[winfo height .top] != $total} { .top configure -height $total }
}
bind .top <Configure> flow_top

set status "idle"
ttk::label .status -textvariable status -anchor w -padding {6 0}
pack .status -side top -fill x

ttk::panedwindow .pw -orient vertical
pack .pw -side top -fill both -expand 1

ttk::frame .main
.pw add .main -weight 4

ttk::labelframe .main.cfg -text "Configurations" -padding 4
pack .main.cfg -side left -fill y -padx 4 -pady 4

canvas .main.c -background white -highlightthickness 0 -yscrollcommand {.main.s set}
ttk::scrollbar .main.s -command {.main.c yview}
pack .main.s -side right -fill y -pady 4
pack .main.c -side left -fill both -expand 1 -padx 4 -pady 4
bind .main.c <MouseWheel> {.main.c yview scroll [expr {%D > 0 ? -1 : 1}] units}
bind .main.c <Button-4> {.main.c yview scroll -1 units}
bind .main.c <Button-5> {.main.c yview scroll 1 units}
bind .main.c <Configure> redraw
bind .main.c <Button-1> {select_row [row_at %y]}
bind .main.c <Motion> {
    %W configure -cursor [expr {[row_at %y] ne "" ? "hand2" : ""}]
}

# detail pane: perf stat counters of the selected config
ttk::frame .detail
.pw add .detail -weight 2
ttk::label .detail.hdr -text "Click a result to see its perf stat counters." \
    -anchor w -padding {6 4} -wraplength 800
bind .detail <Configure> {.detail.hdr configure -wraplength [expr {%w - 12}]}
ttk::treeview .detail.tv -show headings -selectmode none \
    -columns {event value unit var metric counted} -yscrollcommand {.detail.s set} \
    -xscrollcommand {.detail.xs set}
ttk::scrollbar .detail.s -command {.detail.tv yview}
ttk::scrollbar .detail.xs -orient horizontal -command {.detail.tv xview}
foreach {c title w anchor} {
    event "counter" 180 w  value "value" 110 e  unit "unit" 45 w
    var "± (runs)" 70 e  metric "derived metric" 240 w  counted "counted %" 80 e
} {
    .detail.tv heading $c -text $title -anchor $anchor
    .detail.tv column $c -width $w -minwidth $w -anchor $anchor \
        -stretch [expr {$c eq "metric"}]
}
.detail.tv tag configure missing -foreground $col(dim)
pack .detail.hdr -side top -fill x
pack .detail.xs -side bottom -fill x
pack .detail.s -side right -fill y
pack .detail.tv -side left -fill both -expand 1
bind .detail.tv <Motion> {tip_motion %W %x %y %X %Y}
bind .detail.tv <Leave> tip_hide

ttk::frame .logf
.pw add .logf -weight 1
text .logf.t -height 10 -wrap none -font TkFixedFont -yscrollcommand {.logf.s set}
ttk::scrollbar .logf.s -command {.logf.t yview}
pack .logf.s -side right -fill y
pack .logf.t -side left -fill both -expand 1

proc log {msg} {
    .logf.t insert end $msg
    .logf.t see end
}

proc build_params {} {
    global bench
    foreach w [winfo children .top.params] { destroy $w }
    if {$bench eq "pi"} {
        set fields {pi_n "steps (-n)" 14}
    } else {
        set fields {fib_n "fib (-n)" 5 fib_niter "-niter" 5 fib_cutoff "-cutoff" 5}
    }
    set i 0
    foreach {key lbl width} $fields {
        ttk::label .top.params.l$i -text $lbl
        ttk::entry .top.params.e$i -width $width -textvariable form($key)
        pack .top.params.l$i .top.params.e$i -side left -padx 3
        incr i
    }
    after idle flow_top
}

proc build_configs {} {
    global bench configs enabled threads
    foreach w [winfo children .main.cfg] { destroy $w }
    array unset enabled
    array unset threads
    set i 0
    foreach cfg $configs($bench) {
        lassign $cfg label args flag def
        set enabled($i) 1
        set threads($i) $def
        ttk::checkbutton .main.cfg.cb$i -text "$label" -variable enabled($i)
        grid .main.cfg.cb$i -row $i -column 0 -sticky w
        if {$flag ne ""} {
            ttk::label .main.cfg.fl$i -text $flag -foreground $::col(dim)
            ttk::spinbox .main.cfg.sp$i -from 1 -to 256 -width 4 -textvariable threads($i)
            grid .main.cfg.fl$i -row $i -column 1 -sticky e -padx {8 2}
            grid .main.cfg.sp$i -row $i -column 2 -sticky w
        }
        incr i
    }
}

proc on_bench_change {} {
    global running
    if {$running} return
    array unset ::results
    array unset ::perf
    build_params
    build_configs
    select_row ""
}

# full command line for config $i
proc command_for {i} {
    global bench configs exe form threads
    lassign [lindex $configs($bench) $i] label args flag def
    set cmd "./_build/default/$exe($bench)"
    if {$bench eq "pi"} {
        append cmd " -n $form(pi_n)"
    } else {
        append cmd " -n $form(fib_n) -cutoff $form(fib_cutoff) -niter $form(fib_niter)"
    }
    if {$flag ne "" && [string trim $threads($i)] ne ""} {
        append cmd " $flag [string trim $threads($i)]"
    }
    append cmd " $args"
    return $cmd
}

proc row_label {i} {
    global bench configs threads
    lassign [lindex $configs($bench) $i] label args flag def
    if {$flag ne "" && [string trim $threads($i)] ne ""} {
        append label " [string trimleft $flag -]=[string trim $threads($i)]"
    }
    return $label
}

# ---- runner ---------------------------------------------------------------

proc set_running {r} {
    global running
    set running $r
    set st [expr {$r ? "disabled" : "normal"}]
    foreach w [list .top.btn.run .top.bench.pi .top.bench.fib] { $w configure -state $st }
    .top.btn.stop configure -state [expr {$r ? "normal" : "disabled"}]
}

proc start_run {} {
    global bench exe enabled queue configs
    array unset ::results
    array unset ::perf
    set queue {}
    for {set i 0} {$i < [llength $configs($bench)]} {incr i} {
        if {$enabled($i)} { lappend queue $i }
    }
    if {![llength $queue]} { set ::status "nothing selected"; return }
    redraw
    set_running 1
    .logf.t delete 1.0 end
    set ::status "building $exe($bench)…"
    spawn [list dune build --profile=release $exe($bench)] [list after_build]
}

# run $cmd asynchronously, streaming output to the log; call $k with exit code
proc spawn {cmd k} {
    global chan
    log "\$ [join $cmd]\n"
    if {[catch {open |[concat $cmd [list 2>@1]] r} ch]} {
        log "error: $ch\n"
        {*}$k 1
        return
    }
    set chan $ch
    fconfigure $ch -blocking 0 -buffering none -translation auto
    fileevent $ch readable [list on_output $ch $k]
}

proc on_output {ch k} {
    set data [read $ch]
    if {$data ne ""} { log $data }
    if {![eof $ch]} return
    fileevent $ch readable {}
    fconfigure $ch -blocking 1
    set code 0
    if {[catch {close $ch} err opts]} {
        set ec [dict get $opts -errorcode]
        if {[lindex $ec 0] eq "CHILDSTATUS"} {
            set code [lindex $ec 2]
        } elseif {[lindex $ec 0] eq "CHILDKILLED"} {
            set code "killed"
        } elseif {[lindex $ec 0] ne "NONE"} {
            set code 1
            log "$err\n"
        }
    }
    set ::chan ""
    {*}$k $code
}

proc after_build {code} {
    if {!$::running} return
    if {$code ne 0} {
        set ::status "build failed ($code)"
        set_running 0
        return
    }
    run_next
}

proc run_next {} {
    global queue form tmpcsv
    if {!$::running} return
    if {![llength $queue]} {
        set ::status "done"
        set_running 0
        return
    }
    set i [lindex $queue 0]
    set cmd [list hyperfine --style basic --warmup $form(warmup) --export-csv $tmpcsv]
    if {[string trim $form(runs)] ne ""} { lappend cmd --runs [string trim $form(runs)] }
    lappend cmd [command_for $i]
    set ::status "running [row_label $i]…  ([llength $queue] left)"
    file delete -force $tmpcsv
    spawn $cmd [list after_bench $i]
}

proc after_bench {i code} {
    global tmpcsv form
    if {!$::running} return
    set ok 0
    if {$code eq 0} {
        if {[catch {parse_csv $tmpcsv} r]} {
            log "could not parse hyperfine CSV: $r\n"
        } else {
            set ::results($i) $r
            set ok 1
        }
    } else {
        log "hyperfine exited with $code\n"
        set ::results($i) failed
    }
    redraw
    if {$ok && $form(perf)} {
        run_perf $i
    } else {
        next_config
    }
}

proc next_config {} {
    set ::queue [lrange $::queue 1 end]
    run_next
}

# run the config under `perf stat` (separately from hyperfine, so the
# timings above are not affected by perf's overhead)
proc run_perf {i} {
    global form tmpperf
    set r [string trim $form(perf_r)]
    if {![string is integer -strict $r] || $r < 1} { set r 1 }
    set cmd [list perf stat -d -r $r -x , -o $tmpperf --]
    lappend cmd {*}[regexp -all -inline {\S+} [command_for $i]]
    set ::status "perf stat [row_label $i]…  ([llength $::queue] left)"
    file delete -force $tmpperf
    spawn $cmd [list after_perf $i $r]
}

proc after_perf {i r code} {
    global tmpperf
    if {!$::running} return
    if {$code eq 0} {
        if {[catch {parse_perf $tmpperf} counters]} {
            log "could not parse perf output: $counters\n"
        } else {
            set ::perf($i) [dict create runs $r counters $counters]
        }
    } else {
        log "perf stat exited with $code\n"
    }
    if {$::selected eq $i} { show_detail $i }
    next_config
}

# perf stat -x, lines: value,unit,event,[variance,]runtime,pct,metric,metric-unit
# where metric-unit is "<human unit>  <metric_name>".
proc parse_perf {path} {
    set f [open $path r]
    set data [read $f]
    close $f
    set counters {}
    foreach line [split $data "\n"] {
        if {[string trim $line] eq "" || [string match "#*" $line]} continue
        set fs [split $line ,]
        if {[llength $fs] < 5} continue
        lassign $fs value unit event
        set rest [lrange $fs 3 end]
        set var ""
        if {[string match *% [lindex $rest 0]]} {
            set var [lindex $rest 0]
            set rest [lrange $rest 1 end]
        }
        lassign $rest runtime pct mval munit
        lappend counters [dict create event $event value $value unit $unit var $var \
                              pct $pct mval $mval munit [string trim $munit]]
    }
    if {![llength $counters]} { error "no counters in $path" }
    return $counters
}

# hyperfine CSV: command,mean,stddev,median,user,system,min,max (seconds).
# Parse numbers from the end so the command field can't confuse us.
proc parse_csv {path} {
    set f [open $path r]
    set lines [split [string trim [read $f]] "\n"]
    close $f
    set row [split [lindex $lines 1] ,]
    lassign [lrange $row end-6 end] mean stddev median user system min max
    foreach v [list $mean $stddev $user $system $min $max] {
        if {![string is double -strict $v]} { error "bad value '$v'" }
    }
    return [dict create mean $mean stddev $stddev median $median \
                user $user system $system min $min max $max]
}

# ---- detail pane ----------------------------------------------------------

proc row_at {wy} {
    set y [.main.c canvasy $wy]
    foreach g $::rowgeom {
        lassign $g i y0 y1
        if {$y >= $y0 && $y < $y1} { return $i }
    }
    return ""
}

proc select_row {i} {
    set ::selected $i
    redraw
    show_detail $i
}

# 12345678 -> 12,345,678
proc group_digits {v} {
    if {![regexp {^(\d+)(\.\d+)?$} $v -> int frac]} { return $v }
    while {[regsub {^(\d+)(\d{3})} $int {\1,\2} int]} {}
    return $int$frac
}

proc show_detail {i} {
    set tv .detail.tv
    $tv delete [$tv children {}]
    if {$i eq ""} {
        .detail.hdr configure -text "Click a result to see its perf stat counters."
        return
    }
    set hdr "[row_label $i]:  [command_for $i]"
    if {[info exists ::results($i)] && $::results($i) ne "failed"} {
        set r $::results($i)
        append hdr "\nwall [fmt_time [dict get $r mean]] ± [fmt_time [dict get $r stddev]]"
        append hdr ",  user [fmt_time [dict get $r user]], sys [fmt_time [dict get $r system]]"
    }
    if {![info exists ::perf($i)]} {
        append hdr [expr {$::form(perf) ? "\n(no perf data yet)" : "\n(perf stat disabled)"}]
        .detail.hdr configure -text $hdr
        return
    }
    set runs [dict get $::perf($i) runs]
    append hdr "\nperf stat, mean of $runs run[expr {$runs > 1 ? "s" : ""}],"
    append hdr " user space only (:u)"
    .detail.hdr configure -text $hdr
    foreach cnt [dict get $::perf($i) counters] {
        dict with cnt {}
        set metric ""
        if {$mval ne "" && $munit ne ""} {
            # "GHz  cycles_frequency" -> "cycles frequency = 1.3 GHz"; perf's
            # unit is only kept when it adds something ("instructions" doesn't)
            set parts [regexp -all -inline {\S+} $munit]
            set name [string map {_ " "} [lindex $parts end]]
            set human [join [lrange $parts 0 end-1]]
            set metric "$name = $mval"
            if {$human in {% GHz} || [string match */sec $human]} { append metric " $human" }
        }
        set tags [expr {[string match <* $value] ? "missing" : ""}]
        $tv insert {} end -values [list $event [group_digits $value] $unit $var $metric \
                                       [expr {$pct eq "" ? "" : "$pct %"}]] -tags $tags
    }
}

# ---- tooltips for the detail pane -----------------------------------------

set perf_doc {
    task-clock       "CPU time summed over all threads (ms). Derived: CPUs utilized = task-clock / wall time, i.e. the effective parallelism."
    context-switches "Times the kernel switched a thread off its CPU (blocking, sleeping, preemption). High when workers park and wake up a lot."
    cpu-migrations   "Times a thread was moved to another core. Each one costs cache locality."
    page-faults      "First touches of memory pages (or pages swapped in). Mostly heap growth: minor heap, major heap, domain stacks."
    cycles           "Core clock cycles spent running the program. Derived: GHz = effective clock rate while running."
    cpu-cycles       "Core clock cycles spent running the program. Derived: GHz = effective clock rate while running."
    instructions     "Instructions retired. Derived: IPC (instructions per cycle). Around 2-4 is compute-bound and healthy; below 1 usually means stalls on memory or branches."
    branches         "Branch instructions executed. Derived: rate in millions per second."
    branch-misses    "Mispredicted branches, each ~15-20 cycles lost. Derived: % of all branches."
    stalled-cycles-frontend "Cycles where fetch/decode delivered nothing (i-cache misses, branch mispredictions). Derived: % of cycles idle."
    stalled-cycles-backend  "Cycles waiting on execution units, usually on memory loads. Derived: % of cycles idle."
    L1-dcache-loads       "Loads served by the L1 data cache."
    L1-dcache-load-misses "L1 data loads that had to go to L2 or further. Derived: miss rate. Contention on shared data (atomics, queues) shows up here."
    LLC-loads        "Loads that reached the last-level (L3) cache."
    LLC-load-misses  "Last-level cache misses: the load went to RAM (~100 ns each)."
    cache-references "Accesses to the last-level cache."
    cache-misses     "Last-level cache misses (went to RAM)."
}

set column_doc {
    event   "perf event name. ':u' means user-space only (kernel.perf_event_paranoid=2 hides kernel activity)."
    value   "Counter value, averaged over the perf -r runs."
    unit    "Unit of the value (empty = plain count)."
    var     "Relative standard deviation of the value across the -r runs."
    metric  "Metric perf derives from this counter (IPC, miss rate, GHz, ...)."
    counted "Share of the run this counter was actually counting. Below 100% the hardware counters were multiplexed and the value is scaled up (estimated)."
}

toplevel .tip -background "#333333" -borderwidth 0
wm overrideredirect .tip 1
wm withdraw .tip
label .tip.l -background "#ffffe0" -foreground "#222222" -justify left \
    -wraplength 380 -padx 6 -pady 4 -borderwidth 1 -relief solid
pack .tip.l
set tip_key ""

proc tip_motion {tv x y X Y} {
    set text ""
    set key ""
    switch [$tv identify region $x $y] {
        heading {
            set col [$tv column [$tv identify column $x $y] -id]
            set key "h:$col"
            set text [dict get $::column_doc $col]
        }
        cell {
            set item [$tv identify item $x $y]
            set ev [lindex [$tv item $item -values] 0]
            set key "r:$ev"
            regsub {:[a-zA-Z]+$} $ev {} base
            if {[dict exists $::perf_doc $base]} {
                set text "$ev\n[dict get $::perf_doc $base]"
            }
        }
    }
    if {$text eq ""} { tip_hide; return }
    if {$key ne $::tip_key} {
        set ::tip_key $key
        .tip.l configure -text $text
        wm deiconify .tip
        raise .tip
    }
    wm geometry .tip +[expr {$X + 14}]+[expr {$Y + 16}]
}

proc tip_hide {} {
    set ::tip_key ""
    wm withdraw .tip
}

proc kill_tree {p} {
    catch {
        foreach c [split [exec pgrep -P $p] "\n"] { kill_tree $c }
    }
    catch {exec kill $p}
}

proc stop_run {} {
    global chan
    set ::running 0
    if {$chan ne ""} {
        foreach p [pid $chan] { kill_tree $p }
        fileevent $chan readable {}
        catch {fconfigure $chan -blocking 1; close $chan}
        set chan ""
    }
    log "\n-- stopped --\n"
    set ::status "stopped"
    set_running 0
}

# ---- drawing --------------------------------------------------------------

proc fmt_time {s} {
    if {$s >= 1.0}   { return [format "%.3f s" $s] }
    if {$s >= 1e-3}  { return [format "%.1f ms" [expr {$s * 1e3}]] }
    return [format "%.1f µs" [expr {$s * 1e6}]]
}

# "nice" tick step for range [0, max] with ~n ticks
proc nice_step {max n} {
    set raw [expr {$max / double($n)}]
    set mag [expr {10 ** floor(log10($raw))}]
    foreach m {1 2 2.5 5 10} {
        if {$m * $mag >= $raw} { return [expr {$m * $mag}] }
    }
    return [expr {10 * $mag}]
}

font create BarBold {*}[font actual TkDefaultFont] -weight bold

# Each row: a text line (label, cpu info, mean ± σ, ratio), then the wall bar
# and the thinner cpu bar underneath, spanning the whole canvas width.
proc redraw {} {
    global bench configs results col queue running
    set c .main.c
    $c delete all
    set W [winfo width $c]
    if {$W < 50} return

    set rows {}
    for {set i 0} {$i < [llength $configs($bench)]} {incr i} {
        if {[info exists results($i)] || $::enabled($i)} { lappend rows $i }
    }

    set scale_max 0.0
    set fastest ""
    foreach i $rows {
        if {![info exists results($i)] || $results($i) eq "failed"} continue
        set r $results($i)
        set m [dict get $r mean]
        foreach v [list [dict get $r max] [expr {$m + [dict get $r stddev]}] \
                       [expr {[dict get $r user] + [dict get $r system]}]] {
            if {$v > $scale_max} { set scale_max $v }
        }
        if {$fastest eq "" || $m < $fastest} { set fastest $m }
    }
    # a little headroom so the longest bar doesn't touch the edge
    set scale_max [expr {$scale_max * 1.02}]

    set font TkDefaultFont
    set fh [font metrics $font -linespace]
    set margin 10
    set x0 $margin
    set x1 [expr {$W - $margin}]
    set plotw [expr {$x1 - $x0}]

    # legend, wrapping like the top bar
    set lx $x0
    set ly [expr {$margin + $fh / 2}]
    foreach {kind name} {bar "wall mean" user "user CPU" sys "sys CPU" range "min–max" sigma "±σ"} {
        set w [expr {32 + [font measure $font $name]}]
        if {$lx > $x0 && $lx + $w > $x1} {
            set lx $x0
            incr ly [expr {$fh + 4}]
        }
        switch $kind {
            range { $c create line $lx $ly [expr {$lx+14}] $ly -fill $col(range) }
            sigma {
                $c create line $lx $ly [expr {$lx+14}] $ly -fill $col(sigma) -width 2
                $c create line $lx [expr {$ly-5}] $lx [expr {$ly+5}] -fill $col(sigma) -width 2
                $c create line [expr {$lx+14}] [expr {$ly-5}] [expr {$lx+14}] [expr {$ly+5}] \
                    -fill $col(sigma) -width 2
            }
            default {
                $c create rectangle $lx [expr {$ly-6}] [expr {$lx+14}] [expr {$ly+6}] \
                    -fill $col($kind) -outline ""
            }
        }
        $c create text [expr {$lx+18}] $ly -text $name -anchor w -fill $col(text)
        incr lx [expr {$w + 12}]
    }

    set barh 14
    set cpuh 6
    set rowh [expr {$fh + 2 + $barh + 2 + $cpuh + 10}]
    set top [expr {$ly + $fh}]

    set ::rowgeom {}
    set y $top
    foreach i $rows {
        set rh $rowh
        set ty [expr {$y + $fh / 2}]
        $c create text $x0 $ty -text [row_label $i] -anchor w -fill $col(text) -font BarBold
        set labelw [font measure BarBold [row_label $i]]
        if {![info exists results($i)]} {
            set msg [expr {$running && $i == [lindex $queue 0] ? "running…" : ($running ? "pending" : "")}]
            $c create text $x1 $ty -text $msg -anchor e -fill $col(dim)
        } elseif {$results($i) eq "failed"} {
            $c create text $x1 $ty -text "failed (see log)" -anchor e -fill $col(sys)
        } else {
            set r $results($i)
            dict with r {}
            # right side: "mean ± σ   ratio", ratio in bold when fastest
            if {$mean == $fastest} {
                set ratio "fastest"
                set rfont BarBold
            } else {
                set ratio [format "%.2f× slower" [expr {$mean / $fastest}]]
                set rfont $font
            }
            set stats "[fmt_time $mean] ± [fmt_time $stddev]"
            set statsw [expr {[font measure $font $stats] + 12 + [font measure $rfont $ratio]}]
            # too narrow for label and stats on one line: stats go on a second line
            set sty $ty
            if {$labelw + 12 + $statsw > $plotw} {
                incr sty $fh
                incr rh $fh
            }
            $c create text $x1 $sty -text $ratio -anchor e -fill $col(text) -font $rfont
            set sx1 [expr {$x1 - [font measure $rfont $ratio] - 12}]
            $c create text $sx1 $sty -text $stats -anchor e -fill $col(text)
            # middle: cpu info, only if it fits
            set cpu [format "cpu %s (%.1f× wall)" [fmt_time [expr {$user + $system}]] \
                         [expr {($user + $system) / $mean}]]
            set cx [expr {$x0 + $labelw + 12}]
            if {$cx + [font measure $font $cpu] + 12 < $sx1 - [font measure $font $stats]} {
                $c create text $cx $ty -text $cpu -anchor w -fill $col(dim)
            }

            set sx [expr {$plotw / $scale_max}]
            set by0 [expr {$sty + $fh / 2 + 2}]
            set by1 [expr {$by0 + $barh}]
            set bc [expr {($by0 + $by1) / 2}]
            set fill [expr {$mean == $fastest ? $col(fast) : $col(bar)}]
            $c create rectangle $x0 $by0 [expr {$x0 + $mean * $sx}] $by1 \
                -fill $fill -outline ""
            $c create line [expr {$x0 + $min * $sx}] $bc [expr {$x0 + $max * $sx}] $bc \
                -fill $col(range)
            set sl [expr {$x0 + max(0, $mean - $stddev) * $sx}]
            set sr [expr {$x0 + ($mean + $stddev) * $sx}]
            $c create line $sl $bc $sr $bc -fill $col(sigma) -width 2
            $c create line $sl [expr {$by0 + 3}] $sl [expr {$by1 - 3}] -fill $col(sigma) -width 2
            $c create line $sr [expr {$by0 + 3}] $sr [expr {$by1 - 3}] -fill $col(sigma) -width 2
            set cy0 [expr {$by1 + 2}]
            set cy1 [expr {$cy0 + $cpuh}]
            set ux [expr {$x0 + $user * $sx}]
            $c create rectangle $x0 $cy0 $ux $cy1 -fill $col(user) -outline ""
            $c create rectangle $ux $cy0 [expr {$ux + $system * $sx}] $cy1 \
                -fill $col(sys) -outline ""
        }
        lappend ::rowgeom [list $i [expr {$y - 4}] [expr {$y + $rh - 6}]]
        if {$i eq $::selected} {
            $c create rectangle 0 [expr {$y - 4}] $W [expr {$y + $rh - 6}] \
                -fill $col(sel) -outline "" -tags sel
        }
        incr y $rh
    }
    set bottom $y

    # grid + axis, roughly one tick per 90px
    if {$scale_max > 0} {
        set step [nice_step $scale_max [expr {max(2, $plotw / 90)}]]
        for {set t 0.0} {$t <= $scale_max} {set t [expr {$t + $step}]} {
            set x [expr {$x0 + $t / $scale_max * $plotw}]
            $c create line $x $top $x $bottom -fill $col(grid) -tags grid
            set lbl [expr {$t == 0 ? "0" : [fmt_time $t]}]
            set half [expr {[font measure $font $lbl] / 2}]
            if {$t == 0} {
                set anchor nw
            } elseif {$x + $half > $x1 + $margin} {
                set anchor ne
            } else {
                set anchor n
            }
            $c create text $x [expr {$bottom + 2}] -text $lbl -anchor $anchor -fill $col(dim)
        }
    }

    $c lower grid
    $c lower sel
    set bb [$c bbox all]
    $c configure -scrollregion [list 0 0 $W [expr {[lindex $bb 3] + $margin}]]
}

on_bench_change
