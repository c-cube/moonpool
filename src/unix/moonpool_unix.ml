(*
(** Unix-compatible event loop *)
class type event_loop =
  object
    method one_step : block:bool -> unit -> unit
    (** Run one step of the event loop.
        @param block if [true], the call might block until the next timeout
        or until the next IO event occurs. If [false], this does not
        block and returns after having processed the available events. *)

    method on_readable :
      Unix.file_descr -> (Cancel_handle.t -> unit) -> Cancel_handle.t
    (** [on_readable fd f] creates a new event [ev], and will run [f ev] when
      [fd] becomes readable *)

    method on_writable :
      Unix.file_descr -> (Cancel_handle.t -> unit) -> Cancel_handle.t

    method on_timer :
      float -> repeat:bool -> (Cancel_handle.t -> unit) -> Cancel_handle.t
    (** [on_timer delay ~repeat f] runs [f] after [delay].
      @param repeat if true runs [f] every [delay] seconds *)

    method fake_io : Unix.file_descr -> unit
    (** Simulate activity on the FD *)

    method interrupt_if_in_blocking_section : unit
    (** If run from inside the event loop when it's waiting, wakes the event loop up *)

    method has_pending_tasks : bool
  end

(* TODO: for lwt backend:
      let has_pending_tasks (self : #t) : bool =
        self#readable_count > 0 || self#writable_count > 0 || self#timer_count > 0

          method readable_count : int
          (** Number of events waiting for FDs to be readable FDs *)

          method writable_count : int

          method timer_count : int
          (** Number of events waiting on a timer *)
   let readable_count (self : #t) = self#readable_count
   let writable_count (self : #t) = self#writable_count
   let timer_count (self : #t) = self#timer_count
*)

let[@inline] one_step (self : #t) ~block () = self#one_step ~block ()
let[@inline] on_readable (self : #t) fd f = self#on_readable fd f
let[@inline] on_writable (self : #t) fd f = self#on_writable fd f

let[@inline] on_timer (self : #t) delay ~repeat f =
  self#on_timer delay ~repeat f

let[@inline] fake_io (self : #t) fd = self#fake_io fd
let[@inline] has_pending_tasks (self : #t) = self#has_pending_tasks

let[@inline] interrupt_if_in_blocking_section (self : #t) : unit =
  self#interrupt_if_in_blocking_section
  *)
