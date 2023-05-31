(** Moonpool

  A pool within a bigger pool (ie the ocean). Here, we're talking about
  pools of [Thread.t] which live within a fixed pool of [Domain.t].
*)

type 'a or_error = ('a, exn * Printexc.raw_backtrace) result

(** Thread pool *)
module Pool : sig
  type t

  val create :
    ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
    ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
    ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
    ?min:int ->
    ?per_domain:int ->
    unit ->
    t
  (** [create ()] makes a new thread pool.
     @param on_init_thread called at the beginning of each new thread
       in the pool.
  *)

  val shutdown : t -> unit
  (** Shutdown the pool and wait for it to terminate. Idempotent. *)

  val run : t -> (unit -> unit) -> unit
  (** [run pool f] schedules [f] for later execution on the pool
      in one of the threads. *)
end

(** Futures *)
module Fut : sig
  type 'a t
  (** A future with a result of type ['a] *)

  type 'a promise
  (** A promise, which can be fulfilled exactly once to set
      the corresponding future *)

  val make : unit -> 'a t * 'a promise
  (** Make a new future with the associated promise *)

  val on_result : 'a t -> ('a or_error -> unit) -> unit
  (** [on_result fut f] registers [f] to be called in the future
      when [fut] is set ;
      or calls [f] immediately if [fut] is already set. *)

  exception Already_fulfilled

  val fulfill : 'a promise -> 'a or_error -> unit
  (** Fullfill the promise, setting the future at the same time.
      @raise Already_fulfilled if the promise is already fulfilled. *)

  val return : 'a -> 'a t
  (** Already settled future, with a result *)

  val fail : exn -> Printexc.raw_backtrace -> _ t
  (** Already settled future, with a failure *)

  val of_result : 'a or_error -> 'a t
end
