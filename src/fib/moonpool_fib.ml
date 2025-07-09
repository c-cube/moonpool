(** Fibers for moonpool.

    See {!Fiber} for the most important explanations.

    @since 0.6. *)

module Fiber = Fiber
[@@deprecated "use picos structured concurrency or something else"]

module Fls = Fls
module Handle = Handle

module Main = Main
[@@deprecated "use picos structured concurrency or something else"]

[@@@ocaml.alert "-deprecated"]

include Fiber
include Main
