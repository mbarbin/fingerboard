(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

module Array = Array0
module Bool = Bool0
module Code_error = Code_error0
module Dyn = Dyn0
module Float = Float0
module Int = Int0
module List = List0
module Myers = Myers0
module Option = Option0
module Ordering = Ordering0
module String = String0

val print_dyn : Dyn.t -> unit
val phys_equal : 'a -> 'a -> bool

(** {1 Expect test helpers} *)

(** [require cond] raises if [cond] is false. *)
val require : bool -> unit

(** [require_does_raise f] raises if [f ()] does not raise, and prints the
    exception if it does. *)
val require_does_raise : (unit -> 'a) -> unit

(** To use [require_equal], the type must provide [equal] and [to_dyn]. *)
module With_equal_and_dyn : sig
  module type S = sig
    type t

    val equal : t -> t -> bool
    val to_dyn : t -> Dyn.t
  end
end

(** [require_equal (module M) v1 v2] raises if [v1] and [v2] are not equal,
    naming both in the message. Prefer this to printing a value and reviewing
    the expectation: an expectation can be promoted by accident, an assertion
    cannot. *)
val require_equal : (module With_equal_and_dyn.S with type t = 'a) -> 'a -> 'a -> unit
