(*_*********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*_  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*_                                                                                *)
(*_  This file is part of Fingerboard.                                             *)
(*_                                                                                *)
(*_  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*_  the terms of the GNU Affero General Public License as published by the Free   *)
(*_  Software Foundation, either version 3 of the License, or any later version.   *)
(*_                                                                                *)
(*_  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*_  for more details.                                                             *)
(*_                                                                                *)
(*_  You should have received a copy of the GNU Affero General Public License      *)
(*_  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(*_*********************************************************************************)

(** A strictly positive ratio of two natural numbers. *)

type t = private
  { numerator : int
  ; denominator : int
  }

val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
val to_string : t -> string
val create_exn : numerator:int -> denominator:int -> t
val inverse : t -> t
val multiply : t -> t -> t
val divide : t -> t -> t

module Reduced : sig
  type natural_ratio := t

  (** A [natural_ratio] kept in its primed decomposition form. *)
  type t

  val to_dyn : t -> Dyn.t
  val to_string : t -> string
  val equal : t -> t -> bool
  val one : t

  (** [prime] is assumed to be a prime number. The resulting [t] will be invalid
      and the behavior of the rest of the module unspecified if it isn't. The
      exponent is expected to be non null. Checking that [prime] is indeed
      prime is only done if its value is small enough. This should suffice in
      practice, since creating such values is done with small ps. *)
  val create_exn : prime:int -> exponent:int -> t

  val compound : t list -> t
  val inverse : t -> t
  val multiply : t -> t -> t
  val divide : t -> t -> t
  val to_natural_ratio : t -> natural_ratio

  (** When both the numerator and denominator and below a small bound, this
      function can be used to instantiate a [t]. Raises if out of supported
      bounds. *)
  val of_small_natural_ratio_exn : numerator:int -> denominator:int -> t
end
