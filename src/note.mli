(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

(** *)

module Letter_name : sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val all : t list
  val to_string : t -> string
  val pred : t -> t
  val succ : t -> t

  (** How many semitons are there from [t] to [succ t]. *)
  val semitons_step : from:t -> int

  val succ_octave_designation
    :  t
    -> octave_designation:Octave_designation.t
    -> Octave_designation.t

  val pred_octave_designation
    :  t
    -> octave_designation:Octave_designation.t
    -> Octave_designation.t
end

module Symbol : sig
  type t =
    | Triple_flat
    | Double_flat
    | Flat
    | Natural
    | Sharp
    | Double_sharp
    | Triple_sharp

  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val all : t list
  val to_string : t -> string
  val prefix_notation : t -> string
  val semitons_shift : t -> int
  val succ : t -> t option
  val pred : t -> t option
end

type t =
  { letter_name : Letter_name.t
  ; symbol : Symbol.t
  ; octave_designation : Octave_designation.t
  }

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val to_string : t -> string
