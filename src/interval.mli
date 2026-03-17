(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

(** *)

module Quality : sig
  type t =
    | Doubly_diminished
    | Diminished
    | Minor
    | Perfect
    | Major
    | Augmented
    | Doubly_augmented

  val all : t list
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val prefix_notation : t -> string
end

module Number : sig
  type t =
    | Unison
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Octave

  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val all : t list
  val to_int : t -> int
  val accepts_minor_major_quality : t -> bool
end

type t =
  { number : Number.t
  ; quality : Quality.t
  ; additional_octaves : int
  }

val hash : t -> int
val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val unison : t
val name : t -> string
val to_string : t -> string
val number_of_semitons : t -> int
val compute : from:Note.t -> to_:Note.t -> unit -> t option
val shift_up : t -> Note.t -> Note.t option
val shift_down : t -> Note.t -> Note.t option
