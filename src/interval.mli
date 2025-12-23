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
