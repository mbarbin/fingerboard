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

module Letter_name : sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
  [@@deriving compare, enumerate, equal]

  val to_dyn : t -> Dyn.t
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
  [@@deriving compare, enumerate, equal]

  val to_dyn : t -> Dyn.t
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
[@@deriving compare, equal]

val to_dyn : t -> Dyn.t
val to_string : t -> string
