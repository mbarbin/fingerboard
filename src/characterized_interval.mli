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

type t = private
  { interval : Interval.t
  ; acoustic_interval : Acoustic_interval.t
  }
[@@deriving sexp_of]

(** A number representing, in cents, the maximum deviation that is allowed as
    between an acoustic interval and its 12-equal tempered equivalent. Trying
    to create a [t] exceeding this bound will result in [create_exn] to raise. *)
val allowed_deviation_from_equal_tempered_12_in_cents : float

val create_exn : interval:Interval.t -> acoustic_interval:Acoustic_interval.t -> t
