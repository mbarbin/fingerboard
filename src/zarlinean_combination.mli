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

(** When playing a just major scale in double stops of thirds or sixths, each
    pair of notes is expected to form a just interval. Assuming one note is
    extracted from a pythagorean system (noted [p]), the other must then be
    adjusted accordingly. We note [z] a note taken from a just system to be
    tuned with the [p] note.

    Since there are 7 degrees, it follows that for it to work, at least one
    degree must be changed (from [p] to [z] or the opposite) as we go up the
    scale depending on whether it is the low or the high note. So as to limit
    the number of changing degrees, we do not consider solutions to this
    problems with more than 1 changing degree.

    An example of such combination is the following:
    {v
        ┌───┬─────┬─────┬────┬───┬────┬─────┐
        │ I │ II  │ III │ IV │ V │ VI │ VII │
        ├───┼─────┼─────┼────┼───┼────┼─────┤
        │ p │ p,z │ z   │ p  │ p │ z  │ z   │
        └───┴─────┴─────┴────┴───┴────┴─────┘
    v}

    In this example, the changing note is set on the second degree, and is
    played [z] when played with the degree IV, and [p] when played with the
    degree VII.

    This module goes over all such possible theoretical combinations. There are
    displayed in the test for this module. *)

type t [@@deriving compare, equal, sexp_of]

val to_dyn : t -> Dyn.t

(** Display t in the form of multiple tables in a human readable way. *)
val to_ascii_tables : t -> string

(** Build all combination of [t's]. *)
val all : t list Lazy.t
