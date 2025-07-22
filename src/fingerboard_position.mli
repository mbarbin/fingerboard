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

(** A position is a fingerboard location that is not characterized by any
    string, but rather designated that position that is available on all
    strings at the same relative distance to the open strings. A position is
    given a name for convenience when in use within a system.

    A position may serve for playing several notes (with systems with enharmony
    or tempered systems). Some system may also have distinct positions to play
    the same notes, using slightly different pitches depending on which scale or
    tonality it belongs, so there is no one-to-one match between a note and a
    position. *)

type t [@@deriving compare, equal, hash, sexp_of]

(** Retrieve the name supplied with [t] was created. *)
val name : t -> string

(** A string identifier constituted from the name and the octave designation. *)
val to_string : t -> string

val ascii_table_columns : t Print_table.Column.t list
val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t

(** Returns the same position some number of octaves higher. *)
val at_octave : t -> octave:int -> t

(** Expected to be given a vibrating string portion that is between the open
    string and the first octave. Other positions at higher octave may be
    constructed using [at_octave]. *)
val create_exn
  :  name:string
  -> acoustic_interval_to_the_open_string:Acoustic_interval.t
  -> t

val open_string : t
