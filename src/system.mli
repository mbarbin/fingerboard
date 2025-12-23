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

type t

val to_dyn : t -> Dyn.t

(** Display t in the form of multiple tables in a human readable way. *)
val to_ascii_tables : t -> string

(** Instantiate a system with open strings starting from the highest vibrating
    strings, and with open strings following the given intervals while going
    down the instruments. The resulting system is set with 1 more string than
    the length of the array. *)
val create
  :  high_vibrating_string:Note.t
  -> pitch:Frequency.t
  -> intervals_going_down:Characterized_interval.t array
  -> t

(** Reset the pitch from any given string. *)
val reset_pitch : t -> Roman_numeral.t -> pitch:Frequency.t -> unit

val pitch : t -> Fingerboard_location.t -> Frequency.t

(** Returns None if [from] has a higher pitch than [to_]. *)
val acoustic_interval
  :  t
  -> from:Fingerboard_location.t
  -> to_:Fingerboard_location.t
  -> Acoustic_interval.t option

(** Add a new position to the system. A call to this function will cause
    multiple fingerboard locations to be added to the system systematically,
    in particular:

    - the locations that are at the same distance to the open string on all
      other strings;

    - the locations on the same string at higher octaves. By default, 3
      locations on the strings are added, on 3 octaves. This may be controlled
      by the [on_n_octaves] parameter.

    The position's name must be unique within a system, that is a fingerboard
    position with the same name must not have been previously added to that
    system. Because positions are systematically added at the octaves of the
    given string portion, the one given as reference for a new position is
    expected to be lower than one octave to the open string. *)
val add_fingerboard_position_exn
  :  ?on_n_octaves:int
  -> t
  -> Fingerboard_position.t
  -> unit

(** Returns the list of fingerboard_positions known by this system, returned in
    the order they are found going up the fingerboard starting from the open
    string. *)
val fingerboard_positions : t -> Fingerboard_position.t list

(** Find the position created with the given name if present or raise. The
    position returned is that of the first octave on the string. *)
val find_fingerboard_position_exn : t -> name:string -> Fingerboard_position.t

(** Check whether given positions and locations are present in the system. *)

val exists_fingerboard_position : t -> Fingerboard_position.t -> bool
val exists_fingerboard_location : t -> Fingerboard_location.t -> bool

val find_next_located_note
  :  t
  -> Located_note.t
  -> Characterized_interval.t
  -> Located_note.t option

(** Return the given located_note if a position was added to the system for the
    open string. *)
val open_string : t -> Roman_numeral.t -> Located_note.t option

(** Try and go up from the first supplied located note, according to the
    characterized scale supplied. If no available positions are available to
    produce the right intervals, the scales stops and the degree built so far
    are returned. If the degree of the scale have all been used before
    reaching the highest note desired, the same scale is reused from that
    point.

    This function will favor going up on the strings, and play with longer
    vibrating strings rather than using high positions on lower strings (when
    multiple choices exist). *)
val make_scale
  :  t
  -> characterized_scale:Characterized_scale.t
  -> from:Located_note.t
  -> to_:Note.t
  -> Located_note.t list

(** Given a located note, find a position on the lower string next to where it
    is that yields the same note. Returns [None] if such a position does not
    exists, or if the given note is already played on the lowest string
    available. *)
val find_same_note_one_string_down : t -> Located_note.t -> Located_note.t option

module Double_stops : sig
    type system
    type t = Double_stop.t list

    val to_ascii_table : system -> t -> string

    module Adjustment : sig
      type t =
        { from : Acoustic_interval.t
        ; to_ : Acoustic_interval.t
        }
    end

    val adjust : system -> tonic:Note.t -> adjustment:Adjustment.t -> t -> t

    val make_scale
      :  ?adjustment:Adjustment.t
      -> system
      -> characterized_scale:Characterized_scale.t
      -> interval_number:Interval.Number.t
      -> from:Located_note.t
      -> to_:Note.t
      -> t
  end
  with type system := t
