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

(** Manipulating small integers displayed as roman numerals. By convention,
    roman numerals are used to designate the vibrating strings of the
    instruments, with I (1) being the higher string, and increasing numbers
    going down. For example, the lower C string on the 4-strings cello is IV
    (4).

    Roman numeral can also be used to designate the degree of the notes in a
    scale, with I being the tonic, V the dominant, etc. *)

type t =
  | I
  | II
  | III
  | IV
  | V
  | VI
  | VII
[@@deriving compare, equal, enumerate, hash, sexp_of]

val to_string : t -> string

(** Because we don't need to handle instruments with a very large number of
    strings, only a few roman numerals are representable with this module. An
    overflow will cause the function to raise. *)
val of_int_exn : int -> t

val one : t
val succ_exn : t -> t
val to_int : t -> int
