(**********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*                                                                                *)
(*  This file is part of Fingerboard.                                             *)
(*                                                                                *)
(*  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*  the terms of the GNU Affero General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or any later version.   *)
(*                                                                                *)
(*  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*  for more details.                                                             *)
(*                                                                                *)
(*  You should have received a copy of the GNU Affero General Public License      *)
(*  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(**********************************************************************************)

type t =
  | I
  | II
  | III
  | IV
  | V
  | VI
  | VII
[@@deriving compare, equal, enumerate, hash]

let constructor_name = function
  | I -> "I"
  | II -> "II"
  | III -> "III"
  | IV -> "IV"
  | V -> "V"
  | VI -> "VI"
  | VII -> "VII"
;;

let to_string = constructor_name
let to_dyn t = Dyn.variant (constructor_name t) []
let sexp_of_t t = Dyn.to_sexp (to_dyn t)

let to_int = function
  | I -> 1
  | II -> 2
  | III -> 3
  | IV -> 4
  | V -> 5
  | VI -> 6
  | VII -> 7
;;

let of_int_exn = function
  | 1 -> I
  | 2 -> II
  | 3 -> III
  | 4 -> IV
  | 5 -> V
  | 6 -> VI
  | 7 -> VII
  | i -> raise_s [%sexp "Out of bounds", [%here], (i : int)]
;;

let one = I
let succ_exn t = of_int_exn (Int.succ (to_int t))
