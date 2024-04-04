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

let%expect_test "semitons_step" =
  let rec aux start acc index =
    if Note.Letter_name.equal start index && acc > 0
    then acc
    else (
      let acc = acc + Note.Letter_name.semitons_step ~from:index in
      aux start acc (Note.Letter_name.succ index))
  in
  let number_of_semitons_per_octave = aux A 0 A in
  print_s [%sexp { number_of_semitons_per_octave : int }];
  [%expect {| ((number_of_semitons_per_octave 12)) |}]
;;
