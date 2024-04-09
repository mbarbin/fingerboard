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

type t = Characterized_interval.t list

let major_just =
  let second quality acoustic_interval =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn ~interval ~acoustic_interval
  in
  let minor_ton = second Major Acoustic_interval.just_minor_ton in
  let major_ton = second Major Acoustic_interval.just_major_ton in
  let semiton = second Minor Acoustic_interval.just_diatonic_semiton in
  [ major_ton; minor_ton; semiton; major_ton; minor_ton; major_ton; semiton ]
;;

let major_pythagorean =
  let second quality =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.pythagorean interval)
  in
  let ton = second Major in
  let semiton = second Minor in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_just_e53 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions)
  in
  let minor_ton = second Major 8 in
  let major_ton = second Major 9 in
  let semiton = second Minor 5 in
  [ major_ton; minor_ton; semiton; major_ton; minor_ton; major_ton; semiton ]
;;

let major_pythagorean_e53 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions)
  in
  let ton = second Major 9 in
  let semiton = second Minor 4 in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_e12 =
  let second quality =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.equal_tempered_12 interval)
  in
  let ton = second Major in
  let semiton = second Minor in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_e31 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:31 ~number_of_divisions)
  in
  let ton = second Major 5 in
  let semiton = second Minor 3 in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_e55 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:55 ~number_of_divisions)
  in
  let ton = second Major 9 in
  let semiton = second Minor 5 in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;
