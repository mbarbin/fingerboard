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

open! Fingerboard_cello_system_e12
open! Fingerboard_cello_system_e19
open! Fingerboard_cello_system_e31
open! Fingerboard_cello_system_e53
open! Fingerboard_cello_system_e55
open! Fingerboard_cello_system_just
open! Fingerboard_cello_system_pythagorean

let%expect_test "E12.sexp_of_t" =
  let t = force E12.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         146.83238395870379)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         97.998858995437331)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         65.406391325149656)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             12)
          (number_of_divisions 7))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             12)
          (number_of_divisions 7))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             12)
          (number_of_divisions 7))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 1))))
       ((name      M2e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 2))))
       ((name      m3e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 3))))
       ((name      M3e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 4))))
       ((name      4e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 5))))
       ((name      A4e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 6))))
       ((name      5e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 7))))
       ((name      m6e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 8))))
       ((name      M6e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 9))))
       ((name      m7e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 10))))
       ((name      M7e)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 11))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 1))))
       ((name      M2e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 2))))
       ((name      m3e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 3))))
       ((name      M3e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 4))))
       ((name      4e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 5))))
       ((name      A4e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 6))))
       ((name      5e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 7))))
       ((name      m6e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 8))))
       ((name      M6e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 9))))
       ((name      m7e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 10))))
       ((name      M7e)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 11))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 1))))
       ((name      M2e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 2))))
       ((name      m3e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 3))))
       ((name      M3e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 4))))
       ((name      4e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 5))))
       ((name      A4e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 6))))
       ((name      5e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 7))))
       ((name      m6e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 8))))
       ((name      M6e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 9))))
       ((name      m7e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 10))))
       ((name      M7e)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             12)
           (number_of_divisions 11))))))) |}]
;;

let%expect_test "E53.sexp_of_t" =
  let t = force E53.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         146.67244526002719)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         97.785482720707591)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         65.192890279901448)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             53)
          (number_of_divisions 31))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             53)
          (number_of_divisions 31))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             53)
          (number_of_divisions 31))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 4))))
       ((name      m2z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 5))))
       ((name      M2z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 8))))
       ((name      M2p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 9))))
       ((name      m3p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 13))))
       ((name      m3z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 14))))
       ((name      M3z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 17))))
       ((name      M3p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 18))))
       ((name      4p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 22))))
       ((name      4z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 23))))
       ((name      A4z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 26))))
       ((name      d5z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 27))))
       ((name      5z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 30))))
       ((name      5p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 31))))
       ((name      m6p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 35))))
       ((name      m6z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 36))))
       ((name      M6z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 39))))
       ((name      M6p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 40))))
       ((name      m7p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 44))))
       ((name      m7z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 45))))
       ((name      M7z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 48))))
       ((name      M7p-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 49))))
       ((name      8z-e53)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 52))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 4))))
       ((name      m2z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 5))))
       ((name      M2z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 8))))
       ((name      M2p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 9))))
       ((name      m3p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 13))))
       ((name      m3z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 14))))
       ((name      M3z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 17))))
       ((name      M3p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 18))))
       ((name      4p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 22))))
       ((name      4z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 23))))
       ((name      A4z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 26))))
       ((name      d5z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 27))))
       ((name      5z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 30))))
       ((name      5p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 31))))
       ((name      m6p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 35))))
       ((name      m6z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 36))))
       ((name      M6z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 39))))
       ((name      M6p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 40))))
       ((name      m7p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 44))))
       ((name      m7z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 45))))
       ((name      M7z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 48))))
       ((name      M7p-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 49))))
       ((name      8z-e53)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 52))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 4))))
       ((name      m2z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 5))))
       ((name      M2z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 8))))
       ((name      M2p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 9))))
       ((name      m3p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 13))))
       ((name      m3z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 14))))
       ((name      M3z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 17))))
       ((name      M3p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 18))))
       ((name      4p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 22))))
       ((name      4z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 23))))
       ((name      A4z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 26))))
       ((name      d5z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 27))))
       ((name      5z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 30))))
       ((name      5p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 31))))
       ((name      m6p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 35))))
       ((name      m6z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 36))))
       ((name      M6z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 39))))
       ((name      M6p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 40))))
       ((name      m7p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 44))))
       ((name      m7z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 45))))
       ((name      M7z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 48))))
       ((name      M7p-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 49))))
       ((name      8z-e53)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             53)
           (number_of_divisions 52))))))) |}];
  ()
;;

let%expect_test "E19.sexp_of_t" =
  let t = force E19.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         147.27945113469193)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         98.596530575164124)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         66.005649576795662)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             19)
          (number_of_divisions 11))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             19)
          (number_of_divisions 11))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             19)
          (number_of_divisions 11))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 1))))
       ((name      m2-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 2))))
       ((name      M2-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 3))))
       ((name      A2-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 4))))
       ((name      m3-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 5))))
       ((name      M3-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 6))))
       ((name      A3-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 7))))
       ((name      4-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 8))))
       ((name      A4-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 9))))
       ((name      d5-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 10))))
       ((name      5-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 11))))
       ((name      A5-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 12))))
       ((name      m6-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 13))))
       ((name      M6-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 14))))
       ((name      d7-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 15))))
       ((name      m7-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 16))))
       ((name      M7-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 17))))
       ((name      d8-e19)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 18))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 1))))
       ((name      m2-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 2))))
       ((name      M2-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 3))))
       ((name      A2-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 4))))
       ((name      m3-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 5))))
       ((name      M3-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 6))))
       ((name      A3-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 7))))
       ((name      4-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 8))))
       ((name      A4-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 9))))
       ((name      d5-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 10))))
       ((name      5-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 11))))
       ((name      A5-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 12))))
       ((name      m6-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 13))))
       ((name      M6-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 14))))
       ((name      d7-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 15))))
       ((name      m7-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 16))))
       ((name      M7-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 17))))
       ((name      d8-e19)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 18))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 1))))
       ((name      m2-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 2))))
       ((name      M2-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 3))))
       ((name      A2-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 4))))
       ((name      m3-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 5))))
       ((name      M3-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 6))))
       ((name      A3-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 7))))
       ((name      4-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 8))))
       ((name      A4-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 9))))
       ((name      d5-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 10))))
       ((name      5-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 11))))
       ((name      A5-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 12))))
       ((name      m6-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 13))))
       ((name      M6-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 14))))
       ((name      d7-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 15))))
       ((name      m7-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 16))))
       ((name      M7-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 17))))
       ((name      d8-e19)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             19)
           (number_of_divisions 18))))))) |}];
  ()
;;

let%expect_test "E31.sexp_of_t" =
  let t = force E31.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         147.10623162496987)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         98.364742649542208)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         65.773030072427773)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             31)
          (number_of_divisions 18))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             31)
          (number_of_divisions 18))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             31)
          (number_of_divisions 18))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 2))))
       ((name      m2-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 3))))
       ((name      M2-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 5))))
       ((name      d3-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 6))))
       ((name      A2-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 7))))
       ((name      m3-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 8))))
       ((name      M3-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 10))))
       ((name      d4-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 11))))
       ((name      A3-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 12))))
       ((name      4-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 13))))
       ((name      A4-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 15))))
       ((name      d5-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 16))))
       ((name      5-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 18))))
       ((name      A5-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 20))))
       ((name      m6-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 21))))
       ((name      M6-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 23))))
       ((name      d7-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 24))))
       ((name      m7-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 26))))
       ((name      M7-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 28))))
       ((name      d8-e31)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 29))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 2))))
       ((name      m2-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 3))))
       ((name      M2-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 5))))
       ((name      d3-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 6))))
       ((name      A2-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 7))))
       ((name      m3-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 8))))
       ((name      M3-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 10))))
       ((name      d4-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 11))))
       ((name      A3-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 12))))
       ((name      4-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 13))))
       ((name      A4-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 15))))
       ((name      d5-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 16))))
       ((name      5-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 18))))
       ((name      A5-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 20))))
       ((name      m6-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 21))))
       ((name      M6-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 23))))
       ((name      d7-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 24))))
       ((name      m7-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 26))))
       ((name      M7-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 28))))
       ((name      d8-e31)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 29))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 2))))
       ((name      m2-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 3))))
       ((name      M2-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 5))))
       ((name      d3-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 6))))
       ((name      A2-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 7))))
       ((name      m3-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 8))))
       ((name      M3-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 10))))
       ((name      d4-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 11))))
       ((name      A3-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 12))))
       ((name      4-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 13))))
       ((name      A4-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 15))))
       ((name      d5-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 16))))
       ((name      5-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 18))))
       ((name      A5-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 20))))
       ((name      m6-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 21))))
       ((name      M6-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 23))))
       ((name      d7-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 24))))
       ((name      m7-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 26))))
       ((name      M7-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 28))))
       ((name      d8-e31)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             31)
           (number_of_divisions 29))))))) |}];
  ()
;;

let%expect_test "E55.sexp_of_t" =
  let t = force E55.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         146.98667170969784)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         98.204916637702212)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         65.612790191382629)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             55)
          (number_of_divisions 32))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             55)
          (number_of_divisions 32))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Equal_division_of_the_octave
          (divisor             55)
          (number_of_divisions 32))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 4))))
       ((name      m2-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 5))))
       ((name      M2-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 9))))
       ((name      d3-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 10))))
       ((name      A2-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 13))))
       ((name      m3-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 14))))
       ((name      M3-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 18))))
       ((name      d4-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 19))))
       ((name      A3-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 22))))
       ((name      4-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 23))))
       ((name      A4-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 27))))
       ((name      d5-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 28))))
       ((name      5-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 32))))
       ((name      A5-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 36))))
       ((name      m6-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 37))))
       ((name      M6-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 41))))
       ((name      d7-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 42))))
       ((name      m7-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 46))))
       ((name      M7-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 50))))
       ((name      d8-e55)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 51))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 4))))
       ((name      m2-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 5))))
       ((name      M2-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 9))))
       ((name      d3-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 10))))
       ((name      A2-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 13))))
       ((name      m3-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 14))))
       ((name      M3-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 18))))
       ((name      d4-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 19))))
       ((name      A3-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 22))))
       ((name      4-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 23))))
       ((name      A4-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 27))))
       ((name      d5-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 28))))
       ((name      5-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 32))))
       ((name      A5-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 36))))
       ((name      m6-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 37))))
       ((name      M6-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 41))))
       ((name      d7-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 42))))
       ((name      m7-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 46))))
       ((name      M7-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 50))))
       ((name      d8-e55)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 51))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      A1-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 4))))
       ((name      m2-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 5))))
       ((name      M2-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 9))))
       ((name      d3-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 10))))
       ((name      A2-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 13))))
       ((name      m3-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 14))))
       ((name      M3-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 18))))
       ((name      d4-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 19))))
       ((name      A3-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 22))))
       ((name      4-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 23))))
       ((name      A4-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 27))))
       ((name      d5-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 28))))
       ((name      5-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 32))))
       ((name      A5-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 36))))
       ((name      m6-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 37))))
       ((name      M6-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 41))))
       ((name      d7-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 42))))
       ((name      m7-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 46))))
       ((name      M7-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 50))))
       ((name      d8-e55)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave
           (divisor             55)
           (number_of_divisions 51))))))) |}];
  ()
;;

let%expect_test "Just.sexp_of_t" =
  let t = force Just.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         146.66666666666666)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         97.777777777777771)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         65.185185185185176)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Reduced_natural_ratio (
            ((prime 2) (exponent -1))
            ((prime 3) (exponent 1))))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Reduced_natural_ratio (
            ((prime 2) (exponent -1))
            ((prime 3) (exponent 1))))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Reduced_natural_ratio (
            ((prime 2) (exponent -1))
            ((prime 3) (exponent 1))))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -5))))))
       ((name      A1z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 3))
           ((prime 5) (exponent 1))))))
       ((name      m2z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -1))
           ((prime 5) (exponent -1))))))
       ((name      A1p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -11))
           ((prime 3) (exponent 7))))))
       ((name      d3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 16))
           ((prime 3) (exponent -10))))))
       ((name      M2z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 1))
           ((prime 3) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name      d3z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -6))
           ((prime 5) (exponent -1))))))
       ((name      M2p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 2))))))
       ((name      m3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -3))))))
       ((name      m3z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 1))
           ((prime 3) (exponent 1))
           ((prime 5) (exponent -1))))))
       ((name      A2p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -14))
           ((prime 3) (exponent 9))))))
       ((name      d4p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 13))
           ((prime 3) (exponent -8))))))
       ((name      M3z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name      d4z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 9))
           ((prime 3) (exponent -4))
           ((prime 5) (exponent -1))))))
       ((name      M3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -6))
           ((prime 3) (exponent 4))))))
       ((name      4p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 2))
           ((prime 3) (exponent -1))))))
       ((name      4z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -2))
           ((prime 3) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name      A3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -17))
           ((prime 3) (exponent 11))))))
       ((name      d5p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 10))
           ((prime 3) (exponent -6))))))
       ((name      A4z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -5))
           ((prime 3) (exponent 2))
           ((prime 5) (exponent 1))))))
       ((name      d5z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 6))
           ((prime 3) (exponent -2))
           ((prime 5) (exponent -1))))))
       ((name      A4p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -9))
           ((prime 3) (exponent 6))))))
       ((name      d6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 18))
           ((prime 3) (exponent -11))))))
       ((name      5z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 3))
           ((prime 3) (exponent -3))
           ((prime 5) (exponent 1))))))
       ((name      5p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -1))
           ((prime 3) (exponent 1))))))
       ((name      m6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 7))
           ((prime 3) (exponent -4))))))
       ((name      A5z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -8))
           ((prime 3) (exponent 4))
           ((prime 5) (exponent 1))))))
       ((name      m6z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name      A5p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -12))
           ((prime 3) (exponent 8))))))
       ((name      d7p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 15))
           ((prime 3) (exponent -9))))))
       ((name      M6z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 3) (exponent -1))
           ((prime 5) (exponent 1))))))
       ((name      d7z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 11))
           ((prime 3) (exponent -5))
           ((prime 5) (exponent -1))))))
       ((name      M6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -4))
           ((prime 3) (exponent 3))))))
       ((name      m7p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -2))))))
       ((name      A6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -15))
           ((prime 3) (exponent 10))))))
       ((name      d8p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -7))))))
       ((name      M7z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 1))
           ((prime 5) (exponent 1))))))
       ((name      d8z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -3))
           ((prime 5) (exponent -1))))))
       ((name      M7p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 5))))))
       ((name      8z)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -4))
           ((prime 5) (exponent 1))))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -5))))))
       ((name      A1z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 3))
           ((prime 5) (exponent 1))))))
       ((name      m2z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -1))
           ((prime 5) (exponent -1))))))
       ((name      A1p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -11))
           ((prime 3) (exponent 7))))))
       ((name      d3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 16))
           ((prime 3) (exponent -10))))))
       ((name      M2z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 1))
           ((prime 3) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name      d3z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -6))
           ((prime 5) (exponent -1))))))
       ((name      M2p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 2))))))
       ((name      m3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -3))))))
       ((name      m3z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 1))
           ((prime 3) (exponent 1))
           ((prime 5) (exponent -1))))))
       ((name      A2p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -14))
           ((prime 3) (exponent 9))))))
       ((name      d4p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 13))
           ((prime 3) (exponent -8))))))
       ((name      M3z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name      d4z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 9))
           ((prime 3) (exponent -4))
           ((prime 5) (exponent -1))))))
       ((name      M3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -6))
           ((prime 3) (exponent 4))))))
       ((name      4p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 2))
           ((prime 3) (exponent -1))))))
       ((name      4z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -2))
           ((prime 3) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name      A3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -17))
           ((prime 3) (exponent 11))))))
       ((name      d5p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 10))
           ((prime 3) (exponent -6))))))
       ((name      A4z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -5))
           ((prime 3) (exponent 2))
           ((prime 5) (exponent 1))))))
       ((name      d5z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 6))
           ((prime 3) (exponent -2))
           ((prime 5) (exponent -1))))))
       ((name      A4p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -9))
           ((prime 3) (exponent 6))))))
       ((name      d6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 18))
           ((prime 3) (exponent -11))))))
       ((name      5z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 3))
           ((prime 3) (exponent -3))
           ((prime 5) (exponent 1))))))
       ((name      5p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -1))
           ((prime 3) (exponent 1))))))
       ((name      m6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 7))
           ((prime 3) (exponent -4))))))
       ((name      A5z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -8))
           ((prime 3) (exponent 4))
           ((prime 5) (exponent 1))))))
       ((name      m6z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name      A5p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -12))
           ((prime 3) (exponent 8))))))
       ((name      d7p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 15))
           ((prime 3) (exponent -9))))))
       ((name      M6z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 3) (exponent -1))
           ((prime 5) (exponent 1))))))
       ((name      d7z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 11))
           ((prime 3) (exponent -5))
           ((prime 5) (exponent -1))))))
       ((name      M6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -4))
           ((prime 3) (exponent 3))))))
       ((name      m7p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -2))))))
       ((name      A6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -15))
           ((prime 3) (exponent 10))))))
       ((name      d8p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -7))))))
       ((name      M7z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 1))
           ((prime 5) (exponent 1))))))
       ((name      d8z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -3))
           ((prime 5) (exponent -1))))))
       ((name      M7p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 5))))))
       ((name      8z)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -4))
           ((prime 5) (exponent 1))))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -5))))))
       ((name      A1z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 3))
           ((prime 5) (exponent 1))))))
       ((name      m2z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -1))
           ((prime 5) (exponent -1))))))
       ((name      A1p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -11))
           ((prime 3) (exponent 7))))))
       ((name      d3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 16))
           ((prime 3) (exponent -10))))))
       ((name      M2z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 1))
           ((prime 3) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name      d3z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -6))
           ((prime 5) (exponent -1))))))
       ((name      M2p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 2))))))
       ((name      m3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -3))))))
       ((name      m3z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 1))
           ((prime 3) (exponent 1))
           ((prime 5) (exponent -1))))))
       ((name      A2p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -14))
           ((prime 3) (exponent 9))))))
       ((name      d4p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 13))
           ((prime 3) (exponent -8))))))
       ((name      M3z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name      d4z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 9))
           ((prime 3) (exponent -4))
           ((prime 5) (exponent -1))))))
       ((name      M3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -6))
           ((prime 3) (exponent 4))))))
       ((name      4p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 2))
           ((prime 3) (exponent -1))))))
       ((name      4z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -2))
           ((prime 3) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name      A3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -17))
           ((prime 3) (exponent 11))))))
       ((name      d5p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 10))
           ((prime 3) (exponent -6))))))
       ((name      A4z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -5))
           ((prime 3) (exponent 2))
           ((prime 5) (exponent 1))))))
       ((name      d5z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 6))
           ((prime 3) (exponent -2))
           ((prime 5) (exponent -1))))))
       ((name      A4p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -9))
           ((prime 3) (exponent 6))))))
       ((name      d6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 18))
           ((prime 3) (exponent -11))))))
       ((name      5z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 3))
           ((prime 3) (exponent -3))
           ((prime 5) (exponent 1))))))
       ((name      5p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -1))
           ((prime 3) (exponent 1))))))
       ((name      m6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 7))
           ((prime 3) (exponent -4))))))
       ((name      A5z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -8))
           ((prime 3) (exponent 4))
           ((prime 5) (exponent 1))))))
       ((name      m6z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name      A5p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -12))
           ((prime 3) (exponent 8))))))
       ((name      d7p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 15))
           ((prime 3) (exponent -9))))))
       ((name      M6z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 3) (exponent -1))
           ((prime 5) (exponent 1))))))
       ((name      d7z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 11))
           ((prime 3) (exponent -5))
           ((prime 5) (exponent -1))))))
       ((name      M6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -4))
           ((prime 3) (exponent 3))))))
       ((name      m7p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -2))))))
       ((name      A6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -15))
           ((prime 3) (exponent 10))))))
       ((name      d8p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -7))))))
       ((name      M7z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 1))
           ((prime 5) (exponent 1))))))
       ((name      d8z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -3))
           ((prime 5) (exponent -1))))))
       ((name      M7p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 5))))))
       ((name      8z)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -4))
           ((prime 5) (exponent 1))))))))) |}];
  ()
;;

let%expect_test "Pythagorean.sexp_of_t" =
  let t = force Pythagorean.t in
  print_s [%sexp (t : System.t)];
  [%expect
    {|
    ((vibrating_strings (
       ((open_string (
          (letter_name        A)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         220)
        (roman_numeral I))
       ((open_string (
          (letter_name        D)
          (symbol             Natural)
          (octave_designation 3)))
        (pitch         146.66666666666666)
        (roman_numeral II))
       ((open_string (
          (letter_name        G)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         97.777777777777771)
        (roman_numeral III))
       ((open_string (
          (letter_name        C)
          (symbol             Natural)
          (octave_designation 2)))
        (pitch         65.185185185185176)
        (roman_numeral IV))))
     (intervals_going_down (
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Reduced_natural_ratio (
            ((prime 2) (exponent -1))
            ((prime 3) (exponent 1))))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Reduced_natural_ratio (
            ((prime 2) (exponent -1))
            ((prime 3) (exponent 1))))))
       ((interval (
          (number             Fifth)
          (quality            Perfect)
          (additional_octaves 0)))
        (acoustic_interval (
          Reduced_natural_ratio (
            ((prime 2) (exponent -1))
            ((prime 3) (exponent 1))))))))
     (fingerboard_positions (
       ((name                                       0)
        (at_octave                                  0)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -5))))))
       ((name      A1p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -11))
           ((prime 3) (exponent 7))))))
       ((name      d3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 16))
           ((prime 3) (exponent -10))))))
       ((name      M2p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 2))))))
       ((name      m3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -3))))))
       ((name      A2p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -14))
           ((prime 3) (exponent 9))))))
       ((name      d4p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 13))
           ((prime 3) (exponent -8))))))
       ((name      M3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -6))
           ((prime 3) (exponent 4))))))
       ((name      4p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 2))
           ((prime 3) (exponent -1))))))
       ((name      A3p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -17))
           ((prime 3) (exponent 11))))))
       ((name      d5p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 10))
           ((prime 3) (exponent -6))))))
       ((name      A4p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -9))
           ((prime 3) (exponent 6))))))
       ((name      d6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 18))
           ((prime 3) (exponent -11))))))
       ((name      5p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -1))
           ((prime 3) (exponent 1))))))
       ((name      m6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 7))
           ((prime 3) (exponent -4))))))
       ((name      A5p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -12))
           ((prime 3) (exponent 8))))))
       ((name      d7p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 15))
           ((prime 3) (exponent -9))))))
       ((name      M6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -4))
           ((prime 3) (exponent 3))))))
       ((name      m7p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -2))))))
       ((name      A6p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -15))
           ((prime 3) (exponent 10))))))
       ((name      d8p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -7))))))
       ((name      M7p)
        (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 5))))))
       ((name                                       0)
        (at_octave                                  1)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -5))))))
       ((name      A1p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -11))
           ((prime 3) (exponent 7))))))
       ((name      d3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 16))
           ((prime 3) (exponent -10))))))
       ((name      M2p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 2))))))
       ((name      m3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -3))))))
       ((name      A2p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -14))
           ((prime 3) (exponent 9))))))
       ((name      d4p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 13))
           ((prime 3) (exponent -8))))))
       ((name      M3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -6))
           ((prime 3) (exponent 4))))))
       ((name      4p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 2))
           ((prime 3) (exponent -1))))))
       ((name      A3p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -17))
           ((prime 3) (exponent 11))))))
       ((name      d5p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 10))
           ((prime 3) (exponent -6))))))
       ((name      A4p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -9))
           ((prime 3) (exponent 6))))))
       ((name      d6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 18))
           ((prime 3) (exponent -11))))))
       ((name      5p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -1))
           ((prime 3) (exponent 1))))))
       ((name      m6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 7))
           ((prime 3) (exponent -4))))))
       ((name      A5p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -12))
           ((prime 3) (exponent 8))))))
       ((name      d7p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 15))
           ((prime 3) (exponent -9))))))
       ((name      M6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -4))
           ((prime 3) (exponent 3))))))
       ((name      m7p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -2))))))
       ((name      A6p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -15))
           ((prime 3) (exponent 10))))))
       ((name      d8p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -7))))))
       ((name      M7p)
        (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 5))))))
       ((name                                       0)
        (at_octave                                  2)
        (basis_acoustic_interval_to_the_open_string Zero))
       ((name      m2p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 8))
           ((prime 3) (exponent -5))))))
       ((name      A1p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -11))
           ((prime 3) (exponent 7))))))
       ((name      d3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 16))
           ((prime 3) (exponent -10))))))
       ((name      M2p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -3))
           ((prime 3) (exponent 2))))))
       ((name      m3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 5))
           ((prime 3) (exponent -3))))))
       ((name      A2p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -14))
           ((prime 3) (exponent 9))))))
       ((name      d4p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 13))
           ((prime 3) (exponent -8))))))
       ((name      M3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -6))
           ((prime 3) (exponent 4))))))
       ((name      4p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 2))
           ((prime 3) (exponent -1))))))
       ((name      A3p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -17))
           ((prime 3) (exponent 11))))))
       ((name      d5p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 10))
           ((prime 3) (exponent -6))))))
       ((name      A4p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -9))
           ((prime 3) (exponent 6))))))
       ((name      d6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 18))
           ((prime 3) (exponent -11))))))
       ((name      5p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -1))
           ((prime 3) (exponent 1))))))
       ((name      m6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 7))
           ((prime 3) (exponent -4))))))
       ((name      A5p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -12))
           ((prime 3) (exponent 8))))))
       ((name      d7p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 15))
           ((prime 3) (exponent -9))))))
       ((name      M6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -4))
           ((prime 3) (exponent 3))))))
       ((name      m7p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 4))
           ((prime 3) (exponent -2))))))
       ((name      A6p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -15))
           ((prime 3) (exponent 10))))))
       ((name      d8p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent 12))
           ((prime 3) (exponent -7))))))
       ((name      M7p)
        (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio (
           ((prime 2) (exponent -7))
           ((prime 3) (exponent 5))))))))) |}];
  ()
;;

module type POSITIONS = sig
  type t [@@deriving enumerate, sexp_of]

  val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
end

let check_order_exn (module P : POSITIONS) =
  let acoustic_interval =
    List.fold_left P.all ~init:Acoustic_interval.unison ~f:(fun acc p ->
      let acoustic_interval = P.acoustic_interval_to_the_open_string p in
      match Acoustic_interval.compare acc acoustic_interval |> Ordering.of_int with
      | Less -> acoustic_interval
      | Equal | Greater ->
        raise_s
          [%sexp
            "Unexpected position order"
          , { previous_acoustic_interval = (acc : Acoustic_interval.t)
            ; p : P.t
            ; acoustic_interval : Acoustic_interval.t
            }])
  in
  ignore (acoustic_interval : Acoustic_interval.t)
;;

let%expect_test "Edo12.order" =
  check_order_exn (module Cello.Fingerboard_position_name.Edo12)
;;

let%expect_test "Edo31.order" =
  check_order_exn (module Cello.Fingerboard_position_name.Edo31)
;;

let%expect_test "Edo53.order" =
  check_order_exn (module Cello.Fingerboard_position_name.Edo53)
;;

let%expect_test "Edo55.order" =
  check_order_exn (module Cello.Fingerboard_position_name.Edo55)
;;

let%expect_test "Pythagorean.order" =
  check_order_exn (module Cello.Fingerboard_position_name.Pythagorean)
;;

let%expect_test "Just.order" =
  check_order_exn (module Cello.Fingerboard_position_name.Just)
;;
