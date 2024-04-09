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

let adjustment =
  { System.Double_stops.Adjustment.from =
      Acoustic_interval.pythagorean
        { number = Sixth; quality = Major; additional_octaves = 0 }
  ; to_ = Acoustic_interval.just_major_sixth
  }
;;

let make_scale ~characterized_scale ~from ~adjustment =
  let t = force Just.t in
  System.Double_stops.make_scale
    t
    ?adjustment
    ~characterized_scale
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_pythagorean_scale ~from =
  make_scale
    ~characterized_scale:Characterized_scale.major_pythagorean
    ~from
    ~adjustment:None
;;

let make_major_just_scale ~from =
  make_scale
    ~characterized_scale:Characterized_scale.major_just
    ~from
    ~adjustment:(Some adjustment)
;;

let%expect_test "c_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ C2  │ IV     │ 0     │     0 │ A2   │ III    │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ D2  │ IV     │ M2p   │   204 │ B2   │ III    │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ E2  │ IV     │ M3z   │   386 │ C3   │ III    │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ F2  │ IV     │ 4z    │   520 │ D3   │ III    │ 5p    │   702 │ M6 - 5 / 3   │   884 │
    │ G2  │ III    │ 0     │     0 │ E3   │ II     │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ A2  │ III    │ M2z   │   182 │ F3   │ II     │ m3p   │   294 │ m6 - 2^3 / 5 │   814 │
    │ B2  │ III    │ M3z   │   386 │ G3   │ II     │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ C3  │ III    │ 4p    │   498 │ A3   │ II     │ 5z    │   680 │ M6 - 5 / 3   │   884 │
    │ D3  │ II     │ 0     │     0 │ B3   │ I      │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ E3  │ II     │ M2z   │   182 │ C4   │ I      │ m3p   │   294 │ m6 - 2^3 / 5 │   814 │
    │ F3  │ II     │ m3z   │   316 │ D4   │ I      │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ G3  │ II     │ 4p    │   498 │ E4   │ I      │ 5z    │   680 │ M6 - 5 / 3   │   884 │
    │ A3  │ II     │ 5z    │   680 │ F4   │ I      │ m6p   │   792 │ m6 - 2^3 / 5 │   814 │
    │ B3  │ II     │ M6z   │   884 │ G4   │ I      │ m7p   │   996 │ m6 - 2^3 / 5 │   814 │
    │ C4  │ II     │ m7p   │   996 │ A4   │ I      │ 8z    │  1178 │ M6 - 5 / 3   │   884 │
    │ D4  │ II     │ 0-1   │  1200 │ B4   │ I      │ M2z-1 │  1382 │ M6 - 5 / 3   │   884 │
    │ E4  │ II     │ M2z-1 │  1382 │ C5   │ I      │ m3p-1 │  1494 │ m6 - 2^3 / 5 │   814 │
    │ F4  │ II     │ m3z-1 │  1516 │ D5   │ I      │ 4p-1  │  1698 │ M6 - 5 / 3   │   884 │
    │ G4  │ II     │ 4p-1  │  1698 │ E5   │ I      │ 5z-1  │  1880 │ M6 - 5 / 3   │   884 │
    │ A4  │ II     │ 5z-1  │  1880 │ F5   │ I      │ m6p-1 │  1992 │ m6 - 2^3 / 5 │   814 │
    │ B4  │ II     │ M6z-1 │  2084 │ G5   │ I      │ m7p-1 │  2196 │ m6 - 2^3 / 5 │   814 │
    │ C5  │ II     │ m7p-1 │  2196 │ A5   │ I      │ 8z-1  │  2378 │ M6 - 5 / 3   │   884 │
    │ D5  │ II     │ 0-2   │  2400 │ B5   │ I      │ M2z-2 │  2582 │ M6 - 5 / 3   │   884 │
    │ E5  │ II     │ M2z-2 │  2582 │ C6   │ I      │ m3p-2 │  2694 │ m6 - 2^3 / 5 │   814 │
    │ F5  │ II     │ m3z-2 │  2716 │ D6   │ I      │ 4p-2  │  2898 │ M6 - 5 / 3   │   884 │
    │ G5  │ II     │ 4p-2  │  2898 │ E6   │ I      │ 5z-2  │  3080 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

(* Since [Just] is a superset of [Pythagorean], it supports all of the
   same pythagorean scales. We only test one here, since they all
   already appear in [pythagorean/sixths.ml]. *)
let%expect_test "c_major_pythagorean" =
  let t = force Just.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval       │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼────────────────┼───────┤
    │ C2  │ IV     │ 0     │     0 │ A2   │ III    │ M2p   │   204 │ M6 - 3^3 / 2^4 │   906 │
    │ D2  │ IV     │ M2p   │   204 │ B2   │ III    │ M3p   │   408 │ M6 - 3^3 / 2^4 │   906 │
    │ E2  │ IV     │ M3p   │   408 │ C3   │ III    │ 4p    │   498 │ m6 - 2^7 / 3^4 │   792 │
    │ F2  │ IV     │ 4p    │   498 │ D3   │ III    │ 5p    │   702 │ M6 - 3^3 / 2^4 │   906 │
    │ G2  │ III    │ 0     │     0 │ E3   │ II     │ M2p   │   204 │ M6 - 3^3 / 2^4 │   906 │
    │ A2  │ III    │ M2p   │   204 │ F3   │ II     │ m3p   │   294 │ m6 - 2^7 / 3^4 │   792 │
    │ B2  │ III    │ M3p   │   408 │ G3   │ II     │ 4p    │   498 │ m6 - 2^7 / 3^4 │   792 │
    │ C3  │ III    │ 4p    │   498 │ A3   │ II     │ 5p    │   702 │ M6 - 3^3 / 2^4 │   906 │
    │ D3  │ II     │ 0     │     0 │ B3   │ I      │ M2p   │   204 │ M6 - 3^3 / 2^4 │   906 │
    │ E3  │ II     │ M2p   │   204 │ C4   │ I      │ m3p   │   294 │ m6 - 2^7 / 3^4 │   792 │
    │ F3  │ II     │ m3p   │   294 │ D4   │ I      │ 4p    │   498 │ M6 - 3^3 / 2^4 │   906 │
    │ G3  │ II     │ 4p    │   498 │ E4   │ I      │ 5p    │   702 │ M6 - 3^3 / 2^4 │   906 │
    │ A3  │ II     │ 5p    │   702 │ F4   │ I      │ m6p   │   792 │ m6 - 2^7 / 3^4 │   792 │
    │ B3  │ II     │ M6p   │   906 │ G4   │ I      │ m7p   │   996 │ m6 - 2^7 / 3^4 │   792 │
    │ C4  │ II     │ m7p   │   996 │ A4   │ I      │ 0-1   │  1200 │ M6 - 3^3 / 2^4 │   906 │
    │ D4  │ II     │ 0-1   │  1200 │ B4   │ I      │ M2p-1 │  1404 │ M6 - 3^3 / 2^4 │   906 │
    │ E4  │ II     │ M2p-1 │  1404 │ C5   │ I      │ m3p-1 │  1494 │ m6 - 2^7 / 3^4 │   792 │
    │ F4  │ II     │ m3p-1 │  1494 │ D5   │ I      │ 4p-1  │  1698 │ M6 - 3^3 / 2^4 │   906 │
    │ G4  │ II     │ 4p-1  │  1698 │ E5   │ I      │ 5p-1  │  1902 │ M6 - 3^3 / 2^4 │   906 │
    │ A4  │ II     │ 5p-1  │  1902 │ F5   │ I      │ m6p-1 │  1992 │ m6 - 2^7 / 3^4 │   792 │
    │ B4  │ II     │ M6p-1 │  2106 │ G5   │ I      │ m7p-1 │  2196 │ m6 - 2^7 / 3^4 │   792 │
    │ C5  │ II     │ m7p-1 │  2196 │ A5   │ I      │ 0-2   │  2400 │ M6 - 3^3 / 2^4 │   906 │
    │ D5  │ II     │ 0-2   │  2400 │ B5   │ I      │ M2p-2 │  2604 │ M6 - 3^3 / 2^4 │   906 │
    │ E5  │ II     │ M2p-2 │  2604 │ C6   │ I      │ m3p-2 │  2694 │ m6 - 2^7 / 3^4 │   792 │
    │ F5  │ II     │ m3p-2 │  2694 │ D6   │ I      │ 4p-2  │  2898 │ M6 - 3^3 / 2^4 │   906 │
    │ G5  │ II     │ 4p-2  │  2898 │ E6   │ I      │ 5p-2  │  3102 │ M6 - 3^3 / 2^4 │   906 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴────────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_dz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ Db2 │ IV     │ m2z   │   112 │ Bb2  │ III    │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ Eb2 │ IV     │ m3z   │   316 │ C3   │ III    │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ F2  │ IV     │ 4p    │   498 │ Db3  │ III    │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ Gb2 │ IV     │ d5z   │   610 │ Eb3  │ III    │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Ab2 │ III    │ m2z   │   112 │ F3   │ II     │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ Bb2 │ III    │ m3p   │   294 │ Gb3  │ II     │ d4z   │   406 │ m6 - 2^3 / 5 │   814 │
    │ C3  │ III    │ 4p    │   498 │ Ab3  │ II     │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ Db3 │ III    │ d5z   │   610 │ Bb3  │ II     │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Eb3 │ II     │ m2z   │   112 │ C4   │ I      │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ F3  │ II     │ m3p   │   294 │ Db4  │ I      │ d4z   │   406 │ m6 - 2^3 / 5 │   814 │
    │ Gb3 │ II     │ d4z   │   406 │ Eb4  │ I      │ d5p   │   588 │ M6 - 5 / 3   │   884 │
    │ Ab3 │ II     │ d5z   │   610 │ F4   │ I      │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Bb3 │ II     │ m6p   │   792 │ Gb4  │ I      │ d7z   │   904 │ m6 - 2^3 / 5 │   814 │
    │ C4  │ II     │ m7p   │   996 │ Ab4  │ I      │ d8z   │  1108 │ m6 - 2^3 / 5 │   814 │
    │ Db4 │ II     │ d8z   │  1108 │ Bb4  │ I      │ m2p-1 │  1290 │ M6 - 5 / 3   │   884 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ C5   │ I      │ m3p-1 │  1494 │ M6 - 5 / 3   │   884 │
    │ F4  │ II     │ m3p-1 │  1494 │ Db5  │ I      │ d4z-1 │  1606 │ m6 - 2^3 / 5 │   814 │
    │ Gb4 │ II     │ d4z-1 │  1606 │ Eb5  │ I      │ d5p-1 │  1788 │ M6 - 5 / 3   │   884 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ F5   │ I      │ m6p-1 │  1992 │ M6 - 5 / 3   │   884 │
    │ Bb4 │ II     │ m6p-1 │  1992 │ Gb5  │ I      │ d7z-1 │  2104 │ m6 - 2^3 / 5 │   814 │
    │ C5  │ II     │ m7p-1 │  2196 │ Ab5  │ I      │ d8z-1 │  2308 │ m6 - 2^3 / 5 │   814 │
    │ Db5 │ II     │ d8z-1 │  2308 │ Bb5  │ I      │ m2p-2 │  2490 │ M6 - 5 / 3   │   884 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ C6   │ I      │ m3p-2 │  2694 │ M6 - 5 / 3   │   884 │
    │ F5  │ II     │ m3p-2 │  2694 │ Db6  │ I      │ d4z-2 │  2806 │ m6 - 2^3 / 5 │   814 │
    │ Gb5 │ II     │ d4z-2 │  2806 │ Eb6  │ I      │ d5p-2 │  2988 │ M6 - 5 / 3   │   884 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ F6   │ I      │ m6p-2 │  3192 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "d_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ D2  │ IV     │ M2p   │   204 │ B2   │ III    │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ E2  │ IV     │ M3p   │   408 │ C#3  │ III    │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ F#2 │ IV     │ A4z   │   590 │ D3   │ III    │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ G2  │ III    │ 0     │     0 │ E3   │ II     │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ A2  │ III    │ M2p   │   204 │ F#3  │ II     │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ B2  │ III    │ M3z   │   386 │ G3   │ II     │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ C#3 │ III    │ A4z   │   590 │ A3   │ II     │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ D3  │ II     │ 0     │     0 │ B3   │ I      │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ E3  │ II     │ M2p   │   204 │ C#4  │ I      │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ F#3 │ II     │ M3z   │   386 │ D4   │ I      │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ G3  │ II     │ 4p    │   498 │ E4   │ I      │ 5z    │   680 │ M6 - 5 / 3   │   884 │
    │ A3  │ II     │ 5p    │   702 │ F#4  │ I      │ M6z   │   884 │ M6 - 5 / 3   │   884 │
    │ B3  │ II     │ M6z   │   884 │ G4   │ I      │ m7p   │   996 │ m6 - 2^3 / 5 │   814 │
    │ C#4 │ II     │ M7z   │  1088 │ A4   │ I      │ 0-1   │  1200 │ m6 - 2^3 / 5 │   814 │
    │ D4  │ II     │ 0-1   │  1200 │ B4   │ I      │ M2z-1 │  1382 │ M6 - 5 / 3   │   884 │
    │ E4  │ II     │ M2p-1 │  1404 │ C#5  │ I      │ M3z-1 │  1586 │ M6 - 5 / 3   │   884 │
    │ F#4 │ II     │ M3z-1 │  1586 │ D5   │ I      │ 4p-1  │  1698 │ m6 - 2^3 / 5 │   814 │
    │ G4  │ II     │ 4p-1  │  1698 │ E5   │ I      │ 5z-1  │  1880 │ M6 - 5 / 3   │   884 │
    │ A4  │ II     │ 5p-1  │  1902 │ F#5  │ I      │ M6z-1 │  2084 │ M6 - 5 / 3   │   884 │
    │ B4  │ II     │ M6z-1 │  2084 │ G5   │ I      │ m7p-1 │  2196 │ m6 - 2^3 / 5 │   814 │
    │ C#5 │ II     │ M7z-1 │  2288 │ A5   │ I      │ 0-2   │  2400 │ m6 - 2^3 / 5 │   814 │
    │ D5  │ II     │ 0-2   │  2400 │ B5   │ I      │ M2z-2 │  2582 │ M6 - 5 / 3   │   884 │
    │ E5  │ II     │ M2p-2 │  2604 │ C#6  │ I      │ M3z-2 │  2786 │ M6 - 5 / 3   │   884 │
    │ F#5 │ II     │ M3z-2 │  2786 │ D6   │ I      │ 4p-2  │  2898 │ m6 - 2^3 / 5 │   814 │
    │ G5  │ II     │ 4p-2  │  2898 │ E6   │ I      │ 5z-2  │  3080 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_ez_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ Eb2 │ IV     │ m3z   │   316 │ C3   │ III    │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ F2  │ IV     │ 4z    │   520 │ D3   │ III    │ 5p    │   702 │ M6 - 5 / 3   │   884 │
    │ G2  │ III    │ 0     │     0 │ Eb3  │ II     │ m2z   │   112 │ m6 - 2^3 / 5 │   814 │
    │ Ab2 │ III    │ m2z   │   112 │ F3   │ II     │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ Bb2 │ III    │ m3z   │   316 │ G3   │ II     │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ C3  │ III    │ 4p    │   498 │ Ab3  │ II     │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ D3  │ II     │ 0     │     0 │ Bb3  │ I      │ m2z   │   112 │ m6 - 2^3 / 5 │   814 │
    │ Eb3 │ II     │ m2z   │   112 │ C4   │ I      │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ F3  │ II     │ m3z   │   316 │ D4   │ I      │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ G3  │ II     │ 4p    │   498 │ Eb4  │ I      │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ Ab3 │ II     │ d5z   │   610 │ F4   │ I      │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Bb3 │ II     │ m6z   │   814 │ G4   │ I      │ m7p   │   996 │ M6 - 5 / 3   │   884 │
    │ C4  │ II     │ m7p   │   996 │ Ab4  │ I      │ d8z   │  1108 │ m6 - 2^3 / 5 │   814 │
    │ D4  │ II     │ 0-1   │  1200 │ Bb4  │ I      │ m2z-1 │  1312 │ m6 - 2^3 / 5 │   814 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ C5   │ I      │ m3p-1 │  1494 │ M6 - 5 / 3   │   884 │
    │ F4  │ II     │ m3z-1 │  1516 │ D5   │ I      │ 4p-1  │  1698 │ M6 - 5 / 3   │   884 │
    │ G4  │ II     │ 4p-1  │  1698 │ Eb5  │ I      │ d5z-1 │  1810 │ m6 - 2^3 / 5 │   814 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ F5   │ I      │ m6p-1 │  1992 │ M6 - 5 / 3   │   884 │
    │ Bb4 │ II     │ m6z-1 │  2014 │ G5   │ I      │ m7p-1 │  2196 │ M6 - 5 / 3   │   884 │
    │ C5  │ II     │ m7p-1 │  2196 │ Ab5  │ I      │ d8z-1 │  2308 │ m6 - 2^3 / 5 │   814 │
    │ D5  │ II     │ 0-2   │  2400 │ Bb5  │ I      │ m2z-2 │  2512 │ m6 - 2^3 / 5 │   814 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ C6   │ I      │ m3p-2 │  2694 │ M6 - 5 / 3   │   884 │
    │ F5  │ II     │ m3z-2 │  2716 │ D6   │ I      │ 4p-2  │  2898 │ M6 - 5 / 3   │   884 │
    │ G5  │ II     │ 4p-2  │  2898 │ Eb6  │ I      │ d5z-2 │  3010 │ m6 - 2^3 / 5 │   814 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ F6   │ I      │ m6p-2 │  3192 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "e_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ E2  │ IV     │ M3p   │   408 │ C#3  │ III    │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ F#2 │ IV     │ A4p   │   612 │ D#3  │ III    │ A5z   │   794 │ M6 - 5 / 3   │   884 │
    │ G#2 │ III    │ A1z   │    92 │ E3   │ II     │ M2p   │   204 │ m6 - 2^3 / 5 │   814 │
    │ A2  │ III    │ M2p   │   204 │ F#3  │ II     │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ B2  │ III    │ M3p   │   408 │ G#3  │ II     │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ C#3 │ III    │ A4z   │   590 │ A3   │ II     │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ D#3 │ II     │ A1z   │    92 │ B3   │ I      │ M2p   │   204 │ m6 - 2^3 / 5 │   814 │
    │ E3  │ II     │ M2p   │   204 │ C#4  │ I      │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ F#3 │ II     │ M3p   │   408 │ D#4  │ I      │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ G#3 │ II     │ A4z   │   590 │ E4   │ I      │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ A3  │ II     │ 5p    │   702 │ F#4  │ I      │ M6z   │   884 │ M6 - 5 / 3   │   884 │
    │ B3  │ II     │ M6p   │   906 │ G#4  │ I      │ M7z   │  1088 │ M6 - 5 / 3   │   884 │
    │ C#4 │ II     │ M7z   │  1088 │ A4   │ I      │ 0-1   │  1200 │ m6 - 2^3 / 5 │   814 │
    │ D#4 │ II     │ A1z-1 │  1292 │ B4   │ I      │ M2p-1 │  1404 │ m6 - 2^3 / 5 │   814 │
    │ E4  │ II     │ M2p-1 │  1404 │ C#5  │ I      │ M3z-1 │  1586 │ M6 - 5 / 3   │   884 │
    │ F#4 │ II     │ M3p-1 │  1608 │ D#5  │ I      │ A4z-1 │  1790 │ M6 - 5 / 3   │   884 │
    │ G#4 │ II     │ A4z-1 │  1790 │ E5   │ I      │ 5p-1  │  1902 │ m6 - 2^3 / 5 │   814 │
    │ A4  │ II     │ 5p-1  │  1902 │ F#5  │ I      │ M6z-1 │  2084 │ M6 - 5 / 3   │   884 │
    │ B4  │ II     │ M6p-1 │  2106 │ G#5  │ I      │ M7z-1 │  2288 │ M6 - 5 / 3   │   884 │
    │ C#5 │ II     │ M7z-1 │  2288 │ A5   │ I      │ 0-2   │  2400 │ m6 - 2^3 / 5 │   814 │
    │ D#5 │ II     │ A1z-2 │  2492 │ B5   │ I      │ M2p-2 │  2604 │ m6 - 2^3 / 5 │   814 │
    │ E5  │ II     │ M2p-2 │  2604 │ C#6  │ I      │ M3z-2 │  2786 │ M6 - 5 / 3   │   884 │
    │ F#5 │ II     │ M3p-2 │  2808 │ D#6  │ I      │ A4z-2 │  2990 │ M6 - 5 / 3   │   884 │
    │ G#5 │ II     │ A4z-2 │  2990 │ E6   │ I      │ 5p-2  │  3102 │ m6 - 2^3 / 5 │   814 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "fp_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────┬───────┬──────┬────────┬─────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos │ Cents │ High │ String │ Pos │ Cents │ Interval     │ Cents │
    ├─────┼────────┼─────┼───────┼──────┼────────┼─────┼───────┼──────────────┼───────┤
    │ F2  │ IV     │ 4p  │   498 │ D3   │ III    │ 5z  │   680 │ M6 - 5 / 3   │   884 │
    │ G2  │ III    │ 0   │     0 │ E3   │ II     │ M2z │   182 │ M6 - 5 / 3   │   884 │
    │ A2  │ III    │ M2z │   182 │ F3   │ II     │ m3p │   294 │ m6 - 2^3 / 5 │   814 │
    │ Bb2 │ III    │ m3z │   316 │ G3   │ II     │ 4p  │   498 │ M6 - 5 / 3   │   884 │
    │ C3  │ III    │ 4p  │   498 │ A3   │ II     │ 5z  │   680 │ M6 - 5 / 3   │   884 │
    │ D3  │ III    │ 5z  │   680 │ Bb3  │ II     │ m6p │   792 │ m6 - 2^3 / 5 │   814 │
    │ E3  │ II     │ M2z │   182 │ C4   │ I      │ m3p │   294 │ m6 - 2^3 / 5 │   814 │
    └─────┴────────┴─────┴───────┴──────┴────────┴─────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "fz_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_fz in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────┬───────┬──────┬────────┬─────┬───────┬──────────┬───────┐
    │ Low │ String │ Pos │ Cents │ High │ String │ Pos │ Cents │ Interval │ Cents │
    ├┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┤
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘ |}]
;;

let%expect_test "f_sharp_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────┬───────┬──────┬────────┬─────┬───────┬──────────┬───────┐
    │ Low │ String │ Pos │ Cents │ High │ String │ Pos │ Cents │ Interval │ Cents │
    ├┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┤
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘ |}]
;;

let%expect_test "g_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_gz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ Gb2 │ IV     │ d5z   │   610 │ Eb3  │ III    │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Ab2 │ III    │ m2z   │   112 │ F3   │ II     │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ Bb2 │ III    │ m3p   │   294 │ Gb3  │ II     │ d4z   │   406 │ m6 - 2^3 / 5 │   814 │
    │ Cb3 │ III    │ d4z   │   406 │ Ab3  │ II     │ d5p   │   588 │ M6 - 5 / 3   │   884 │
    │ Db3 │ III    │ d5z   │   610 │ Bb3  │ II     │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Eb3 │ II     │ m2p   │    90 │ Cb4  │ I      │ d3z   │   202 │ m6 - 2^3 / 5 │   814 │
    │ F3  │ II     │ m3p   │   294 │ Db4  │ I      │ d4z   │   406 │ m6 - 2^3 / 5 │   814 │
    │ Gb3 │ II     │ d4z   │   406 │ Eb4  │ I      │ d5p   │   588 │ M6 - 5 / 3   │   884 │
    │ Ab3 │ II     │ d5z   │   610 │ F4   │ I      │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Bb3 │ II     │ m6p   │   792 │ Gb4  │ I      │ d7z   │   904 │ m6 - 2^3 / 5 │   814 │
    │ Cb4 │ II     │ d7z   │   904 │ Ab4  │ I      │ d8p   │  1086 │ M6 - 5 / 3   │   884 │
    │ Db4 │ II     │ d8z   │  1108 │ Bb4  │ I      │ m2p-1 │  1290 │ M6 - 5 / 3   │   884 │
    │ Eb4 │ II     │ m2p-1 │  1290 │ Cb5  │ I      │ d3z-1 │  1402 │ m6 - 2^3 / 5 │   814 │
    │ F4  │ II     │ m3p-1 │  1494 │ Db5  │ I      │ d4z-1 │  1606 │ m6 - 2^3 / 5 │   814 │
    │ Gb4 │ II     │ d4z-1 │  1606 │ Eb5  │ I      │ d5p-1 │  1788 │ M6 - 5 / 3   │   884 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ F5   │ I      │ m6p-1 │  1992 │ M6 - 5 / 3   │   884 │
    │ Bb4 │ II     │ m6p-1 │  1992 │ Gb5  │ I      │ d7z-1 │  2104 │ m6 - 2^3 / 5 │   814 │
    │ Cb5 │ II     │ d7z-1 │  2104 │ Ab5  │ I      │ d8p-1 │  2286 │ M6 - 5 / 3   │   884 │
    │ Db5 │ II     │ d8z-1 │  2308 │ Bb5  │ I      │ m2p-2 │  2490 │ M6 - 5 / 3   │   884 │
    │ Eb5 │ II     │ m2p-2 │  2490 │ Cb6  │ I      │ d3z-2 │  2602 │ m6 - 2^3 / 5 │   814 │
    │ F5  │ II     │ m3p-2 │  2694 │ Db6  │ I      │ d4z-2 │  2806 │ m6 - 2^3 / 5 │   814 │
    │ Gb5 │ II     │ d4z-2 │  2806 │ Eb6  │ I      │ d5p-2 │  2988 │ M6 - 5 / 3   │   884 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ F6   │ I      │ m6p-2 │  3192 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "g_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_g in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ G2  │ III    │ 0     │     0 │ E3   │ II     │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ A2  │ III    │ M2p   │   204 │ F#3  │ II     │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ B2  │ III    │ M3z   │   386 │ G3   │ II     │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ C3  │ III    │ 4p    │   498 │ A3   │ II     │ 5z    │   680 │ M6 - 5 / 3   │   884 │
    │ D3  │ II     │ 0     │     0 │ B3   │ I      │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ E3  │ II     │ M2z   │   182 │ C4   │ I      │ m3p   │   294 │ m6 - 2^3 / 5 │   814 │
    │ F#3 │ II     │ M3z   │   386 │ D4   │ I      │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ G3  │ II     │ 4p    │   498 │ E4   │ I      │ 5z    │   680 │ M6 - 5 / 3   │   884 │
    │ A3  │ II     │ 5p    │   702 │ F#4  │ I      │ M6z   │   884 │ M6 - 5 / 3   │   884 │
    │ B3  │ II     │ M6z   │   884 │ G4   │ I      │ m7p   │   996 │ m6 - 2^3 / 5 │   814 │
    │ C4  │ II     │ m7p   │   996 │ A4   │ I      │ 8z    │  1178 │ M6 - 5 / 3   │   884 │
    │ D4  │ II     │ 0-1   │  1200 │ B4   │ I      │ M2z-1 │  1382 │ M6 - 5 / 3   │   884 │
    │ E4  │ II     │ M2z-1 │  1382 │ C5   │ I      │ m3p-1 │  1494 │ m6 - 2^3 / 5 │   814 │
    │ F#4 │ II     │ M3z-1 │  1586 │ D5   │ I      │ 4p-1  │  1698 │ m6 - 2^3 / 5 │   814 │
    │ G4  │ II     │ 4p-1  │  1698 │ E5   │ I      │ 5z-1  │  1880 │ M6 - 5 / 3   │   884 │
    │ A4  │ II     │ 5p-1  │  1902 │ F#5  │ I      │ M6z-1 │  2084 │ M6 - 5 / 3   │   884 │
    │ B4  │ II     │ M6z-1 │  2084 │ G5   │ I      │ m7p-1 │  2196 │ m6 - 2^3 / 5 │   814 │
    │ C5  │ II     │ m7p-1 │  2196 │ A5   │ I      │ 8z-1  │  2378 │ M6 - 5 / 3   │   884 │
    │ D5  │ II     │ 0-2   │  2400 │ B5   │ I      │ M2z-2 │  2582 │ M6 - 5 / 3   │   884 │
    │ E5  │ II     │ M2z-2 │  2582 │ C6   │ I      │ m3p-2 │  2694 │ m6 - 2^3 / 5 │   814 │
    │ F#5 │ II     │ M3z-2 │  2786 │ D6   │ I      │ 4p-2  │  2898 │ m6 - 2^3 / 5 │   814 │
    │ G5  │ II     │ 4p-2  │  2898 │ E6   │ I      │ 5z-2  │  3080 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_az_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ Ab2 │ III    │ m2z   │   112 │ F3   │ II     │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ Bb2 │ III    │ m3z   │   316 │ G3   │ II     │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ C3  │ III    │ 4p    │   498 │ Ab3  │ II     │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ Db3 │ III    │ d5z   │   610 │ Bb3  │ II     │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Eb3 │ II     │ m2z   │   112 │ C4   │ I      │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ F3  │ II     │ m3p   │   294 │ Db4  │ I      │ d4z   │   406 │ m6 - 2^3 / 5 │   814 │
    │ G3  │ II     │ 4p    │   498 │ Eb4  │ I      │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ Ab3 │ II     │ d5z   │   610 │ F4   │ I      │ m6p   │   792 │ M6 - 5 / 3   │   884 │
    │ Bb3 │ II     │ m6z   │   814 │ G4   │ I      │ m7p   │   996 │ M6 - 5 / 3   │   884 │
    │ C4  │ II     │ m7p   │   996 │ Ab4  │ I      │ d8z   │  1108 │ m6 - 2^3 / 5 │   814 │
    │ Db4 │ II     │ d8z   │  1108 │ Bb4  │ I      │ m2p-1 │  1290 │ M6 - 5 / 3   │   884 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ C5   │ I      │ m3p-1 │  1494 │ M6 - 5 / 3   │   884 │
    │ F4  │ II     │ m3p-1 │  1494 │ Db5  │ I      │ d4z-1 │  1606 │ m6 - 2^3 / 5 │   814 │
    │ G4  │ II     │ 4p-1  │  1698 │ Eb5  │ I      │ d5z-1 │  1810 │ m6 - 2^3 / 5 │   814 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ F5   │ I      │ m6p-1 │  1992 │ M6 - 5 / 3   │   884 │
    │ Bb4 │ II     │ m6z-1 │  2014 │ G5   │ I      │ m7p-1 │  2196 │ M6 - 5 / 3   │   884 │
    │ C5  │ II     │ m7p-1 │  2196 │ Ab5  │ I      │ d8z-1 │  2308 │ m6 - 2^3 / 5 │   814 │
    │ Db5 │ II     │ d8z-1 │  2308 │ Bb5  │ I      │ m2p-2 │  2490 │ M6 - 5 / 3   │   884 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ C6   │ I      │ m3p-2 │  2694 │ M6 - 5 / 3   │   884 │
    │ F5  │ II     │ m3p-2 │  2694 │ Db6  │ I      │ d4z-2 │  2806 │ m6 - 2^3 / 5 │   814 │
    │ G5  │ II     │ 4p-2  │  2898 │ Eb6  │ I      │ d5z-2 │  3010 │ m6 - 2^3 / 5 │   814 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ F6   │ I      │ m6p-2 │  3192 │ M6 - 5 / 3   │   884 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "a_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ A2  │ III    │ M2p   │   204 │ F#3  │ II     │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ B2  │ III    │ M3p   │   408 │ G#3  │ II     │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ C#3 │ III    │ A4z   │   590 │ A3   │ II     │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ D3  │ II     │ 0     │     0 │ B3   │ I      │ M2z   │   182 │ M6 - 5 / 3   │   884 │
    │ E3  │ II     │ M2p   │   204 │ C#4  │ I      │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ F#3 │ II     │ M3z   │   386 │ D4   │ I      │ 4p    │   498 │ m6 - 2^3 / 5 │   814 │
    │ G#3 │ II     │ A4z   │   590 │ E4   │ I      │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ A3  │ II     │ 5p    │   702 │ F#4  │ I      │ M6z   │   884 │ M6 - 5 / 3   │   884 │
    │ B3  │ II     │ M6p   │   906 │ G#4  │ I      │ M7z   │  1088 │ M6 - 5 / 3   │   884 │
    │ C#4 │ II     │ M7z   │  1088 │ A4   │ I      │ 0-1   │  1200 │ m6 - 2^3 / 5 │   814 │
    │ D4  │ II     │ 0-1   │  1200 │ B4   │ I      │ M2z-1 │  1382 │ M6 - 5 / 3   │   884 │
    │ E4  │ II     │ M2p-1 │  1404 │ C#5  │ I      │ M3z-1 │  1586 │ M6 - 5 / 3   │   884 │
    │ F#4 │ II     │ M3z-1 │  1586 │ D5   │ I      │ 4p-1  │  1698 │ m6 - 2^3 / 5 │   814 │
    │ G#4 │ II     │ A4z-1 │  1790 │ E5   │ I      │ 5p-1  │  1902 │ m6 - 2^3 / 5 │   814 │
    │ A4  │ II     │ 5p-1  │  1902 │ F#5  │ I      │ M6z-1 │  2084 │ M6 - 5 / 3   │   884 │
    │ B4  │ II     │ M6p-1 │  2106 │ G#5  │ I      │ M7z-1 │  2288 │ M6 - 5 / 3   │   884 │
    │ C#5 │ II     │ M7z-1 │  2288 │ A5   │ I      │ 0-2   │  2400 │ m6 - 2^3 / 5 │   814 │
    │ D5  │ II     │ 0-2   │  2400 │ B5   │ I      │ M2z-2 │  2582 │ M6 - 5 / 3   │   884 │
    │ E5  │ II     │ M2p-2 │  2604 │ C#6  │ I      │ M3z-2 │  2786 │ M6 - 5 / 3   │   884 │
    │ F#5 │ II     │ M3z-2 │  2786 │ D6   │ I      │ 4p-2  │  2898 │ m6 - 2^3 / 5 │   814 │
    │ G#5 │ II     │ A4z-2 │  2990 │ E6   │ I      │ 5p-2  │  3102 │ m6 - 2^3 / 5 │   814 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_bz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ Bb2 │ III    │ m3z   │   316 │ G3   │ II     │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ C3  │ III    │ 4z    │   520 │ A3   │ II     │ 5p    │   702 │ M6 - 5 / 3   │   884 │
    │ D3  │ II     │ 0     │     0 │ Bb3  │ I      │ m2z   │   112 │ m6 - 2^3 / 5 │   814 │
    │ Eb3 │ II     │ m2z   │   112 │ C4   │ I      │ m3p   │   294 │ M6 - 5 / 3   │   884 │
    │ F3  │ II     │ m3z   │   316 │ D4   │ I      │ 4p    │   498 │ M6 - 5 / 3   │   884 │
    │ G3  │ II     │ 4p    │   498 │ Eb4  │ I      │ d5z   │   610 │ m6 - 2^3 / 5 │   814 │
    │ A3  │ II     │ 5p    │   702 │ F4   │ I      │ m6z   │   814 │ m6 - 2^3 / 5 │   814 │
    │ Bb3 │ II     │ m6z   │   814 │ G4   │ I      │ m7p   │   996 │ M6 - 5 / 3   │   884 │
    │ D4  │ II     │ 0-1   │  1200 │ Bb4  │ I      │ m2z-1 │  1312 │ m6 - 2^3 / 5 │   814 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ C5   │ I      │ m3p-1 │  1494 │ M6 - 5 / 3   │   884 │
    │ F4  │ II     │ m3z-1 │  1516 │ D5   │ I      │ 4p-1  │  1698 │ M6 - 5 / 3   │   884 │
    │ G4  │ II     │ 4p-1  │  1698 │ Eb5  │ I      │ d5z-1 │  1810 │ m6 - 2^3 / 5 │   814 │
    │ A4  │ II     │ 5p-1  │  1902 │ F5   │ I      │ m6z-1 │  2014 │ m6 - 2^3 / 5 │   814 │
    │ Bb4 │ II     │ m6z-1 │  2014 │ G5   │ I      │ m7p-1 │  2196 │ M6 - 5 / 3   │   884 │
    │ D5  │ II     │ 0-2   │  2400 │ Bb5  │ I      │ m2z-2 │  2512 │ m6 - 2^3 / 5 │   814 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ C6   │ I      │ m3p-2 │  2694 │ M6 - 5 / 3   │   884 │
    │ F5  │ II     │ m3z-2 │  2716 │ D6   │ I      │ 4p-2  │  2898 │ M6 - 5 / 3   │   884 │
    │ G5  │ II     │ 4p-2  │  2898 │ Eb6  │ I      │ d5z-2 │  3010 │ m6 - 2^3 / 5 │   814 │
    │ A5  │ II     │ 5p-2  │  3102 │ F6   │ I      │ m6z-2 │  3214 │ m6 - 2^3 / 5 │   814 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "b_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_bp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ B2  │ III    │ M3p   │   408 │ G#3  │ II     │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ C#3 │ III    │ A4p   │   612 │ A#3  │ II     │ A5z   │   794 │ M6 - 5 / 3   │   884 │
    │ D#3 │ II     │ A1z   │    92 │ B3   │ I      │ M2p   │   204 │ m6 - 2^3 / 5 │   814 │
    │ E3  │ II     │ M2p   │   204 │ C#4  │ I      │ M3z   │   386 │ M6 - 5 / 3   │   884 │
    │ F#3 │ II     │ M3p   │   408 │ D#4  │ I      │ A4z   │   590 │ M6 - 5 / 3   │   884 │
    │ G#3 │ II     │ A4z   │   590 │ E4   │ I      │ 5p    │   702 │ m6 - 2^3 / 5 │   814 │
    │ A#3 │ II     │ A5z   │   794 │ F#4  │ I      │ M6p   │   906 │ m6 - 2^3 / 5 │   814 │
    │ B3  │ II     │ M6p   │   906 │ G#4  │ I      │ M7z   │  1088 │ M6 - 5 / 3   │   884 │
    │ C#4 │ II     │ M7p   │  1110 │ A#4  │ I      │ A1z-1 │  1292 │ M6 - 5 / 3   │   884 │
    │ D#4 │ II     │ A1z-1 │  1292 │ B4   │ I      │ M2p-1 │  1404 │ m6 - 2^3 / 5 │   814 │
    │ E4  │ II     │ M2p-1 │  1404 │ C#5  │ I      │ M3z-1 │  1586 │ M6 - 5 / 3   │   884 │
    │ F#4 │ II     │ M3p-1 │  1608 │ D#5  │ I      │ A4z-1 │  1790 │ M6 - 5 / 3   │   884 │
    │ G#4 │ II     │ A4z-1 │  1790 │ E5   │ I      │ 5p-1  │  1902 │ m6 - 2^3 / 5 │   814 │
    │ A#4 │ II     │ A5z-1 │  1994 │ F#5  │ I      │ M6p-1 │  2106 │ m6 - 2^3 / 5 │   814 │
    │ B4  │ II     │ M6p-1 │  2106 │ G#5  │ I      │ M7z-1 │  2288 │ M6 - 5 / 3   │   884 │
    │ C#5 │ II     │ M7p-1 │  2310 │ A#5  │ I      │ A1z-2 │  2492 │ M6 - 5 / 3   │   884 │
    │ D#5 │ II     │ A1z-2 │  2492 │ B5   │ I      │ M2p-2 │  2604 │ m6 - 2^3 / 5 │   814 │
    │ E5  │ II     │ M2p-2 │  2604 │ C#6  │ I      │ M3z-2 │  2786 │ M6 - 5 / 3   │   884 │
    │ F#5 │ II     │ M3p-2 │  2808 │ D#6  │ I      │ A4z-2 │  2990 │ M6 - 5 / 3   │   884 │
    │ G#5 │ II     │ A4z-2 │  2990 │ E6   │ I      │ 5p-2  │  3102 │ m6 - 2^3 / 5 │   814 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;
