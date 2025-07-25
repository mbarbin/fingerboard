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
        { number = Third; quality = Minor; additional_octaves = 0 }
  ; to_ = Acoustic_interval.just_minor_third
  }
;;

let make_scale ~characterized_scale ~from ~adjustment =
  let t = force Just.t in
  System.Double_stops.make_scale
    t
    ?adjustment
    ~characterized_scale
    ~interval_number:Third
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
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ E2  │ IV     │ M3z   │   386 │ G2   │ III    │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ F2  │ IV     │ 4p    │   498 │ A2   │ III    │ M2z   │   182 │ M3 - 5 / 2^2     │   386 │
    │ G2  │ IV     │ 5p    │   702 │ B2   │ III    │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ A2  │ IV     │ M6z   │   884 │ C3   │ III    │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ B2  │ III    │ M3z   │   386 │ D3   │ II     │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ C3  │ III    │ 4p    │   498 │ E3   │ II     │ M2z   │   182 │ M3 - 5 / 2^2     │   386 │
    │ D3  │ III    │ 5p    │   702 │ F3   │ II     │ m3z   │   316 │ m3 - (2 * 3) / 5 │   316 │
    │ E3  │ III    │ M6z   │   884 │ G3   │ II     │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ F3  │ III    │ m7p   │   996 │ A3   │ II     │ 5z    │   680 │ M3 - 5 / 2^2     │   386 │
    │ G3  │ II     │ 4p    │   498 │ B3   │ I      │ M2z   │   182 │ M3 - 5 / 2^2     │   386 │
    │ A3  │ II     │ 5z    │   680 │ C4   │ I      │ m3p   │   294 │ m3 - (2 * 3) / 5 │   316 │
    │ B3  │ II     │ M6z   │   884 │ D4   │ I      │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ C4  │ II     │ m7p   │   996 │ E4   │ I      │ 5z    │   680 │ M3 - 5 / 2^2     │   386 │
    │ D4  │ II     │ 0-1   │  1200 │ F4   │ I      │ m6z   │   814 │ m3 - (2 * 3) / 5 │   316 │
    │ E4  │ II     │ M2z-1 │  1382 │ G4   │ I      │ m7p   │   996 │ m3 - (2 * 3) / 5 │   316 │
    │ F4  │ II     │ m3p-1 │  1494 │ A4   │ I      │ 8z    │  1178 │ M3 - 5 / 2^2     │   386 │
    │ G4  │ II     │ 4p-1  │  1698 │ B4   │ I      │ M2z-1 │  1382 │ M3 - 5 / 2^2     │   386 │
    │ A4  │ II     │ 5z-1  │  1880 │ C5   │ I      │ m3p-1 │  1494 │ m3 - (2 * 3) / 5 │   316 │
    │ B4  │ II     │ M6z-1 │  2084 │ D5   │ I      │ 4p-1  │  1698 │ m3 - (2 * 3) / 5 │   316 │
    │ C5  │ II     │ m7p-1 │  2196 │ E5   │ I      │ 5z-1  │  1880 │ M3 - 5 / 2^2     │   386 │
    │ D5  │ II     │ 0-2   │  2400 │ F5   │ I      │ m6z-1 │  2014 │ m3 - (2 * 3) / 5 │   316 │
    │ E5  │ II     │ M2z-2 │  2582 │ G5   │ I      │ m7p-1 │  2196 │ m3 - (2 * 3) / 5 │   316 │
    │ F5  │ II     │ m3p-2 │  2694 │ A5   │ I      │ 8z-1  │  2378 │ M3 - 5 / 2^2     │   386 │
    │ G5  │ II     │ 4p-2  │  2898 │ B5   │ I      │ M2z-2 │  2582 │ M3 - 5 / 2^2     │   386 │
    │ A5  │ II     │ 5z-2  │  3080 │ C6   │ I      │ m3p-2 │  2694 │ m3 - (2 * 3) / 5 │   316 │
    │ B5  │ II     │ M6z-2 │  3284 │ D6   │ I      │ 4p-2  │  2898 │ m3 - (2 * 3) / 5 │   316 │
    │ C6  │ II     │ m7p-2 │  3396 │ E6   │ I      │ 5z-2  │  3080 │ M3 - 5 / 2^2     │   386 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

(* Since [Just] is a superset of [Pythagorean], it supports all of the
   same pythagorean scales. We only test one here, since they all
   already appear in [pythagorean/thirds.ml]. *)
let%expect_test "c_major_pythagorean" =
  let t = force Just.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval       │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼────────────────┼───────┤
    │ E2  │ IV     │ M3p   │   408 │ G2   │ III    │ 0     │     0 │ m3 - 2^5 / 3^3 │   294 │
    │ F2  │ IV     │ 4p    │   498 │ A2   │ III    │ M2p   │   204 │ M3 - 3^4 / 2^6 │   408 │
    │ G2  │ IV     │ 5p    │   702 │ B2   │ III    │ M3p   │   408 │ M3 - 3^4 / 2^6 │   408 │
    │ A2  │ IV     │ M6p   │   906 │ C3   │ III    │ 4p    │   498 │ m3 - 2^5 / 3^3 │   294 │
    │ B2  │ III    │ M3p   │   408 │ D3   │ II     │ 0     │     0 │ m3 - 2^5 / 3^3 │   294 │
    │ C3  │ III    │ 4p    │   498 │ E3   │ II     │ M2p   │   204 │ M3 - 3^4 / 2^6 │   408 │
    │ D3  │ III    │ 5p    │   702 │ F3   │ II     │ m3p   │   294 │ m3 - 2^5 / 3^3 │   294 │
    │ E3  │ III    │ M6p   │   906 │ G3   │ II     │ 4p    │   498 │ m3 - 2^5 / 3^3 │   294 │
    │ F3  │ II     │ m3p   │   294 │ A3   │ I      │ 0     │     0 │ M3 - 3^4 / 2^6 │   408 │
    │ G3  │ II     │ 4p    │   498 │ B3   │ I      │ M2p   │   204 │ M3 - 3^4 / 2^6 │   408 │
    │ A3  │ II     │ 5p    │   702 │ C4   │ I      │ m3p   │   294 │ m3 - 2^5 / 3^3 │   294 │
    │ B3  │ II     │ M6p   │   906 │ D4   │ I      │ 4p    │   498 │ m3 - 2^5 / 3^3 │   294 │
    │ C4  │ II     │ m7p   │   996 │ E4   │ I      │ 5p    │   702 │ M3 - 3^4 / 2^6 │   408 │
    │ D4  │ II     │ 0-1   │  1200 │ F4   │ I      │ m6p   │   792 │ m3 - 2^5 / 3^3 │   294 │
    │ E4  │ II     │ M2p-1 │  1404 │ G4   │ I      │ m7p   │   996 │ m3 - 2^5 / 3^3 │   294 │
    │ F4  │ II     │ m3p-1 │  1494 │ A4   │ I      │ 0-1   │  1200 │ M3 - 3^4 / 2^6 │   408 │
    │ G4  │ II     │ 4p-1  │  1698 │ B4   │ I      │ M2p-1 │  1404 │ M3 - 3^4 / 2^6 │   408 │
    │ A4  │ II     │ 5p-1  │  1902 │ C5   │ I      │ m3p-1 │  1494 │ m3 - 2^5 / 3^3 │   294 │
    │ B4  │ II     │ M6p-1 │  2106 │ D5   │ I      │ 4p-1  │  1698 │ m3 - 2^5 / 3^3 │   294 │
    │ C5  │ II     │ m7p-1 │  2196 │ E5   │ I      │ 5p-1  │  1902 │ M3 - 3^4 / 2^6 │   408 │
    │ D5  │ II     │ 0-2   │  2400 │ F5   │ I      │ m6p-1 │  1992 │ m3 - 2^5 / 3^3 │   294 │
    │ E5  │ II     │ M2p-2 │  2604 │ G5   │ I      │ m7p-1 │  2196 │ m3 - 2^5 / 3^3 │   294 │
    │ F5  │ II     │ m3p-2 │  2694 │ A5   │ I      │ 0-2   │  2400 │ M3 - 3^4 / 2^6 │   408 │
    │ G5  │ II     │ 4p-2  │  2898 │ B5   │ I      │ M2p-2 │  2604 │ M3 - 3^4 / 2^6 │   408 │
    │ A5  │ II     │ 5p-2  │  3102 │ C6   │ I      │ m3p-2 │  2694 │ m3 - 2^5 / 3^3 │   294 │
    │ B5  │ II     │ M6p-2 │  3306 │ D6   │ I      │ 4p-2  │  2898 │ m3 - 2^5 / 3^3 │   294 │
    │ C6  │ II     │ m7p-2 │  3396 │ E6   │ I      │ 5p-2  │  3102 │ M3 - 3^4 / 2^6 │   408 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴────────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_dz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ F2  │ IV     │ 4p    │   498 │ Ab2  │ III    │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb2 │ IV     │ d5z   │   610 │ Bb2  │ III    │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Ab2 │ IV     │ m6z   │   814 │ C3   │ III    │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ Bb2 │ IV     │ m7p   │   996 │ Db3  │ III    │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ C3  │ III    │ 4p    │   498 │ Eb3  │ II     │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ Db3 │ III    │ d5z   │   610 │ F3   │ II     │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Eb3 │ III    │ m6p   │   792 │ Gb3  │ II     │ d4z   │   406 │ m3 - (2 * 3) / 5 │   316 │
    │ F3  │ III    │ m7p   │   996 │ Ab3  │ II     │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb3 │ II     │ d4z   │   406 │ Bb3  │ I      │ m2p   │    90 │ M3 - 5 / 2^2     │   386 │
    │ Ab3 │ II     │ d5z   │   610 │ C4   │ I      │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Bb3 │ II     │ m6p   │   792 │ Db4  │ I      │ d4z   │   406 │ m3 - (2 * 3) / 5 │   316 │
    │ C4  │ II     │ m7p   │   996 │ Eb4  │ I      │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ Db4 │ II     │ d8z   │  1108 │ F4   │ I      │ m6p   │   792 │ M3 - 5 / 2^2     │   386 │
    │ Eb4 │ II     │ m2p-1 │  1290 │ Gb4  │ I      │ d7z   │   904 │ m3 - (2 * 3) / 5 │   316 │
    │ F4  │ II     │ m3p-1 │  1494 │ Ab4  │ I      │ d8z   │  1108 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb4 │ II     │ d4z-1 │  1606 │ Bb4  │ I      │ m2p-1 │  1290 │ M3 - 5 / 2^2     │   386 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ C5   │ I      │ m3p-1 │  1494 │ M3 - 5 / 2^2     │   386 │
    │ Bb4 │ II     │ m6p-1 │  1992 │ Db5  │ I      │ d4z-1 │  1606 │ m3 - (2 * 3) / 5 │   316 │
    │ C5  │ II     │ m7p-1 │  2196 │ Eb5  │ I      │ d5z-1 │  1810 │ m3 - (2 * 3) / 5 │   316 │
    │ Db5 │ II     │ d8z-1 │  2308 │ F5   │ I      │ m6p-1 │  1992 │ M3 - 5 / 2^2     │   386 │
    │ Eb5 │ II     │ m2p-2 │  2490 │ Gb5  │ I      │ d7z-1 │  2104 │ m3 - (2 * 3) / 5 │   316 │
    │ F5  │ II     │ m3p-2 │  2694 │ Ab5  │ I      │ d8z-1 │  2308 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb5 │ II     │ d4z-2 │  2806 │ Bb5  │ I      │ m2p-2 │  2490 │ M3 - 5 / 2^2     │   386 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ C6   │ I      │ m3p-2 │  2694 │ M3 - 5 / 2^2     │   386 │
    │ Bb5 │ II     │ m6p-2 │  3192 │ Db6  │ I      │ d4z-2 │  2806 │ m3 - (2 * 3) / 5 │   316 │
    │ C6  │ II     │ m7p-2 │  3396 │ Eb6  │ I      │ d5z-2 │  3010 │ m3 - (2 * 3) / 5 │   316 │
    │ Db6 │ II     │ d8z-2 │  3508 │ F6   │ I      │ m6p-2 │  3192 │ M3 - 5 / 2^2     │   386 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "d_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ E2  │ IV     │ M3z   │   386 │ G2   │ III    │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ F#2 │ IV     │ A4z   │   590 │ A2   │ III    │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ G2  │ IV     │ 5p    │   702 │ B2   │ III    │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ A2  │ IV     │ M6p   │   906 │ C#3  │ III    │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ B2  │ III    │ M3z   │   386 │ D3   │ II     │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ C#3 │ III    │ A4z   │   590 │ E3   │ II     │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ D3  │ III    │ 5p    │   702 │ F#3  │ II     │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ E3  │ III    │ M6z   │   884 │ G3   │ II     │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ F#3 │ II     │ M3z   │   386 │ A3   │ I      │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ G3  │ II     │ 4p    │   498 │ B3   │ I      │ M2z   │   182 │ M3 - 5 / 2^2     │   386 │
    │ A3  │ II     │ 5p    │   702 │ C#4  │ I      │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ B3  │ II     │ M6z   │   884 │ D4   │ I      │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ C#4 │ II     │ M7z   │  1088 │ E4   │ I      │ 5p    │   702 │ m3 - (2 * 3) / 5 │   316 │
    │ D4  │ II     │ 0-1   │  1200 │ F#4  │ I      │ M6z   │   884 │ M3 - 5 / 2^2     │   386 │
    │ E4  │ II     │ M2z-1 │  1382 │ G4   │ I      │ m7p   │   996 │ m3 - (2 * 3) / 5 │   316 │
    │ F#4 │ II     │ M3z-1 │  1586 │ A4   │ I      │ 0-1   │  1200 │ m3 - (2 * 3) / 5 │   316 │
    │ G4  │ II     │ 4p-1  │  1698 │ B4   │ I      │ M2z-1 │  1382 │ M3 - 5 / 2^2     │   386 │
    │ A4  │ II     │ 5p-1  │  1902 │ C#5  │ I      │ M3z-1 │  1586 │ M3 - 5 / 2^2     │   386 │
    │ B4  │ II     │ M6z-1 │  2084 │ D5   │ I      │ 4p-1  │  1698 │ m3 - (2 * 3) / 5 │   316 │
    │ C#5 │ II     │ M7z-1 │  2288 │ E5   │ I      │ 5p-1  │  1902 │ m3 - (2 * 3) / 5 │   316 │
    │ D5  │ II     │ 0-2   │  2400 │ F#5  │ I      │ M6z-1 │  2084 │ M3 - 5 / 2^2     │   386 │
    │ E5  │ II     │ M2z-2 │  2582 │ G5   │ I      │ m7p-1 │  2196 │ m3 - (2 * 3) / 5 │   316 │
    │ F#5 │ II     │ M3z-2 │  2786 │ A5   │ I      │ 0-2   │  2400 │ m3 - (2 * 3) / 5 │   316 │
    │ G5  │ II     │ 4p-2  │  2898 │ B5   │ I      │ M2z-2 │  2582 │ M3 - 5 / 2^2     │   386 │
    │ A5  │ II     │ 5p-2  │  3102 │ C#6  │ I      │ M3z-2 │  2786 │ M3 - 5 / 2^2     │   386 │
    │ B5  │ II     │ M6z-2 │  3284 │ D6   │ I      │ 4p-2  │  2898 │ m3 - (2 * 3) / 5 │   316 │
    │ C#6 │ II     │ M7z-2 │  3488 │ E6   │ I      │ 5p-2  │  3102 │ m3 - (2 * 3) / 5 │   316 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_ez_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ Eb2 │ IV     │ m3z   │   316 │ G2   │ III    │ 0     │     0 │ M3 - 5 / 2^2     │   386 │
    │ F2  │ IV     │ 4p    │   498 │ Ab2  │ III    │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ G2  │ IV     │ 5p    │   702 │ Bb2  │ III    │ m3z   │   316 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab2 │ IV     │ m6z   │   814 │ C3   │ III    │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ Bb2 │ III    │ m3z   │   316 │ D3   │ II     │ 0     │     0 │ M3 - 5 / 2^2     │   386 │
    │ C3  │ III    │ 4p    │   498 │ Eb3  │ II     │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ D3  │ III    │ 5p    │   702 │ F3   │ II     │ m3z   │   316 │ m3 - (2 * 3) / 5 │   316 │
    │ Eb3 │ III    │ m6z   │   814 │ G3   │ II     │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ G3  │ II     │ 4p    │   498 │ Bb3  │ I      │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab3 │ II     │ d5z   │   610 │ C4   │ I      │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Bb3 │ II     │ m6z   │   814 │ D4   │ I      │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ C4  │ II     │ m7p   │   996 │ Eb4  │ I      │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ D4  │ II     │ 0-1   │  1200 │ F4   │ I      │ m6z   │   814 │ m3 - (2 * 3) / 5 │   316 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ G4   │ I      │ m7p   │   996 │ M3 - 5 / 2^2     │   386 │
    │ F4  │ II     │ m3p-1 │  1494 │ Ab4  │ I      │ d8z   │  1108 │ m3 - (2 * 3) / 5 │   316 │
    │ G4  │ II     │ 4p-1  │  1698 │ Bb4  │ I      │ m2z-1 │  1312 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ C5   │ I      │ m3p-1 │  1494 │ M3 - 5 / 2^2     │   386 │
    │ Bb4 │ II     │ m6z-1 │  2014 │ D5   │ I      │ 4p-1  │  1698 │ M3 - 5 / 2^2     │   386 │
    │ C5  │ II     │ m7p-1 │  2196 │ Eb5  │ I      │ d5z-1 │  1810 │ m3 - (2 * 3) / 5 │   316 │
    │ D5  │ II     │ 0-2   │  2400 │ F5   │ I      │ m6z-1 │  2014 │ m3 - (2 * 3) / 5 │   316 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ G5   │ I      │ m7p-1 │  2196 │ M3 - 5 / 2^2     │   386 │
    │ F5  │ II     │ m3p-2 │  2694 │ Ab5  │ I      │ d8z-1 │  2308 │ m3 - (2 * 3) / 5 │   316 │
    │ G5  │ II     │ 4p-2  │  2898 │ Bb5  │ I      │ m2z-2 │  2512 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ C6   │ I      │ m3p-2 │  2694 │ M3 - 5 / 2^2     │   386 │
    │ Bb5 │ II     │ m6z-2 │  3214 │ D6   │ I      │ 4p-2  │  2898 │ M3 - 5 / 2^2     │   386 │
    │ C6  │ II     │ m7p-2 │  3396 │ Eb6  │ I      │ d5z-2 │  3010 │ m3 - (2 * 3) / 5 │   316 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "e_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ E2  │ IV     │ M3p   │   408 │ G#2  │ III    │ A1z   │    92 │ M3 - 5 / 2^2     │   386 │
    │ F#2 │ IV     │ A4z   │   590 │ A2   │ III    │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ G#2 │ IV     │ A5z   │   794 │ B2   │ III    │ M3p   │   408 │ m3 - (2 * 3) / 5 │   316 │
    │ A2  │ IV     │ M6p   │   906 │ C#3  │ III    │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ B2  │ III    │ M3p   │   408 │ D#3  │ II     │ A1z   │    92 │ M3 - 5 / 2^2     │   386 │
    │ C#3 │ III    │ A4z   │   590 │ E3   │ II     │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ D#3 │ III    │ A5z   │   794 │ F#3  │ II     │ M3p   │   408 │ m3 - (2 * 3) / 5 │   316 │
    │ E3  │ III    │ M6p   │   906 │ G#3  │ II     │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ F#3 │ II     │ M3z   │   386 │ A3   │ I      │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ G#3 │ II     │ A4z   │   590 │ B3   │ I      │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ A3  │ II     │ 5p    │   702 │ C#4  │ I      │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ B3  │ II     │ M6p   │   906 │ D#4  │ I      │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ C#4 │ II     │ M7z   │  1088 │ E4   │ I      │ 5p    │   702 │ m3 - (2 * 3) / 5 │   316 │
    │ D#4 │ II     │ A1z-1 │  1292 │ F#4  │ I      │ M6p   │   906 │ m3 - (2 * 3) / 5 │   316 │
    │ E4  │ II     │ M2p-1 │  1404 │ G#4  │ I      │ M7z   │  1088 │ M3 - 5 / 2^2     │   386 │
    │ F#4 │ II     │ M3z-1 │  1586 │ A4   │ I      │ 0-1   │  1200 │ m3 - (2 * 3) / 5 │   316 │
    │ G#4 │ II     │ A4z-1 │  1790 │ B4   │ I      │ M2p-1 │  1404 │ m3 - (2 * 3) / 5 │   316 │
    │ A4  │ II     │ 5p-1  │  1902 │ C#5  │ I      │ M3z-1 │  1586 │ M3 - 5 / 2^2     │   386 │
    │ B4  │ II     │ M6p-1 │  2106 │ D#5  │ I      │ A4z-1 │  1790 │ M3 - 5 / 2^2     │   386 │
    │ C#5 │ II     │ M7z-1 │  2288 │ E5   │ I      │ 5p-1  │  1902 │ m3 - (2 * 3) / 5 │   316 │
    │ D#5 │ II     │ A1z-2 │  2492 │ F#5  │ I      │ M6p-1 │  2106 │ m3 - (2 * 3) / 5 │   316 │
    │ E5  │ II     │ M2p-2 │  2604 │ G#5  │ I      │ M7z-1 │  2288 │ M3 - 5 / 2^2     │   386 │
    │ F#5 │ II     │ M3z-2 │  2786 │ A5   │ I      │ 0-2   │  2400 │ m3 - (2 * 3) / 5 │   316 │
    │ G#5 │ II     │ A4z-2 │  2990 │ B5   │ I      │ M2p-2 │  2604 │ m3 - (2 * 3) / 5 │   316 │
    │ A5  │ II     │ 5p-2  │  3102 │ C#6  │ I      │ M3z-2 │  2786 │ M3 - 5 / 2^2     │   386 │
    │ B5  │ II     │ M6p-2 │  3306 │ D#6  │ I      │ A4z-2 │  2990 │ M3 - 5 / 2^2     │   386 │
    │ C#6 │ II     │ M7z-2 │  3488 │ E6   │ I      │ 5p-2  │  3102 │ m3 - (2 * 3) / 5 │   316 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "fp_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────┬───────┬──────┬────────┬─────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos │ Cents │ High │ String │ Pos │ Cents │ Interval         │ Cents │
    ├─────┼────────┼─────┼───────┼──────┼────────┼─────┼───────┼──────────────────┼───────┤
    │ F2  │ IV     │ 4p  │   498 │ A2   │ III    │ M2z │   182 │ M3 - 5 / 2^2     │   386 │
    │ G2  │ IV     │ 5p  │   702 │ Bb2  │ III    │ m3z │   316 │ m3 - (2 * 3) / 5 │   316 │
    │ A2  │ IV     │ M6z │   884 │ C3   │ III    │ 4p  │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb2 │ IV     │ m7p │   996 │ D3   │ III    │ 5z  │   680 │ M3 - 5 / 2^2     │   386 │
    │ C3  │ III    │ 4p  │   498 │ E3   │ II     │ M2z │   182 │ M3 - 5 / 2^2     │   386 │
    │ D3  │ III    │ 5z  │   680 │ F3   │ II     │ m3p │   294 │ m3 - (2 * 3) / 5 │   316 │
    │ E3  │ III    │ M6z │   884 │ G3   │ II     │ 4p  │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ F3  │ III    │ m7p │   996 │ A3   │ II     │ 5z  │   680 │ M3 - 5 / 2^2     │   386 │
    │ G3  │ II     │ 4p  │   498 │ Bb3  │ I      │ m2z │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ A3  │ II     │ 5z  │   680 │ C4   │ I      │ m3p │   294 │ m3 - (2 * 3) / 5 │   316 │
    └─────┴────────┴─────┴───────┴──────┴────────┴─────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "fz_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_fz in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect {| |}]
;;

let%expect_test "f_sharp_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect {| |}]
;;

let%expect_test "g_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_gz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ Gb2 │ IV     │ d5z   │   610 │ Bb2  │ III    │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Ab2 │ IV     │ m6p   │   792 │ Cb3  │ III    │ d4z   │   406 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb2 │ IV     │ m7p   │   996 │ Db3  │ III    │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ Cb3 │ III    │ d4z   │   406 │ Eb3  │ II     │ m2p   │    90 │ M3 - 5 / 2^2     │   386 │
    │ Db3 │ III    │ d5z   │   610 │ F3   │ II     │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Eb3 │ III    │ m6p   │   792 │ Gb3  │ II     │ d4z   │   406 │ m3 - (2 * 3) / 5 │   316 │
    │ F3  │ III    │ m7p   │   996 │ Ab3  │ II     │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb3 │ II     │ d4z   │   406 │ Bb3  │ I      │ m2p   │    90 │ M3 - 5 / 2^2     │   386 │
    │ Ab3 │ II     │ d5p   │   588 │ Cb4  │ I      │ d3z   │   202 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb3 │ II     │ m6p   │   792 │ Db4  │ I      │ d4z   │   406 │ m3 - (2 * 3) / 5 │   316 │
    │ Cb4 │ II     │ d7z   │   904 │ Eb4  │ I      │ d5p   │   588 │ M3 - 5 / 2^2     │   386 │
    │ Db4 │ II     │ d8z   │  1108 │ F4   │ I      │ m6p   │   792 │ M3 - 5 / 2^2     │   386 │
    │ Eb4 │ II     │ m2p-1 │  1290 │ Gb4  │ I      │ d7z   │   904 │ m3 - (2 * 3) / 5 │   316 │
    │ F4  │ II     │ m3p-1 │  1494 │ Ab4  │ I      │ d8z   │  1108 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb4 │ II     │ d4z-1 │  1606 │ Bb4  │ I      │ m2p-1 │  1290 │ M3 - 5 / 2^2     │   386 │
    │ Ab4 │ II     │ d5p-1 │  1788 │ Cb5  │ I      │ d3z-1 │  1402 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb4 │ II     │ m6p-1 │  1992 │ Db5  │ I      │ d4z-1 │  1606 │ m3 - (2 * 3) / 5 │   316 │
    │ Cb5 │ II     │ d7z-1 │  2104 │ Eb5  │ I      │ d5p-1 │  1788 │ M3 - 5 / 2^2     │   386 │
    │ Db5 │ II     │ d8z-1 │  2308 │ F5   │ I      │ m6p-1 │  1992 │ M3 - 5 / 2^2     │   386 │
    │ Eb5 │ II     │ m2p-2 │  2490 │ Gb5  │ I      │ d7z-1 │  2104 │ m3 - (2 * 3) / 5 │   316 │
    │ F5  │ II     │ m3p-2 │  2694 │ Ab5  │ I      │ d8z-1 │  2308 │ m3 - (2 * 3) / 5 │   316 │
    │ Gb5 │ II     │ d4z-2 │  2806 │ Bb5  │ I      │ m2p-2 │  2490 │ M3 - 5 / 2^2     │   386 │
    │ Ab5 │ II     │ d5p-2 │  2988 │ Cb6  │ I      │ d3z-2 │  2602 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb5 │ II     │ m6p-2 │  3192 │ Db6  │ I      │ d4z-2 │  2806 │ m3 - (2 * 3) / 5 │   316 │
    │ Cb6 │ II     │ d7z-2 │  3304 │ Eb6  │ I      │ d5p-2 │  2988 │ M3 - 5 / 2^2     │   386 │
    │ Db6 │ II     │ d8z-2 │  3508 │ F6   │ I      │ m6p-2 │  3192 │ M3 - 5 / 2^2     │   386 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "g_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_g in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ G2  │ IV     │ 5p    │   702 │ B2   │ III    │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ A2  │ IV     │ M6z   │   884 │ C3   │ III    │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ B2  │ III    │ M3z   │   386 │ D3   │ II     │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ C3  │ III    │ 4p    │   498 │ E3   │ II     │ M2z   │   182 │ M3 - 5 / 2^2     │   386 │
    │ D3  │ III    │ 5p    │   702 │ F#3  │ II     │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ E3  │ III    │ M6z   │   884 │ G3   │ II     │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ F#3 │ II     │ M3z   │   386 │ A3   │ I      │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ G3  │ II     │ 4p    │   498 │ B3   │ I      │ M2z   │   182 │ M3 - 5 / 2^2     │   386 │
    │ A3  │ II     │ 5z    │   680 │ C4   │ I      │ m3p   │   294 │ m3 - (2 * 3) / 5 │   316 │
    │ B3  │ II     │ M6z   │   884 │ D4   │ I      │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ C4  │ II     │ m7p   │   996 │ E4   │ I      │ 5z    │   680 │ M3 - 5 / 2^2     │   386 │
    │ D4  │ II     │ 0-1   │  1200 │ F#4  │ I      │ M6z   │   884 │ M3 - 5 / 2^2     │   386 │
    │ E4  │ II     │ M2z-1 │  1382 │ G4   │ I      │ m7p   │   996 │ m3 - (2 * 3) / 5 │   316 │
    │ F#4 │ II     │ M3z-1 │  1586 │ A4   │ I      │ 0-1   │  1200 │ m3 - (2 * 3) / 5 │   316 │
    │ G4  │ II     │ 4p-1  │  1698 │ B4   │ I      │ M2z-1 │  1382 │ M3 - 5 / 2^2     │   386 │
    │ A4  │ II     │ 5z-1  │  1880 │ C5   │ I      │ m3p-1 │  1494 │ m3 - (2 * 3) / 5 │   316 │
    │ B4  │ II     │ M6z-1 │  2084 │ D5   │ I      │ 4p-1  │  1698 │ m3 - (2 * 3) / 5 │   316 │
    │ C5  │ II     │ m7p-1 │  2196 │ E5   │ I      │ 5z-1  │  1880 │ M3 - 5 / 2^2     │   386 │
    │ D5  │ II     │ 0-2   │  2400 │ F#5  │ I      │ M6z-1 │  2084 │ M3 - 5 / 2^2     │   386 │
    │ E5  │ II     │ M2z-2 │  2582 │ G5   │ I      │ m7p-1 │  2196 │ m3 - (2 * 3) / 5 │   316 │
    │ F#5 │ II     │ M3z-2 │  2786 │ A5   │ I      │ 0-2   │  2400 │ m3 - (2 * 3) / 5 │   316 │
    │ G5  │ II     │ 4p-2  │  2898 │ B5   │ I      │ M2z-2 │  2582 │ M3 - 5 / 2^2     │   386 │
    │ A5  │ II     │ 5z-2  │  3080 │ C6   │ I      │ m3p-2 │  2694 │ m3 - (2 * 3) / 5 │   316 │
    │ B5  │ II     │ M6z-2 │  3284 │ D6   │ I      │ 4p-2  │  2898 │ m3 - (2 * 3) / 5 │   316 │
    │ C6  │ II     │ m7p-2 │  3396 │ E6   │ I      │ 5z-2  │  3080 │ M3 - 5 / 2^2     │   386 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_az_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ Ab2 │ IV     │ m6z   │   814 │ C3   │ III    │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ C3  │ III    │ 4p    │   498 │ Eb3  │ II     │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ Db3 │ III    │ d5z   │   610 │ F3   │ II     │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Eb3 │ III    │ m6z   │   814 │ G3   │ II     │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ F3  │ III    │ m7p   │   996 │ Ab3  │ II     │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ G3  │ II     │ 4p    │   498 │ Bb3  │ I      │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab3 │ II     │ d5z   │   610 │ C4   │ I      │ m3p   │   294 │ M3 - 5 / 2^2     │   386 │
    │ Bb3 │ II     │ m6p   │   792 │ Db4  │ I      │ d4z   │   406 │ m3 - (2 * 3) / 5 │   316 │
    │ C4  │ II     │ m7p   │   996 │ Eb4  │ I      │ d5z   │   610 │ m3 - (2 * 3) / 5 │   316 │
    │ Db4 │ II     │ d8z   │  1108 │ F4   │ I      │ m6p   │   792 │ M3 - 5 / 2^2     │   386 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ G4   │ I      │ m7p   │   996 │ M3 - 5 / 2^2     │   386 │
    │ F4  │ II     │ m3p-1 │  1494 │ Ab4  │ I      │ d8z   │  1108 │ m3 - (2 * 3) / 5 │   316 │
    │ G4  │ II     │ 4p-1  │  1698 │ Bb4  │ I      │ m2z-1 │  1312 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab4 │ II     │ d5z-1 │  1810 │ C5   │ I      │ m3p-1 │  1494 │ M3 - 5 / 2^2     │   386 │
    │ Bb4 │ II     │ m6p-1 │  1992 │ Db5  │ I      │ d4z-1 │  1606 │ m3 - (2 * 3) / 5 │   316 │
    │ C5  │ II     │ m7p-1 │  2196 │ Eb5  │ I      │ d5z-1 │  1810 │ m3 - (2 * 3) / 5 │   316 │
    │ Db5 │ II     │ d8z-1 │  2308 │ F5   │ I      │ m6p-1 │  1992 │ M3 - 5 / 2^2     │   386 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ G5   │ I      │ m7p-1 │  2196 │ M3 - 5 / 2^2     │   386 │
    │ F5  │ II     │ m3p-2 │  2694 │ Ab5  │ I      │ d8z-1 │  2308 │ m3 - (2 * 3) / 5 │   316 │
    │ G5  │ II     │ 4p-2  │  2898 │ Bb5  │ I      │ m2z-2 │  2512 │ m3 - (2 * 3) / 5 │   316 │
    │ Ab5 │ II     │ d5z-2 │  3010 │ C6   │ I      │ m3p-2 │  2694 │ M3 - 5 / 2^2     │   386 │
    │ Bb5 │ II     │ m6p-2 │  3192 │ Db6  │ I      │ d4z-2 │  2806 │ m3 - (2 * 3) / 5 │   316 │
    │ C6  │ II     │ m7p-2 │  3396 │ Eb6  │ I      │ d5z-2 │  3010 │ m3 - (2 * 3) / 5 │   316 │
    │ Db6 │ II     │ d8z-2 │  3508 │ F6   │ I      │ m6p-2 │  3192 │ M3 - 5 / 2^2     │   386 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "a_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ A2  │ IV     │ M6p   │   906 │ C#3  │ III    │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ B2  │ III    │ M3z   │   386 │ D3   │ II     │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ C#3 │ III    │ A4z   │   590 │ E3   │ II     │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ D3  │ III    │ 5p    │   702 │ F#3  │ II     │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ E3  │ III    │ M6p   │   906 │ G#3  │ II     │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ F#3 │ II     │ M3z   │   386 │ A3   │ I      │ 0     │     0 │ m3 - (2 * 3) / 5 │   316 │
    │ G#3 │ II     │ A4z   │   590 │ B3   │ I      │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ A3  │ II     │ 5p    │   702 │ C#4  │ I      │ M3z   │   386 │ M3 - 5 / 2^2     │   386 │
    │ B3  │ II     │ M6z   │   884 │ D4   │ I      │ 4p    │   498 │ m3 - (2 * 3) / 5 │   316 │
    │ C#4 │ II     │ M7z   │  1088 │ E4   │ I      │ 5p    │   702 │ m3 - (2 * 3) / 5 │   316 │
    │ D4  │ II     │ 0-1   │  1200 │ F#4  │ I      │ M6z   │   884 │ M3 - 5 / 2^2     │   386 │
    │ E4  │ II     │ M2p-1 │  1404 │ G#4  │ I      │ M7z   │  1088 │ M3 - 5 / 2^2     │   386 │
    │ F#4 │ II     │ M3z-1 │  1586 │ A4   │ I      │ 0-1   │  1200 │ m3 - (2 * 3) / 5 │   316 │
    │ G#4 │ II     │ A4z-1 │  1790 │ B4   │ I      │ M2p-1 │  1404 │ m3 - (2 * 3) / 5 │   316 │
    │ A4  │ II     │ 5p-1  │  1902 │ C#5  │ I      │ M3z-1 │  1586 │ M3 - 5 / 2^2     │   386 │
    │ B4  │ II     │ M6z-1 │  2084 │ D5   │ I      │ 4p-1  │  1698 │ m3 - (2 * 3) / 5 │   316 │
    │ C#5 │ II     │ M7z-1 │  2288 │ E5   │ I      │ 5p-1  │  1902 │ m3 - (2 * 3) / 5 │   316 │
    │ D5  │ II     │ 0-2   │  2400 │ F#5  │ I      │ M6z-1 │  2084 │ M3 - 5 / 2^2     │   386 │
    │ E5  │ II     │ M2p-2 │  2604 │ G#5  │ I      │ M7z-1 │  2288 │ M3 - 5 / 2^2     │   386 │
    │ F#5 │ II     │ M3z-2 │  2786 │ A5   │ I      │ 0-2   │  2400 │ m3 - (2 * 3) / 5 │   316 │
    │ G#5 │ II     │ A4z-2 │  2990 │ B5   │ I      │ M2p-2 │  2604 │ m3 - (2 * 3) / 5 │   316 │
    │ A5  │ II     │ 5p-2  │  3102 │ C#6  │ I      │ M3z-2 │  2786 │ M3 - 5 / 2^2     │   386 │
    │ B5  │ II     │ M6z-2 │  3284 │ D6   │ I      │ 4p-2  │  2898 │ m3 - (2 * 3) / 5 │   316 │
    │ C#6 │ II     │ M7z-2 │  3488 │ E6   │ I      │ 5p-2  │  3102 │ m3 - (2 * 3) / 5 │   316 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_bz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ Bb2 │ III    │ m3z   │   316 │ D3   │ II     │ 0     │     0 │ M3 - 5 / 2^2     │   386 │
    │ C3  │ III    │ 4p    │   498 │ Eb3  │ II     │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ D3  │ III    │ 5p    │   702 │ F3   │ II     │ m3z   │   316 │ m3 - (2 * 3) / 5 │   316 │
    │ Eb3 │ III    │ m6z   │   814 │ G3   │ II     │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ F3  │ II     │ m3z   │   316 │ A3   │ I      │ 0     │     0 │ M3 - 5 / 2^2     │   386 │
    │ G3  │ II     │ 4p    │   498 │ Bb3  │ I      │ m2z   │   112 │ m3 - (2 * 3) / 5 │   316 │
    │ A3  │ II     │ 5p    │   702 │ C4   │ I      │ m3z   │   316 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb3 │ II     │ m6z   │   814 │ D4   │ I      │ 4p    │   498 │ M3 - 5 / 2^2     │   386 │
    │ D4  │ II     │ 0-1   │  1200 │ F4   │ I      │ m6z   │   814 │ m3 - (2 * 3) / 5 │   316 │
    │ Eb4 │ II     │ m2z-1 │  1312 │ G4   │ I      │ m7p   │   996 │ M3 - 5 / 2^2     │   386 │
    │ F4  │ II     │ m3z-1 │  1516 │ A4   │ I      │ 0-1   │  1200 │ M3 - 5 / 2^2     │   386 │
    │ G4  │ II     │ 4p-1  │  1698 │ Bb4  │ I      │ m2z-1 │  1312 │ m3 - (2 * 3) / 5 │   316 │
    │ A4  │ II     │ 5p-1  │  1902 │ C5   │ I      │ m3z-1 │  1516 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb4 │ II     │ m6z-1 │  2014 │ D5   │ I      │ 4p-1  │  1698 │ M3 - 5 / 2^2     │   386 │
    │ D5  │ II     │ 0-2   │  2400 │ F5   │ I      │ m6z-1 │  2014 │ m3 - (2 * 3) / 5 │   316 │
    │ Eb5 │ II     │ m2z-2 │  2512 │ G5   │ I      │ m7p-1 │  2196 │ M3 - 5 / 2^2     │   386 │
    │ F5  │ II     │ m3z-2 │  2716 │ A5   │ I      │ 0-2   │  2400 │ M3 - 5 / 2^2     │   386 │
    │ G5  │ II     │ 4p-2  │  2898 │ Bb5  │ I      │ m2z-2 │  2512 │ m3 - (2 * 3) / 5 │   316 │
    │ A5  │ II     │ 5p-2  │  3102 │ C6   │ I      │ m3z-2 │  2716 │ m3 - (2 * 3) / 5 │   316 │
    │ Bb5 │ II     │ m6z-2 │  3214 │ D6   │ I      │ 4p-2  │  2898 │ M3 - 5 / 2^2     │   386 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "b_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_bp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ B2  │ III    │ M3p   │   408 │ D#3  │ II     │ A1z   │    92 │ M3 - 5 / 2^2     │   386 │
    │ C#3 │ III    │ A4z   │   590 │ E3   │ II     │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ D#3 │ III    │ A5z   │   794 │ F#3  │ II     │ M3p   │   408 │ m3 - (2 * 3) / 5 │   316 │
    │ E3  │ III    │ M6p   │   906 │ G#3  │ II     │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ F#3 │ II     │ M3p   │   408 │ A#3  │ I      │ A1z   │    92 │ M3 - 5 / 2^2     │   386 │
    │ G#3 │ II     │ A4z   │   590 │ B3   │ I      │ M2p   │   204 │ m3 - (2 * 3) / 5 │   316 │
    │ A#3 │ II     │ A5z   │   794 │ C#4  │ I      │ M3p   │   408 │ m3 - (2 * 3) / 5 │   316 │
    │ B3  │ II     │ M6p   │   906 │ D#4  │ I      │ A4z   │   590 │ M3 - 5 / 2^2     │   386 │
    │ C#4 │ II     │ M7z   │  1088 │ E4   │ I      │ 5p    │   702 │ m3 - (2 * 3) / 5 │   316 │
    │ D#4 │ II     │ A1z-1 │  1292 │ F#4  │ I      │ M6p   │   906 │ m3 - (2 * 3) / 5 │   316 │
    │ E4  │ II     │ M2p-1 │  1404 │ G#4  │ I      │ M7z   │  1088 │ M3 - 5 / 2^2     │   386 │
    │ F#4 │ II     │ M3p-1 │  1608 │ A#4  │ I      │ A1z-1 │  1292 │ M3 - 5 / 2^2     │   386 │
    │ G#4 │ II     │ A4z-1 │  1790 │ B4   │ I      │ M2p-1 │  1404 │ m3 - (2 * 3) / 5 │   316 │
    │ A#4 │ II     │ A5z-1 │  1994 │ C#5  │ I      │ M3p-1 │  1608 │ m3 - (2 * 3) / 5 │   316 │
    │ B4  │ II     │ M6p-1 │  2106 │ D#5  │ I      │ A4z-1 │  1790 │ M3 - 5 / 2^2     │   386 │
    │ C#5 │ II     │ M7z-1 │  2288 │ E5   │ I      │ 5p-1  │  1902 │ m3 - (2 * 3) / 5 │   316 │
    │ D#5 │ II     │ A1z-2 │  2492 │ F#5  │ I      │ M6p-1 │  2106 │ m3 - (2 * 3) / 5 │   316 │
    │ E5  │ II     │ M2p-2 │  2604 │ G#5  │ I      │ M7z-1 │  2288 │ M3 - 5 / 2^2     │   386 │
    │ F#5 │ II     │ M3p-2 │  2808 │ A#5  │ I      │ A1z-2 │  2492 │ M3 - 5 / 2^2     │   386 │
    │ G#5 │ II     │ A4z-2 │  2990 │ B5   │ I      │ M2p-2 │  2604 │ m3 - (2 * 3) / 5 │   316 │
    │ A#5 │ II     │ A5z-2 │  3194 │ C#6  │ I      │ M3p-2 │  2808 │ m3 - (2 * 3) / 5 │   316 │
    │ B5  │ II     │ M6p-2 │  3306 │ D#6  │ I      │ A4z-2 │  2990 │ M3 - 5 / 2^2     │   386 │
    │ C#6 │ II     │ M7z-2 │  3488 │ E6   │ I      │ 5p-2  │  3102 │ m3 - (2 * 3) / 5 │   316 │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;
