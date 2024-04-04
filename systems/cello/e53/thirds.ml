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
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:13
  ; to_ =
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:14
  }
;;

let make_scale ~characterized_scale ~from ~adjustment =
  let t = force E53.t in
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
    ~characterized_scale:Characterized_scale.major_pythagorean_e53
    ~from
    ~adjustment:None
;;

let make_major_just_scale ~from =
  make_scale
    ~characterized_scale:Characterized_scale.major_just_e53
    ~from
    ~adjustment:(Some adjustment)
;;

let%expect_test "c_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3z-e53   │ 385   │ G2   │ III    │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ A2   │ III    │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ A2  │ IV     │ M6z-e53   │ 883   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ B2  │ III    │ M3z-e53   │ 385   │ D3   │ II     │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3z-e53   │ 317   │ m3 - 14-53edo │ 317   │
    │ E3  │ III    │ M6z-e53   │ 883   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ F3  │ III    │ m7p-e53   │ 996   │ A3   │ II     │ 5z-e53    │ 679   │ M3 - 17-53edo │ 385   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ A3  │ II     │ 5z-e53    │ 679   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 14-53edo │ 317   │
    │ B3  │ II     │ M6z-e53   │ 883   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5z-e53    │ 679   │ M3 - 17-53edo │ 385   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6z-e53   │ 815   │ m3 - 14-53edo │ 317   │
    │ E4  │ II     │ M2z-e53-1 │ 1381  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 14-53edo │ 317   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ A4   │ I      │ 8z-e53    │ 1177  │ M3 - 17-53edo │ 385   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M3 - 17-53edo │ 385   │
    │ A4  │ II     │ 5z-e53-1  │ 1879  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 14-53edo │ 317   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 14-53edo │ 317   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5z-e53-1  │ 1879  │ M3 - 17-53edo │ 385   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6z-e53-1 │ 2015  │ m3 - 14-53edo │ 317   │
    │ E5  │ II     │ M2z-e53-2 │ 2581  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 14-53edo │ 317   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ A5   │ I      │ 8z-e53-1  │ 2377  │ M3 - 17-53edo │ 385   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M3 - 17-53edo │ 385   │
    │ A5  │ II     │ 5z-e53-2  │ 3079  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 14-53edo │ 317   │
    │ B5  │ II     │ M6z-e53-2 │ 3283  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 14-53edo │ 317   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5z-e53-2  │ 3079  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ G2   │ III    │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ A2   │ III    │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D3   │ II     │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ F3  │ II     │ m3p-e53   │ 294   │ A3   │ I      │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5p-e53    │ 702   │ M3 - 18-53edo │ 408   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6p-e53   │ 792   │ m3 - 13-53edo │ 294   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 13-53edo │ 294   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ A4   │ I      │ 0-1       │ 1200  │ M3 - 18-53edo │ 408   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M3 - 18-53edo │ 408   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 13-53edo │ 294   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 13-53edo │ 294   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M3 - 18-53edo │ 408   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m3 - 13-53edo │ 294   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 13-53edo │ 294   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ A5   │ I      │ 0-2       │ 2400  │ M3 - 18-53edo │ 408   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M3 - 18-53edo │ 408   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 13-53edo │ 294   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 13-53edo │ 294   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_cp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E#2 │ IV     │ 4z-e53    │ 521   │ G#2  │ III    │ m2z-e53   │ 113   │ m3 - 13-53edo │ 294   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ A#2  │ III    │ m3z-e53   │ 317   │ M3 - 18-53edo │ 408   │
    │ G#2 │ IV     │ m6z-e53   │ 815   │ B#2  │ III    │ 4z-e53    │ 521   │ M3 - 18-53edo │ 408   │
    │ A#2 │ IV     │ m7z-e53   │ 1019  │ C#3  │ III    │ d5z-e53   │ 611   │ m3 - 13-53edo │ 294   │
    │ B#2 │ III    │ 4z-e53    │ 521   │ D#3  │ II     │ m2z-e53   │ 113   │ m3 - 13-53edo │ 294   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E#3  │ II     │ m3z-e53   │ 317   │ M3 - 18-53edo │ 408   │
    │ D#3 │ III    │ m6z-e53   │ 815   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ E#3 │ III    │ m7z-e53   │ 1019  │ G#3  │ II     │ d5z-e53   │ 611   │ m3 - 13-53edo │ 294   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A#3  │ I      │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ B#3  │ I      │ m3z-e53   │ 317   │ M3 - 18-53edo │ 408   │
    │ A#3 │ II     │ m6z-e53   │ 815   │ C#4  │ I      │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ B#3 │ II     │ m7z-e53   │ 1019  │ D#4  │ I      │ d5z-e53   │ 611   │ m3 - 13-53edo │ 294   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E#4  │ I      │ m6z-e53   │ 815   │ M3 - 18-53edo │ 408   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 13-53edo │ 294   │
    │ E#4 │ II     │ m3z-e53-1 │ 1517  │ G#4  │ I      │ M7p-e53   │ 1109  │ m3 - 13-53edo │ 294   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A#4  │ I      │ m2z-e53-1 │ 1313  │ M3 - 18-53edo │ 408   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ B#4  │ I      │ m3z-e53-1 │ 1517  │ M3 - 18-53edo │ 408   │
    │ A#4 │ II     │ m6z-e53-1 │ 2015  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 13-53edo │ 294   │
    │ B#4 │ II     │ m7z-e53-1 │ 2219  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ m3 - 13-53edo │ 294   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E#5  │ I      │ m6z-e53-1 │ 2015  │ M3 - 18-53edo │ 408   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 13-53edo │ 294   │
    │ E#5 │ II     │ m3z-e53-2 │ 2717  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 13-53edo │ 294   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A#5  │ I      │ m2z-e53-2 │ 2513  │ M3 - 18-53edo │ 408   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ B#5  │ I      │ m3z-e53-2 │ 2717  │ M3 - 18-53edo │ 408   │
    │ A#5 │ II     │ m6z-e53-2 │ 3215  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 13-53edo │ 294   │
    │ B#5 │ II     │ m7z-e53-2 │ 3419  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ m3 - 13-53edo │ 294   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E#6  │ I      │ m6z-e53-2 │ 3215  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_dp_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4p-e53    │ 498   │ Ab2  │ III    │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ Gb2 │ IV     │ A4z-e53   │ 589   │ Bb2  │ III    │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Ab2 │ IV     │ m6p-e53   │ 792   │ C3   │ III    │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ Bb2 │ IV     │ m7p-e53   │ 996   │ Db3  │ III    │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ Db3 │ III    │ A4z-e53   │ 589   │ F3   │ II     │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ Gb3  │ II     │ M3z-e53   │ 385   │ m3 - 13-53edo │ 294   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ Gb3 │ II     │ M3z-e53   │ 385   │ Bb3  │ I      │ A1z-e53   │ 91    │ M3 - 18-53edo │ 408   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ C4   │ I      │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Db4  │ I      │ M3z-e53   │ 385   │ m3 - 13-53edo │ 294   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ Db4 │ II     │ M7z-e53   │ 1087  │ F4   │ I      │ m6p-e53   │ 792   │ M3 - 18-53edo │ 408   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ Gb4  │ I      │ M6z-e53   │ 883   │ m3 - 13-53edo │ 294   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7z-e53   │ 1087  │ m3 - 13-53edo │ 294   │
    │ Gb4 │ II     │ M3z-e53-1 │ 1585  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M3 - 18-53edo │ 408   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M3 - 18-53edo │ 408   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Db5  │ I      │ M3z-e53-1 │ 1585  │ m3 - 13-53edo │ 294   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m3 - 13-53edo │ 294   │
    │ Db5 │ II     │ M7z-e53-1 │ 2287  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M3 - 18-53edo │ 408   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ Gb5  │ I      │ M6z-e53-1 │ 2083  │ m3 - 13-53edo │ 294   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m3 - 13-53edo │ 294   │
    │ Gb5 │ II     │ M3z-e53-2 │ 2785  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M3 - 18-53edo │ 408   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M3 - 18-53edo │ 408   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ Db6  │ I      │ M3z-e53-2 │ 2785  │ m3 - 13-53edo │ 294   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m3 - 13-53edo │ 294   │
    │ Db6 │ II     │ M7z-e53-2 │ 3487  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_dz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4p-e53    │ 498   │ Ab2  │ III    │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ Gb2 │ IV     │ d5z-e53   │ 611   │ Bb2  │ III    │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Ab2 │ IV     │ m6z-e53   │ 815   │ C3   │ III    │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ Bb2 │ IV     │ m7p-e53   │ 996   │ Db3  │ III    │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ Db3 │ III    │ d5z-e53   │ 611   │ F3   │ II     │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ Gb3  │ II     │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ Gb3 │ II     │ M3p-e53   │ 408   │ Bb3  │ I      │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ C4   │ I      │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Db4  │ I      │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ Db4 │ II     │ M7p-e53   │ 1109  │ F4   │ I      │ m6p-e53   │ 792   │ M3 - 17-53edo │ 385   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ Gb4  │ I      │ M6p-e53   │ 906   │ m3 - 14-53edo │ 317   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7p-e53   │ 1109  │ m3 - 14-53edo │ 317   │
    │ Gb4 │ II     │ M3p-e53-1 │ 1608  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M3 - 17-53edo │ 385   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M3 - 17-53edo │ 385   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Db5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 14-53edo │ 317   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m3 - 14-53edo │ 317   │
    │ Db5 │ II     │ M7p-e53-1 │ 2309  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M3 - 17-53edo │ 385   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ Gb5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 14-53edo │ 317   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 14-53edo │ 317   │
    │ Gb5 │ II     │ M3p-e53-2 │ 2808  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M3 - 17-53edo │ 385   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M3 - 17-53edo │ 385   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ Db6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 14-53edo │ 317   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m3 - 14-53edo │ 317   │
    │ Db6 │ II     │ M7p-e53-2 │ 3509  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ G2   │ III    │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ A2   │ III    │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C#3  │ III    │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D3   │ II     │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F#3  │ II     │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A3   │ I      │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C#4  │ I      │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 13-53edo │ 294   │
    │ D4  │ II     │ 0-1       │ 1200  │ F#4  │ I      │ M6p-e53   │ 906   │ M3 - 18-53edo │ 408   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 13-53edo │ 294   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 13-53edo │ 294   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M3 - 18-53edo │ 408   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M3 - 18-53edo │ 408   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 13-53edo │ 294   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 13-53edo │ 294   │
    │ D5  │ II     │ 0-2       │ 2400  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M3 - 18-53edo │ 408   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 13-53edo │ 294   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 13-53edo │ 294   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M3 - 18-53edo │ 408   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M3 - 18-53edo │ 408   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 13-53edo │ 294   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 13-53edo │ 294   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3z-e53   │ 385   │ G2   │ III    │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ F#2 │ IV     │ A4z-e53   │ 589   │ A2   │ III    │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C#3  │ III    │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ B2  │ III    │ M3z-e53   │ 385   │ D3   │ II     │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F#3  │ II     │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ E3  │ III    │ M6z-e53   │ 883   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ A3   │ I      │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C#4  │ I      │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ B3  │ II     │ M6z-e53   │ 883   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 14-53edo │ 317   │
    │ D4  │ II     │ 0-1       │ 1200  │ F#4  │ I      │ M6z-e53   │ 883   │ M3 - 17-53edo │ 385   │
    │ E4  │ II     │ M2z-e53-1 │ 1381  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 14-53edo │ 317   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 14-53edo │ 317   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M3 - 17-53edo │ 385   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M3 - 17-53edo │ 385   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 14-53edo │ 317   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 14-53edo │ 317   │
    │ D5  │ II     │ 0-2       │ 2400  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M3 - 17-53edo │ 385   │
    │ E5  │ II     │ M2z-e53-2 │ 2581  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 14-53edo │ 317   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 14-53edo │ 317   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M3 - 17-53edo │ 385   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M3 - 17-53edo │ 385   │
    │ B5  │ II     │ M6z-e53-2 │ 3283  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 14-53edo │ 317   │
    │ C#6 │ II     │ M7z-e53-2 │ 3487  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 14-53edo │ 317   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_ez_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3z-e53   │ 317   │ G2   │ III    │ 0         │ 0     │ M3 - 17-53edo │ 385   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ Ab2  │ III    │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ Bb2  │ III    │ m3z-e53   │ 317   │ m3 - 14-53edo │ 317   │
    │ Ab2 │ IV     │ m6z-e53   │ 815   │ C3   │ III    │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ Bb2 │ III    │ m3z-e53   │ 317   │ D3   │ II     │ 0         │ 0     │ M3 - 17-53edo │ 385   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3z-e53   │ 317   │ m3 - 14-53edo │ 317   │
    │ Eb3 │ III    │ m6z-e53   │ 815   │ G3   │ II     │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ C4   │ I      │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Bb3 │ II     │ m6z-e53   │ 815   │ D4   │ I      │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6z-e53   │ 815   │ m3 - 14-53edo │ 317   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ G4   │ I      │ m7p-e53   │ 996   │ M3 - 17-53edo │ 385   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7p-e53   │ 1109  │ m3 - 14-53edo │ 317   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ m2z-e53-1 │ 1313  │ m3 - 14-53edo │ 317   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M3 - 17-53edo │ 385   │
    │ Bb4 │ II     │ m6z-e53-1 │ 2015  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M3 - 17-53edo │ 385   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m3 - 14-53edo │ 317   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6z-e53-1 │ 2015  │ m3 - 14-53edo │ 317   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M3 - 17-53edo │ 385   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 14-53edo │ 317   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ m2z-e53-2 │ 2513  │ m3 - 14-53edo │ 317   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M3 - 17-53edo │ 385   │
    │ Bb5 │ II     │ m6z-e53-2 │ 3215  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M3 - 17-53edo │ 385   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m3 - 14-53edo │ 317   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_ep_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3p-e53   │ 294   │ G2   │ III    │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ Ab2  │ III    │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ Bb2  │ III    │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ Ab2 │ IV     │ m6p-e53   │ 792   │ C3   │ III    │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ D3   │ II     │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ G3   │ II     │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ C4   │ I      │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ D4   │ I      │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6p-e53   │ 792   │ m3 - 13-53edo │ 294   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ G4   │ I      │ m7p-e53   │ 996   │ M3 - 18-53edo │ 408   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7z-e53   │ 1087  │ m3 - 13-53edo │ 294   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m3 - 13-53edo │ 294   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M3 - 18-53edo │ 408   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M3 - 18-53edo │ 408   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m3 - 13-53edo │ 294   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m3 - 13-53edo │ 294   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M3 - 18-53edo │ 408   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m3 - 13-53edo │ 294   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m3 - 13-53edo │ 294   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M3 - 18-53edo │ 408   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M3 - 18-53edo │ 408   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m3 - 13-53edo │ 294   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ G#2  │ III    │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ A2   │ III    │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ G#2 │ IV     │ m6z-e53   │ 815   │ B2   │ III    │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C#3  │ III    │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D#3  │ II     │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ D#3 │ III    │ m6z-e53   │ 815   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G#3  │ II     │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A3   │ I      │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C#4  │ I      │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D#4  │ I      │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 13-53edo │ 294   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 13-53edo │ 294   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G#4  │ I      │ M7p-e53   │ 1109  │ M3 - 18-53edo │ 408   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 13-53edo │ 294   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 13-53edo │ 294   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M3 - 18-53edo │ 408   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M3 - 18-53edo │ 408   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 13-53edo │ 294   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 13-53edo │ 294   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M3 - 18-53edo │ 408   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 13-53edo │ 294   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 13-53edo │ 294   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M3 - 18-53edo │ 408   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M3 - 18-53edo │ 408   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 13-53edo │ 294   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ G#2  │ III    │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ F#2 │ IV     │ A4z-e53   │ 589   │ A2   │ III    │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ G#2 │ IV     │ m6p-e53   │ 792   │ B2   │ III    │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C#3  │ III    │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D#3  │ II     │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ D#3 │ III    │ m6p-e53   │ 792   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G#3  │ II     │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ A3   │ I      │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C#4  │ I      │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D#4  │ I      │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 14-53edo │ 317   │
    │ D#4 │ II     │ A1z-e53-1 │ 1291  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 14-53edo │ 317   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G#4  │ I      │ M7z-e53   │ 1087  │ M3 - 17-53edo │ 385   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 14-53edo │ 317   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 14-53edo │ 317   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M3 - 17-53edo │ 385   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D#5  │ I      │ A4z-e53-1 │ 1789  │ M3 - 17-53edo │ 385   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 14-53edo │ 317   │
    │ D#5 │ II     │ A1z-e53-2 │ 2491  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 14-53edo │ 317   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M3 - 17-53edo │ 385   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 14-53edo │ 317   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 14-53edo │ 317   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M3 - 17-53edo │ 385   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D#6  │ I      │ A4z-e53-2 │ 2989  │ M3 - 17-53edo │ 385   │
    │ C#6 │ II     │ M7z-e53-2 │ 3487  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 14-53edo │ 317   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "fp_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_fp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4p-e53    │ 498   │ A2   │ III    │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ Bb2  │ III    │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ D3   │ II     │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ F3  │ II     │ m3p-e53   │ 294   │ A3   │ I      │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ D4   │ I      │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5p-e53    │ 702   │ M3 - 18-53edo │ 408   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6p-e53   │ 792   │ m3 - 13-53edo │ 294   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 13-53edo │ 294   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ A4   │ I      │ 0-1       │ 1200  │ M3 - 18-53edo │ 408   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m3 - 13-53edo │ 294   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 13-53edo │ 294   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M3 - 18-53edo │ 408   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M3 - 18-53edo │ 408   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m3 - 13-53edo │ 294   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 13-53edo │ 294   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ A5   │ I      │ 0-2       │ 2400  │ M3 - 18-53edo │ 408   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m3 - 13-53edo │ 294   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 13-53edo │ 294   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M3 - 18-53edo │ 408   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "fp_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────────┬───────┬──────┬────────┬─────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos     │ Cents │ High │ String │ Pos     │ Cents │ Interval      │ Cents │
    ├─────┼────────┼─────────┼───────┼──────┼────────┼─────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4p-e53  │ 498   │ A2   │ III    │ M2z-e53 │ 181   │ M3 - 17-53edo │ 385   │
    │ G2  │ IV     │ 5p-e53  │ 702   │ Bb2  │ III    │ m3z-e53 │ 317   │ m3 - 14-53edo │ 317   │
    │ A2  │ IV     │ M6z-e53 │ 883   │ C3   │ III    │ 4p-e53  │ 498   │ m3 - 14-53edo │ 317   │
    │ Bb2 │ IV     │ m7p-e53 │ 996   │ D3   │ III    │ 5z-e53  │ 679   │ M3 - 17-53edo │ 385   │
    │ C3  │ III    │ 4p-e53  │ 498   │ E3   │ II     │ M2z-e53 │ 181   │ M3 - 17-53edo │ 385   │
    │ D3  │ III    │ 5z-e53  │ 679   │ F3   │ II     │ m3p-e53 │ 294   │ m3 - 14-53edo │ 317   │
    │ E3  │ III    │ M6z-e53 │ 883   │ G3   │ II     │ 4p-e53  │ 498   │ m3 - 14-53edo │ 317   │
    │ F3  │ III    │ m7p-e53 │ 996   │ A3   │ II     │ 5z-e53  │ 679   │ M3 - 17-53edo │ 385   │
    │ G3  │ II     │ 4p-e53  │ 498   │ Bb3  │ I      │ m2z-e53 │ 113   │ m3 - 14-53edo │ 317   │
    │ A3  │ II     │ 5z-e53  │ 679   │ C4   │ I      │ m3p-e53 │ 294   │ m3 - 14-53edo │ 317   │
    └─────┴────────┴─────────┴───────┴──────┴────────┴─────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "fz_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_fz in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────┬───────┬──────┬────────┬─────┬───────┬──────────┬───────┐
    │ Low │ String │ Pos │ Cents │ High │ String │ Pos │ Cents │ Interval │ Cents │
    ├┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┤
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘ |}]
;;

let%expect_test "f_sharp_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_fp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ d5z-e53   │ 611   │ A#2  │ III    │ m3z-e53   │ 317   │ M3 - 18-53edo │ 408   │
    │ G#2 │ IV     │ m6z-e53   │ 815   │ B2   │ III    │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ A#2 │ IV     │ m7z-e53   │ 1019  │ C#3  │ III    │ d5z-e53   │ 611   │ m3 - 13-53edo │ 294   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D#3  │ II     │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E#3  │ II     │ m3z-e53   │ 317   │ M3 - 18-53edo │ 408   │
    │ D#3 │ III    │ m6z-e53   │ 815   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ E#3 │ III    │ m7z-e53   │ 1019  │ G#3  │ II     │ d5z-e53   │ 611   │ m3 - 13-53edo │ 294   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A#3  │ I      │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ A#3 │ II     │ m6z-e53   │ 815   │ C#4  │ I      │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D#4  │ I      │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E#4  │ I      │ m6z-e53   │ 815   │ M3 - 18-53edo │ 408   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 13-53edo │ 294   │
    │ E#4 │ II     │ m3z-e53-1 │ 1517  │ G#4  │ I      │ M7p-e53   │ 1109  │ m3 - 13-53edo │ 294   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A#4  │ I      │ m2z-e53-1 │ 1313  │ M3 - 18-53edo │ 408   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 13-53edo │ 294   │
    │ A#4 │ II     │ m6z-e53-1 │ 2015  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 13-53edo │ 294   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M3 - 18-53edo │ 408   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E#5  │ I      │ m6z-e53-1 │ 2015  │ M3 - 18-53edo │ 408   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 13-53edo │ 294   │
    │ E#5 │ II     │ m3z-e53-2 │ 2717  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 13-53edo │ 294   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A#5  │ I      │ m2z-e53-2 │ 2513  │ M3 - 18-53edo │ 408   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 13-53edo │ 294   │
    │ A#5 │ II     │ m6z-e53-2 │ 3215  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 13-53edo │ 294   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M3 - 18-53edo │ 408   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E#6  │ I      │ m6z-e53-2 │ 3215  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ d5z-e53   │ 611   │ A#2  │ III    │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ G#2 │ IV     │ m6p-e53   │ 792   │ B2   │ III    │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ A#2 │ IV     │ m7p-e53   │ 996   │ C#3  │ III    │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D#3  │ II     │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E#3  │ II     │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ D#3 │ III    │ m6p-e53   │ 792   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ E#3 │ III    │ m7p-e53   │ 996   │ G#3  │ II     │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A#3  │ I      │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ A#3 │ II     │ m6p-e53   │ 792   │ C#4  │ I      │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D#4  │ I      │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E#4  │ I      │ m6p-e53   │ 792   │ M3 - 17-53edo │ 385   │
    │ D#4 │ II     │ A1z-e53-1 │ 1291  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 14-53edo │ 317   │
    │ E#4 │ II     │ m3p-e53-1 │ 1494  │ G#4  │ I      │ M7p-e53   │ 1109  │ m3 - 14-53edo │ 317   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A#4  │ I      │ A1z-e53-1 │ 1291  │ M3 - 17-53edo │ 385   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 14-53edo │ 317   │
    │ A#4 │ II     │ m6p-e53-1 │ 1992  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 14-53edo │ 317   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D#5  │ I      │ A4z-e53-1 │ 1789  │ M3 - 17-53edo │ 385   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E#5  │ I      │ m6p-e53-1 │ 1992  │ M3 - 17-53edo │ 385   │
    │ D#5 │ II     │ A1z-e53-2 │ 2491  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 14-53edo │ 317   │
    │ E#5 │ II     │ m3p-e53-2 │ 2694  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 14-53edo │ 317   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A#5  │ I      │ A1z-e53-2 │ 2491  │ M3 - 17-53edo │ 385   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 14-53edo │ 317   │
    │ A#5 │ II     │ m6p-e53-2 │ 3192  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 14-53edo │ 317   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D#6  │ I      │ A4z-e53-2 │ 2989  │ M3 - 17-53edo │ 385   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E#6  │ I      │ m6p-e53-2 │ 3192  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_gp_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ A4z-e53   │ 589   │ Bb2  │ III    │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Ab2 │ IV     │ m6p-e53   │ 792   │ Cb3  │ III    │ M3z-e53   │ 385   │ m3 - 13-53edo │ 294   │
    │ Bb2 │ IV     │ m7p-e53   │ 996   │ Db3  │ III    │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ Cb3 │ III    │ M3z-e53   │ 385   │ Eb3  │ II     │ A1z-e53   │ 91    │ M3 - 18-53edo │ 408   │
    │ Db3 │ III    │ A4z-e53   │ 589   │ F3   │ II     │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ Gb3  │ II     │ M3z-e53   │ 385   │ m3 - 13-53edo │ 294   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ Gb3 │ II     │ M3z-e53   │ 385   │ Bb3  │ I      │ A1z-e53   │ 91    │ M3 - 18-53edo │ 408   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ Cb4  │ I      │ M2z-e53   │ 181   │ m3 - 13-53edo │ 294   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Db4  │ I      │ M3z-e53   │ 385   │ m3 - 13-53edo │ 294   │
    │ Cb4 │ II     │ M6z-e53   │ 883   │ Eb4  │ I      │ A4z-e53   │ 589   │ M3 - 18-53edo │ 408   │
    │ Db4 │ II     │ M7z-e53   │ 1087  │ F4   │ I      │ m6p-e53   │ 792   │ M3 - 18-53edo │ 408   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ Gb4  │ I      │ M6z-e53   │ 883   │ m3 - 13-53edo │ 294   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7z-e53   │ 1087  │ m3 - 13-53edo │ 294   │
    │ Gb4 │ II     │ M3z-e53-1 │ 1585  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M3 - 18-53edo │ 408   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ Cb5  │ I      │ M2z-e53-1 │ 1381  │ m3 - 13-53edo │ 294   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Db5  │ I      │ M3z-e53-1 │ 1585  │ m3 - 13-53edo │ 294   │
    │ Cb5 │ II     │ M6z-e53-1 │ 2083  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ M3 - 18-53edo │ 408   │
    │ Db5 │ II     │ M7z-e53-1 │ 2287  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M3 - 18-53edo │ 408   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ Gb5  │ I      │ M6z-e53-1 │ 2083  │ m3 - 13-53edo │ 294   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m3 - 13-53edo │ 294   │
    │ Gb5 │ II     │ M3z-e53-2 │ 2785  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M3 - 18-53edo │ 408   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ Cb6  │ I      │ M2z-e53-2 │ 2581  │ m3 - 13-53edo │ 294   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ Db6  │ I      │ M3z-e53-2 │ 2785  │ m3 - 13-53edo │ 294   │
    │ Cb6 │ II     │ M6z-e53-2 │ 3283  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ M3 - 18-53edo │ 408   │
    │ Db6 │ II     │ M7z-e53-2 │ 3487  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_gz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ d5z-e53   │ 611   │ Bb2  │ III    │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Ab2 │ IV     │ m6p-e53   │ 792   │ Cb3  │ III    │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ Bb2 │ IV     │ m7p-e53   │ 996   │ Db3  │ III    │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ Cb3 │ III    │ M3p-e53   │ 408   │ Eb3  │ II     │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ Db3 │ III    │ d5z-e53   │ 611   │ F3   │ II     │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ Gb3  │ II     │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ Gb3 │ II     │ M3p-e53   │ 408   │ Bb3  │ I      │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ Cb4  │ I      │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Db4  │ I      │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ Cb4 │ II     │ M6p-e53   │ 906   │ Eb4  │ I      │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ Db4 │ II     │ M7p-e53   │ 1109  │ F4   │ I      │ m6p-e53   │ 792   │ M3 - 17-53edo │ 385   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ Gb4  │ I      │ M6p-e53   │ 906   │ m3 - 14-53edo │ 317   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7p-e53   │ 1109  │ m3 - 14-53edo │ 317   │
    │ Gb4 │ II     │ M3p-e53-1 │ 1608  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M3 - 17-53edo │ 385   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ Cb5  │ I      │ M2p-e53-1 │ 1404  │ m3 - 14-53edo │ 317   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Db5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 14-53edo │ 317   │
    │ Cb5 │ II     │ M6p-e53-1 │ 2106  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ M3 - 17-53edo │ 385   │
    │ Db5 │ II     │ M7p-e53-1 │ 2309  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M3 - 17-53edo │ 385   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ Gb5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 14-53edo │ 317   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 14-53edo │ 317   │
    │ Gb5 │ II     │ M3p-e53-2 │ 2808  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M3 - 17-53edo │ 385   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ Cb6  │ I      │ M2p-e53-2 │ 2604  │ m3 - 14-53edo │ 317   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ Db6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 14-53edo │ 317   │
    │ Cb6 │ II     │ M6p-e53-2 │ 3306  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ M3 - 17-53edo │ 385   │
    │ Db6 │ II     │ M7p-e53-2 │ 3509  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_g in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D3   │ II     │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F#3  │ II     │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A3   │ I      │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5p-e53    │ 702   │ M3 - 18-53edo │ 408   │
    │ D4  │ II     │ 0-1       │ 1200  │ F#4  │ I      │ M6p-e53   │ 906   │ M3 - 18-53edo │ 408   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 13-53edo │ 294   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 13-53edo │ 294   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M3 - 18-53edo │ 408   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 13-53edo │ 294   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 13-53edo │ 294   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M3 - 18-53edo │ 408   │
    │ D5  │ II     │ 0-2       │ 2400  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M3 - 18-53edo │ 408   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 13-53edo │ 294   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 13-53edo │ 294   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M3 - 18-53edo │ 408   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 13-53edo │ 294   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 13-53edo │ 294   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_g in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ A2  │ IV     │ M6z-e53   │ 883   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ B2  │ III    │ M3z-e53   │ 385   │ D3   │ II     │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F#3  │ II     │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ E3  │ III    │ M6z-e53   │ 883   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ A3   │ I      │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ A3  │ II     │ 5z-e53    │ 679   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 14-53edo │ 317   │
    │ B3  │ II     │ M6z-e53   │ 883   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5z-e53    │ 679   │ M3 - 17-53edo │ 385   │
    │ D4  │ II     │ 0-1       │ 1200  │ F#4  │ I      │ M6z-e53   │ 883   │ M3 - 17-53edo │ 385   │
    │ E4  │ II     │ M2z-e53-1 │ 1381  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 14-53edo │ 317   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 14-53edo │ 317   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M3 - 17-53edo │ 385   │
    │ A4  │ II     │ 5z-e53-1  │ 1879  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 14-53edo │ 317   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 14-53edo │ 317   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5z-e53-1  │ 1879  │ M3 - 17-53edo │ 385   │
    │ D5  │ II     │ 0-2       │ 2400  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M3 - 17-53edo │ 385   │
    │ E5  │ II     │ M2z-e53-2 │ 2581  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 14-53edo │ 317   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 14-53edo │ 317   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M3 - 17-53edo │ 385   │
    │ A5  │ II     │ 5z-e53-2  │ 3079  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 14-53edo │ 317   │
    │ B5  │ II     │ M6z-e53-2 │ 3283  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 14-53edo │ 317   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5z-e53-2  │ 3079  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_ap_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Ab2 │ IV     │ m6p-e53   │ 792   │ C3   │ III    │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ Bb2 │ IV     │ m7p-e53   │ 996   │ Db3  │ III    │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ Db3 │ III    │ A4z-e53   │ 589   │ F3   │ II     │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ G3   │ II     │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ C4   │ I      │ m3p-e53   │ 294   │ M3 - 18-53edo │ 408   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Db4  │ I      │ M3z-e53   │ 385   │ m3 - 13-53edo │ 294   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ Db4 │ II     │ M7z-e53   │ 1087  │ F4   │ I      │ m6p-e53   │ 792   │ M3 - 18-53edo │ 408   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ G4   │ I      │ m7p-e53   │ 996   │ M3 - 18-53edo │ 408   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7z-e53   │ 1087  │ m3 - 13-53edo │ 294   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m3 - 13-53edo │ 294   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M3 - 18-53edo │ 408   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Db5  │ I      │ M3z-e53-1 │ 1585  │ m3 - 13-53edo │ 294   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m3 - 13-53edo │ 294   │
    │ Db5 │ II     │ M7z-e53-1 │ 2287  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M3 - 18-53edo │ 408   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M3 - 18-53edo │ 408   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m3 - 13-53edo │ 294   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m3 - 13-53edo │ 294   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M3 - 18-53edo │ 408   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ Db6  │ I      │ M3z-e53-2 │ 2785  │ m3 - 13-53edo │ 294   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m3 - 13-53edo │ 294   │
    │ Db6 │ II     │ M7z-e53-2 │ 3487  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_az_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Ab2 │ IV     │ m6z-e53   │ 815   │ C3   │ III    │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ Bb2 │ IV     │ m7p-e53   │ 996   │ Db3  │ III    │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ Db3 │ III    │ d5z-e53   │ 611   │ F3   │ II     │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Eb3 │ III    │ m6z-e53   │ 815   │ G3   │ II     │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ F3  │ III    │ m7p-e53   │ 996   │ Ab3  │ II     │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ C4   │ I      │ m3p-e53   │ 294   │ M3 - 17-53edo │ 385   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Db4  │ I      │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ Db4 │ II     │ M7p-e53   │ 1109  │ F4   │ I      │ m6p-e53   │ 792   │ M3 - 17-53edo │ 385   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ G4   │ I      │ m7p-e53   │ 996   │ M3 - 17-53edo │ 385   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Ab4  │ I      │ M7p-e53   │ 1109  │ m3 - 14-53edo │ 317   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ m2z-e53-1 │ 1313  │ m3 - 14-53edo │ 317   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M3 - 17-53edo │ 385   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Db5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 14-53edo │ 317   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m3 - 14-53edo │ 317   │
    │ Db5 │ II     │ M7p-e53-1 │ 2309  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M3 - 17-53edo │ 385   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M3 - 17-53edo │ 385   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m3 - 14-53edo │ 317   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ m2z-e53-2 │ 2513  │ m3 - 14-53edo │ 317   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M3 - 17-53edo │ 385   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ Db6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 14-53edo │ 317   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m3 - 14-53edo │ 317   │
    │ Db6 │ II     │ M7p-e53-2 │ 3509  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ A2  │ IV     │ M6p-e53   │ 906   │ C#3  │ III    │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D3   │ II     │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F#3  │ II     │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G#3  │ II     │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A3   │ I      │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C#4  │ I      │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 13-53edo │ 294   │
    │ D4  │ II     │ 0-1       │ 1200  │ F#4  │ I      │ M6p-e53   │ 906   │ M3 - 18-53edo │ 408   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G#4  │ I      │ M7p-e53   │ 1109  │ M3 - 18-53edo │ 408   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 13-53edo │ 294   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 13-53edo │ 294   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M3 - 18-53edo │ 408   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 13-53edo │ 294   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 13-53edo │ 294   │
    │ D5  │ II     │ 0-2       │ 2400  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M3 - 18-53edo │ 408   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M3 - 18-53edo │ 408   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 13-53edo │ 294   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 13-53edo │ 294   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M3 - 18-53edo │ 408   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 13-53edo │ 294   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 13-53edo │ 294   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ A2  │ IV     │ M6p-e53   │ 906   │ C#3  │ III    │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ B2  │ III    │ M3z-e53   │ 385   │ D3   │ II     │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F#3  │ II     │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G#3  │ II     │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ A3   │ I      │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C#4  │ I      │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ B3  │ II     │ M6z-e53   │ 883   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 14-53edo │ 317   │
    │ D4  │ II     │ 0-1       │ 1200  │ F#4  │ I      │ M6z-e53   │ 883   │ M3 - 17-53edo │ 385   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G#4  │ I      │ M7z-e53   │ 1087  │ M3 - 17-53edo │ 385   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ A4   │ I      │ 0-1       │ 1200  │ m3 - 14-53edo │ 317   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 14-53edo │ 317   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M3 - 17-53edo │ 385   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 14-53edo │ 317   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 14-53edo │ 317   │
    │ D5  │ II     │ 0-2       │ 2400  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M3 - 17-53edo │ 385   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M3 - 17-53edo │ 385   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ A5   │ I      │ 0-2       │ 2400  │ m3 - 14-53edo │ 317   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 14-53edo │ 317   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M3 - 17-53edo │ 385   │
    │ B5  │ II     │ M6z-e53-2 │ 3283  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 14-53edo │ 317   │
    │ C#6 │ II     │ M7z-e53-2 │ 3487  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 14-53edo │ 317   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_bp_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3p-e53   │ 294   │ D3   │ II     │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ Eb3 │ III    │ m6p-e53   │ 792   │ G3   │ II     │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ F3  │ II     │ m3p-e53   │ 294   │ A3   │ I      │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ A1z-e53   │ 91    │ m3 - 13-53edo │ 294   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ D4   │ I      │ 4p-e53    │ 498   │ M3 - 18-53edo │ 408   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ A4z-e53   │ 589   │ m3 - 13-53edo │ 294   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6p-e53   │ 792   │ m3 - 13-53edo │ 294   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ G4   │ I      │ m7p-e53   │ 996   │ M3 - 18-53edo │ 408   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ A4   │ I      │ 0-1       │ 1200  │ M3 - 18-53edo │ 408   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m3 - 13-53edo │ 294   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 13-53edo │ 294   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M3 - 18-53edo │ 408   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m3 - 13-53edo │ 294   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m3 - 13-53edo │ 294   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M3 - 18-53edo │ 408   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ A5   │ I      │ 0-2       │ 2400  │ M3 - 18-53edo │ 408   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m3 - 13-53edo │ 294   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 13-53edo │ 294   │
    │ Bb5 │ II     │ m6p-e53-2 │ 3192  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M3 - 18-53edo │ 408   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m3 - 13-53edo │ 294   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_bz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3z-e53   │ 317   │ D3   │ II     │ 0         │ 0     │ M3 - 17-53edo │ 385   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Eb3  │ II     │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3z-e53   │ 317   │ m3 - 14-53edo │ 317   │
    │ Eb3 │ III    │ m6z-e53   │ 815   │ G3   │ II     │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ F3  │ II     │ m3z-e53   │ 317   │ A3   │ I      │ 0         │ 0     │ M3 - 17-53edo │ 385   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Bb3  │ I      │ m2z-e53   │ 113   │ m3 - 14-53edo │ 317   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C4   │ I      │ m3z-e53   │ 317   │ m3 - 14-53edo │ 317   │
    │ Bb3 │ II     │ m6z-e53   │ 815   │ D4   │ I      │ 4p-e53    │ 498   │ M3 - 17-53edo │ 385   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Eb4  │ I      │ d5z-e53   │ 611   │ m3 - 14-53edo │ 317   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6z-e53   │ 815   │ m3 - 14-53edo │ 317   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ G4   │ I      │ m7p-e53   │ 996   │ M3 - 17-53edo │ 385   │
    │ F4  │ II     │ m3z-e53-1 │ 1517  │ A4   │ I      │ 0-1       │ 1200  │ M3 - 17-53edo │ 385   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Bb4  │ I      │ m2z-e53-1 │ 1313  │ m3 - 14-53edo │ 317   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C5   │ I      │ m3z-e53-1 │ 1517  │ m3 - 14-53edo │ 317   │
    │ Bb4 │ II     │ m6z-e53-1 │ 2015  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M3 - 17-53edo │ 385   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m3 - 14-53edo │ 317   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6z-e53-1 │ 2015  │ m3 - 14-53edo │ 317   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M3 - 17-53edo │ 385   │
    │ F5  │ II     │ m3z-e53-2 │ 2717  │ A5   │ I      │ 0-2       │ 2400  │ M3 - 17-53edo │ 385   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Bb5  │ I      │ m2z-e53-2 │ 2513  │ m3 - 14-53edo │ 317   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C6   │ I      │ m3z-e53-2 │ 2717  │ m3 - 14-53edo │ 317   │
    │ Bb5 │ II     │ m6z-e53-2 │ 3215  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M3 - 17-53edo │ 385   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m3 - 14-53edo │ 317   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_bp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3p-e53   │ 408   │ D#3  │ II     │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ D#3 │ III    │ m6z-e53   │ 815   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G#3  │ II     │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A#3  │ I      │ m2z-e53   │ 113   │ M3 - 18-53edo │ 408   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 13-53edo │ 294   │
    │ A#3 │ II     │ m6z-e53   │ 815   │ C#4  │ I      │ M3p-e53   │ 408   │ m3 - 13-53edo │ 294   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D#4  │ I      │ d5z-e53   │ 611   │ M3 - 18-53edo │ 408   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 13-53edo │ 294   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 13-53edo │ 294   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G#4  │ I      │ M7p-e53   │ 1109  │ M3 - 18-53edo │ 408   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A#4  │ I      │ m2z-e53-1 │ 1313  │ M3 - 18-53edo │ 408   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 13-53edo │ 294   │
    │ A#4 │ II     │ m6z-e53-1 │ 2015  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 13-53edo │ 294   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M3 - 18-53edo │ 408   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 13-53edo │ 294   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 13-53edo │ 294   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M3 - 18-53edo │ 408   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A#5  │ I      │ m2z-e53-2 │ 2513  │ M3 - 18-53edo │ 408   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 13-53edo │ 294   │
    │ A#5 │ II     │ m6z-e53-2 │ 3215  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 13-53edo │ 294   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M3 - 18-53edo │ 408   │
    │ C#6 │ II     │ M7p-e53-2 │ 3509  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 13-53edo │ 294   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_bp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3p-e53   │ 408   │ D#3  │ II     │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ E3   │ II     │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ D#3 │ III    │ m6p-e53   │ 792   │ F#3  │ II     │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G#3  │ II     │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ A#3  │ I      │ A1z-e53   │ 91    │ M3 - 17-53edo │ 385   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ B3   │ I      │ M2p-e53   │ 204   │ m3 - 14-53edo │ 317   │
    │ A#3 │ II     │ m6p-e53   │ 792   │ C#4  │ I      │ M3p-e53   │ 408   │ m3 - 14-53edo │ 317   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D#4  │ I      │ A4z-e53   │ 589   │ M3 - 17-53edo │ 385   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ E4   │ I      │ 5p-e53    │ 702   │ m3 - 14-53edo │ 317   │
    │ D#4 │ II     │ A1z-e53-1 │ 1291  │ F#4  │ I      │ M6p-e53   │ 906   │ m3 - 14-53edo │ 317   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G#4  │ I      │ M7z-e53   │ 1087  │ M3 - 17-53edo │ 385   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ A#4  │ I      │ A1z-e53-1 │ 1291  │ M3 - 17-53edo │ 385   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m3 - 14-53edo │ 317   │
    │ A#4 │ II     │ m6p-e53-1 │ 1992  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m3 - 14-53edo │ 317   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D#5  │ I      │ A4z-e53-1 │ 1789  │ M3 - 17-53edo │ 385   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m3 - 14-53edo │ 317   │
    │ D#5 │ II     │ A1z-e53-2 │ 2491  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m3 - 14-53edo │ 317   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M3 - 17-53edo │ 385   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ A#5  │ I      │ A1z-e53-2 │ 2491  │ M3 - 17-53edo │ 385   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m3 - 14-53edo │ 317   │
    │ A#5 │ II     │ m6p-e53-2 │ 3192  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m3 - 14-53edo │ 317   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D#6  │ I      │ A4z-e53-2 │ 2989  │ M3 - 17-53edo │ 385   │
    │ C#6 │ II     │ M7z-e53-2 │ 3487  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m3 - 14-53edo │ 317   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;
