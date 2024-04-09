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

let make_scale ~characterized_scale ~from =
  let t = force E55.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_e55 ~from
;;

let%expect_test "c_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ C2  │ IV     │ 0        │     0 │ A2   │ III    │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ D2  │ IV     │ M2-e55   │   196 │ B2   │ III    │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ E2  │ IV     │ M3-e55   │   393 │ C3   │ III    │ 4-e55    │   502 │ m6 - 37-55edo │   807 │
    │ F2  │ IV     │ 4-e55    │   502 │ D3   │ III    │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ A2  │ III    │ M2-e55   │   196 │ F3   │ II     │ m3-e55   │   305 │ m6 - 37-55edo │   807 │
    │ B2  │ III    │ M3-e55   │   393 │ G3   │ II     │ 4-e55    │   502 │ m6 - 37-55edo │   807 │
    │ C3  │ III    │ 4-e55    │   502 │ A3   │ II     │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ E3  │ II     │ M2-e55   │   196 │ C4   │ I      │ m3-e55   │   305 │ m6 - 37-55edo │   807 │
    │ F3  │ II     │ m3-e55   │   305 │ D4   │ I      │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ G3  │ II     │ 4-e55    │   502 │ E4   │ I      │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ A3  │ II     │ 5-e55    │   698 │ F4   │ I      │ m6-e55   │   807 │ m6 - 37-55edo │   807 │
    │ B3  │ II     │ M6-e55   │   895 │ G4   │ I      │ m7-e55   │  1004 │ m6 - 37-55edo │   807 │
    │ C4  │ II     │ m7-e55   │  1004 │ A4   │ I      │ 0-1      │  1200 │ M6 - 41-55edo │   895 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e55-1 │  1396 │ M6 - 41-55edo │   895 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C5   │ I      │ m3-e55-1 │  1505 │ m6 - 37-55edo │   807 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ D5   │ I      │ 4-e55-1  │  1702 │ M6 - 41-55edo │   895 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ E5   │ I      │ 5-e55-1  │  1898 │ M6 - 41-55edo │   895 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F5   │ I      │ m6-e55-1 │  2007 │ m6 - 37-55edo │   807 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ G5   │ I      │ m7-e55-1 │  2204 │ m6 - 37-55edo │   807 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ A5   │ I      │ 0-2      │  2400 │ M6 - 41-55edo │   895 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e55-2 │  2596 │ M6 - 41-55edo │   895 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C6   │ I      │ m3-e55-2 │  2705 │ m6 - 37-55edo │   807 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ D6   │ I      │ 4-e55-2  │  2902 │ M6 - 41-55edo │   895 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ E6   │ I      │ 5-e55-2  │  3098 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_c_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ C#2 │ IV     │ A1-e55   │    87 │ A#2  │ III    │ A2-e55   │   284 │ M6 - 41-55edo │   895 │
    │ D#2 │ IV     │ A2-e55   │   284 │ B#2  │ III    │ A3-e55   │   480 │ M6 - 41-55edo │   895 │
    │ E#2 │ IV     │ A3-e55   │   480 │ C#3  │ III    │ A4-e55   │   589 │ m6 - 37-55edo │   807 │
    │ F#2 │ IV     │ A4-e55   │   589 │ D#3  │ III    │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ G#2 │ III    │ A1-e55   │    87 │ E#3  │ II     │ A2-e55   │   284 │ M6 - 41-55edo │   895 │
    │ A#2 │ III    │ A2-e55   │   284 │ F#3  │ II     │ M3-e55   │   393 │ m6 - 37-55edo │   807 │
    │ B#2 │ III    │ A3-e55   │   480 │ G#3  │ II     │ A4-e55   │   589 │ m6 - 37-55edo │   807 │
    │ C#3 │ III    │ A4-e55   │   589 │ A#3  │ II     │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ D#3 │ II     │ A1-e55   │    87 │ B#3  │ I      │ A2-e55   │   284 │ M6 - 41-55edo │   895 │
    │ E#3 │ II     │ A2-e55   │   284 │ C#4  │ I      │ M3-e55   │   393 │ m6 - 37-55edo │   807 │
    │ F#3 │ II     │ M3-e55   │   393 │ D#4  │ I      │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ G#3 │ II     │ A4-e55   │   589 │ E#4  │ I      │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ A#3 │ II     │ A5-e55   │   785 │ F#4  │ I      │ M6-e55   │   895 │ m6 - 37-55edo │   807 │
    │ C#4 │ II     │ M7-e55   │  1091 │ A#4  │ I      │ A1-e55-1 │  1287 │ M6 - 41-55edo │   895 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ B#4  │ I      │ A2-e55-1 │  1484 │ M6 - 41-55edo │   895 │
    │ E#4 │ II     │ A2-e55-1 │  1484 │ C#5  │ I      │ M3-e55-1 │  1593 │ m6 - 37-55edo │   807 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ D#5  │ I      │ A4-e55-1 │  1789 │ M6 - 41-55edo │   895 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ E#5  │ I      │ A5-e55-1 │  1985 │ M6 - 41-55edo │   895 │
    │ A#4 │ II     │ A5-e55-1 │  1985 │ F#5  │ I      │ M6-e55-1 │  2095 │ m6 - 37-55edo │   807 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ A#5  │ I      │ A1-e55-2 │  2487 │ M6 - 41-55edo │   895 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ B#5  │ I      │ A2-e55-2 │  2684 │ M6 - 41-55edo │   895 │
    │ E#5 │ II     │ A2-e55-2 │  2684 │ C#6  │ I      │ M3-e55-2 │  2793 │ m6 - 37-55edo │   807 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ D#6  │ I      │ A4-e55-2 │  2989 │ M6 - 41-55edo │   895 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ E#6  │ I      │ A5-e55-2 │  3185 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_d_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Db2 │ IV     │ m2-e55   │   109 │ Bb2  │ III    │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ Eb2 │ IV     │ m3-e55   │   305 │ C3   │ III    │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ F2  │ IV     │ 4-e55    │   502 │ Db3  │ III    │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ Gb2 │ IV     │ d5-e55   │   611 │ Eb3  │ III    │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Ab2 │ III    │ m2-e55   │   109 │ F3   │ II     │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ Bb2 │ III    │ m3-e55   │   305 │ Gb3  │ II     │ d4-e55   │   415 │ m6 - 37-55edo │   807 │
    │ C3  │ III    │ 4-e55    │   502 │ Ab3  │ II     │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ Db3 │ III    │ d5-e55   │   611 │ Bb3  │ II     │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Eb3 │ II     │ m2-e55   │   109 │ C4   │ I      │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ F3  │ II     │ m3-e55   │   305 │ Db4  │ I      │ d4-e55   │   415 │ m6 - 37-55edo │   807 │
    │ Gb3 │ II     │ d4-e55   │   415 │ Eb4  │ I      │ d5-e55   │   611 │ M6 - 41-55edo │   895 │
    │ Ab3 │ II     │ d5-e55   │   611 │ F4   │ I      │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Bb3 │ II     │ m6-e55   │   807 │ Gb4  │ I      │ d7-e55   │   916 │ m6 - 37-55edo │   807 │
    │ C4  │ II     │ m7-e55   │  1004 │ Ab4  │ I      │ d8-e55   │  1113 │ m6 - 37-55edo │   807 │
    │ Db4 │ II     │ d8-e55   │  1113 │ Bb4  │ I      │ m2-e55-1 │  1309 │ M6 - 41-55edo │   895 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ C5   │ I      │ m3-e55-1 │  1505 │ M6 - 41-55edo │   895 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Db5  │ I      │ d4-e55-1 │  1615 │ m6 - 37-55edo │   807 │
    │ Gb4 │ II     │ d4-e55-1 │  1615 │ Eb5  │ I      │ d5-e55-1 │  1811 │ M6 - 41-55edo │   895 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ F5   │ I      │ m6-e55-1 │  2007 │ M6 - 41-55edo │   895 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ Gb5  │ I      │ d7-e55-1 │  2116 │ m6 - 37-55edo │   807 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m6 - 37-55edo │   807 │
    │ Db5 │ II     │ d8-e55-1 │  2313 │ Bb5  │ I      │ m2-e55-2 │  2509 │ M6 - 41-55edo │   895 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ C6   │ I      │ m3-e55-2 │  2705 │ M6 - 41-55edo │   895 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Db6  │ I      │ d4-e55-2 │  2815 │ m6 - 37-55edo │   807 │
    │ Gb5 │ II     │ d4-e55-2 │  2815 │ Eb6  │ I      │ d5-e55-2 │  3011 │ M6 - 41-55edo │   895 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ F6   │ I      │ m6-e55-2 │  3207 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ D2  │ IV     │ M2-e55   │   196 │ B2   │ III    │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ E2  │ IV     │ M3-e55   │   393 │ C#3  │ III    │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ F#2 │ IV     │ A4-e55   │   589 │ D3   │ III    │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ A2  │ III    │ M2-e55   │   196 │ F#3  │ II     │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ B2  │ III    │ M3-e55   │   393 │ G3   │ II     │ 4-e55    │   502 │ m6 - 37-55edo │   807 │
    │ C#3 │ III    │ A4-e55   │   589 │ A3   │ II     │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ E3  │ II     │ M2-e55   │   196 │ C#4  │ I      │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ F#3 │ II     │ M3-e55   │   393 │ D4   │ I      │ 4-e55    │   502 │ m6 - 37-55edo │   807 │
    │ G3  │ II     │ 4-e55    │   502 │ E4   │ I      │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ A3  │ II     │ 5-e55    │   698 │ F#4  │ I      │ M6-e55   │   895 │ M6 - 41-55edo │   895 │
    │ B3  │ II     │ M6-e55   │   895 │ G4   │ I      │ m7-e55   │  1004 │ m6 - 37-55edo │   807 │
    │ C#4 │ II     │ M7-e55   │  1091 │ A4   │ I      │ 0-1      │  1200 │ m6 - 37-55edo │   807 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e55-1 │  1396 │ M6 - 41-55edo │   895 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C#5  │ I      │ M3-e55-1 │  1593 │ M6 - 41-55edo │   895 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ D5   │ I      │ 4-e55-1  │  1702 │ m6 - 37-55edo │   807 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ E5   │ I      │ 5-e55-1  │  1898 │ M6 - 41-55edo │   895 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F#5  │ I      │ M6-e55-1 │  2095 │ M6 - 41-55edo │   895 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ G5   │ I      │ m7-e55-1 │  2204 │ m6 - 37-55edo │   807 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ A5   │ I      │ 0-2      │  2400 │ m6 - 37-55edo │   807 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e55-2 │  2596 │ M6 - 41-55edo │   895 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C#6  │ I      │ M3-e55-2 │  2793 │ M6 - 41-55edo │   895 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ D6   │ I      │ 4-e55-2  │  2902 │ m6 - 37-55edo │   807 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ E6   │ I      │ 5-e55-2  │  3098 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_e_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3-e55   │   305 │ C3   │ III    │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ F2  │ IV     │ 4-e55    │   502 │ D3   │ III    │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ G2  │ III    │ 0        │     0 │ Eb3  │ II     │ m2-e55   │   109 │ m6 - 37-55edo │   807 │
    │ Ab2 │ III    │ m2-e55   │   109 │ F3   │ II     │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ Bb2 │ III    │ m3-e55   │   305 │ G3   │ II     │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ C3  │ III    │ 4-e55    │   502 │ Ab3  │ II     │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e55   │   109 │ m6 - 37-55edo │   807 │
    │ Eb3 │ II     │ m2-e55   │   109 │ C4   │ I      │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ F3  │ II     │ m3-e55   │   305 │ D4   │ I      │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ G3  │ II     │ 4-e55    │   502 │ Eb4  │ I      │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ Ab3 │ II     │ d5-e55   │   611 │ F4   │ I      │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Bb3 │ II     │ m6-e55   │   807 │ G4   │ I      │ m7-e55   │  1004 │ M6 - 41-55edo │   895 │
    │ C4  │ II     │ m7-e55   │  1004 │ Ab4  │ I      │ d8-e55   │  1113 │ m6 - 37-55edo │   807 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m6 - 37-55edo │   807 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ C5   │ I      │ m3-e55-1 │  1505 │ M6 - 41-55edo │   895 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ D5   │ I      │ 4-e55-1  │  1702 │ M6 - 41-55edo │   895 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m6 - 37-55edo │   807 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ F5   │ I      │ m6-e55-1 │  2007 │ M6 - 41-55edo │   895 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ G5   │ I      │ m7-e55-1 │  2204 │ M6 - 41-55edo │   895 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m6 - 37-55edo │   807 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m6 - 37-55edo │   807 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ C6   │ I      │ m3-e55-2 │  2705 │ M6 - 41-55edo │   895 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ D6   │ I      │ 4-e55-2  │  2902 │ M6 - 41-55edo │   895 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m6 - 37-55edo │   807 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ F6   │ I      │ m6-e55-2 │  3207 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3-e55   │   393 │ C#3  │ III    │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ F#2 │ IV     │ A4-e55   │   589 │ D#3  │ III    │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ G#2 │ III    │ A1-e55   │    87 │ E3   │ II     │ M2-e55   │   196 │ m6 - 37-55edo │   807 │
    │ A2  │ III    │ M2-e55   │   196 │ F#3  │ II     │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ B2  │ III    │ M3-e55   │   393 │ G#3  │ II     │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ C#3 │ III    │ A4-e55   │   589 │ A3   │ II     │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ D#3 │ II     │ A1-e55   │    87 │ B3   │ I      │ M2-e55   │   196 │ m6 - 37-55edo │   807 │
    │ E3  │ II     │ M2-e55   │   196 │ C#4  │ I      │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ F#3 │ II     │ M3-e55   │   393 │ D#4  │ I      │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ G#3 │ II     │ A4-e55   │   589 │ E4   │ I      │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ A3  │ II     │ 5-e55    │   698 │ F#4  │ I      │ M6-e55   │   895 │ M6 - 41-55edo │   895 │
    │ B3  │ II     │ M6-e55   │   895 │ G#4  │ I      │ M7-e55   │  1091 │ M6 - 41-55edo │   895 │
    │ C#4 │ II     │ M7-e55   │  1091 │ A4   │ I      │ 0-1      │  1200 │ m6 - 37-55edo │   807 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ B4   │ I      │ M2-e55-1 │  1396 │ m6 - 37-55edo │   807 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C#5  │ I      │ M3-e55-1 │  1593 │ M6 - 41-55edo │   895 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ D#5  │ I      │ A4-e55-1 │  1789 │ M6 - 41-55edo │   895 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ E5   │ I      │ 5-e55-1  │  1898 │ m6 - 37-55edo │   807 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F#5  │ I      │ M6-e55-1 │  2095 │ M6 - 41-55edo │   895 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ G#5  │ I      │ M7-e55-1 │  2291 │ M6 - 41-55edo │   895 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ A5   │ I      │ 0-2      │  2400 │ m6 - 37-55edo │   807 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ B5   │ I      │ M2-e55-2 │  2596 │ m6 - 37-55edo │   807 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C#6  │ I      │ M3-e55-2 │  2793 │ M6 - 41-55edo │   895 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ D#6  │ I      │ A4-e55-2 │  2989 │ M6 - 41-55edo │   895 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ E6   │ I      │ 5-e55-2  │  3098 │ m6 - 37-55edo │   807 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e55    │   502 │ D3   │ III    │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ A2  │ III    │ M2-e55   │   196 │ F3   │ II     │ m3-e55   │   305 │ m6 - 37-55edo │   807 │
    │ Bb2 │ III    │ m3-e55   │   305 │ G3   │ II     │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ C3  │ III    │ 4-e55    │   502 │ A3   │ II     │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e55   │   109 │ m6 - 37-55edo │   807 │
    │ E3  │ II     │ M2-e55   │   196 │ C4   │ I      │ m3-e55   │   305 │ m6 - 37-55edo │   807 │
    │ F3  │ II     │ m3-e55   │   305 │ D4   │ I      │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ G3  │ II     │ 4-e55    │   502 │ E4   │ I      │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ A3  │ II     │ 5-e55    │   698 │ F4   │ I      │ m6-e55   │   807 │ m6 - 37-55edo │   807 │
    │ Bb3 │ II     │ m6-e55   │   807 │ G4   │ I      │ m7-e55   │  1004 │ M6 - 41-55edo │   895 │
    │ C4  │ II     │ m7-e55   │  1004 │ A4   │ I      │ 0-1      │  1200 │ M6 - 41-55edo │   895 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m6 - 37-55edo │   807 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C5   │ I      │ m3-e55-1 │  1505 │ m6 - 37-55edo │   807 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ D5   │ I      │ 4-e55-1  │  1702 │ M6 - 41-55edo │   895 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ E5   │ I      │ 5-e55-1  │  1898 │ M6 - 41-55edo │   895 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F5   │ I      │ m6-e55-1 │  2007 │ m6 - 37-55edo │   807 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ G5   │ I      │ m7-e55-1 │  2204 │ M6 - 41-55edo │   895 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ A5   │ I      │ 0-2      │  2400 │ M6 - 41-55edo │   895 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m6 - 37-55edo │   807 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C6   │ I      │ m3-e55-2 │  2705 │ m6 - 37-55edo │   807 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ D6   │ I      │ 4-e55-2  │  2902 │ M6 - 41-55edo │   895 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ E6   │ I      │ 5-e55-2  │  3098 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_f_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ A4-e55   │   589 │ D#3  │ III    │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ G#2 │ III    │ A1-e55   │    87 │ E#3  │ II     │ A2-e55   │   284 │ M6 - 41-55edo │   895 │
    │ A#2 │ III    │ A2-e55   │   284 │ F#3  │ II     │ M3-e55   │   393 │ m6 - 37-55edo │   807 │
    │ B2  │ III    │ M3-e55   │   393 │ G#3  │ II     │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ C#3 │ III    │ A4-e55   │   589 │ A#3  │ II     │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ D#3 │ II     │ A1-e55   │    87 │ B3   │ I      │ M2-e55   │   196 │ m6 - 37-55edo │   807 │
    │ E#3 │ II     │ A2-e55   │   284 │ C#4  │ I      │ M3-e55   │   393 │ m6 - 37-55edo │   807 │
    │ F#3 │ II     │ M3-e55   │   393 │ D#4  │ I      │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ G#3 │ II     │ A4-e55   │   589 │ E#4  │ I      │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ A#3 │ II     │ A5-e55   │   785 │ F#4  │ I      │ M6-e55   │   895 │ m6 - 37-55edo │   807 │
    │ B3  │ II     │ M6-e55   │   895 │ G#4  │ I      │ M7-e55   │  1091 │ M6 - 41-55edo │   895 │
    │ C#4 │ II     │ M7-e55   │  1091 │ A#4  │ I      │ A1-e55-1 │  1287 │ M6 - 41-55edo │   895 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ B4   │ I      │ M2-e55-1 │  1396 │ m6 - 37-55edo │   807 │
    │ E#4 │ II     │ A2-e55-1 │  1484 │ C#5  │ I      │ M3-e55-1 │  1593 │ m6 - 37-55edo │   807 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ D#5  │ I      │ A4-e55-1 │  1789 │ M6 - 41-55edo │   895 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ E#5  │ I      │ A5-e55-1 │  1985 │ M6 - 41-55edo │   895 │
    │ A#4 │ II     │ A5-e55-1 │  1985 │ F#5  │ I      │ M6-e55-1 │  2095 │ m6 - 37-55edo │   807 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ G#5  │ I      │ M7-e55-1 │  2291 │ M6 - 41-55edo │   895 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ A#5  │ I      │ A1-e55-2 │  2487 │ M6 - 41-55edo │   895 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ B5   │ I      │ M2-e55-2 │  2596 │ m6 - 37-55edo │   807 │
    │ E#5 │ II     │ A2-e55-2 │  2684 │ C#6  │ I      │ M3-e55-2 │  2793 │ m6 - 37-55edo │   807 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ D#6  │ I      │ A4-e55-2 │  2989 │ M6 - 41-55edo │   895 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ E#6  │ I      │ A5-e55-2 │  3185 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_g_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ d5-e55   │   611 │ Eb3  │ III    │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Ab2 │ III    │ m2-e55   │   109 │ F3   │ II     │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ Bb2 │ III    │ m3-e55   │   305 │ Gb3  │ II     │ d4-e55   │   415 │ m6 - 37-55edo │   807 │
    │ Cb3 │ III    │ d4-e55   │   415 │ Ab3  │ II     │ d5-e55   │   611 │ M6 - 41-55edo │   895 │
    │ Db3 │ III    │ d5-e55   │   611 │ Bb3  │ II     │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Eb3 │ II     │ m2-e55   │   109 │ Cb4  │ I      │ d3-e55   │   218 │ m6 - 37-55edo │   807 │
    │ F3  │ II     │ m3-e55   │   305 │ Db4  │ I      │ d4-e55   │   415 │ m6 - 37-55edo │   807 │
    │ Gb3 │ II     │ d4-e55   │   415 │ Eb4  │ I      │ d5-e55   │   611 │ M6 - 41-55edo │   895 │
    │ Ab3 │ II     │ d5-e55   │   611 │ F4   │ I      │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Bb3 │ II     │ m6-e55   │   807 │ Gb4  │ I      │ d7-e55   │   916 │ m6 - 37-55edo │   807 │
    │ Cb4 │ II     │ d7-e55   │   916 │ Ab4  │ I      │ d8-e55   │  1113 │ M6 - 41-55edo │   895 │
    │ Db4 │ II     │ d8-e55   │  1113 │ Bb4  │ I      │ m2-e55-1 │  1309 │ M6 - 41-55edo │   895 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ Cb5  │ I      │ d3-e55-1 │  1418 │ m6 - 37-55edo │   807 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Db5  │ I      │ d4-e55-1 │  1615 │ m6 - 37-55edo │   807 │
    │ Gb4 │ II     │ d4-e55-1 │  1615 │ Eb5  │ I      │ d5-e55-1 │  1811 │ M6 - 41-55edo │   895 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ F5   │ I      │ m6-e55-1 │  2007 │ M6 - 41-55edo │   895 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ Gb5  │ I      │ d7-e55-1 │  2116 │ m6 - 37-55edo │   807 │
    │ Cb5 │ II     │ d7-e55-1 │  2116 │ Ab5  │ I      │ d8-e55-1 │  2313 │ M6 - 41-55edo │   895 │
    │ Db5 │ II     │ d8-e55-1 │  2313 │ Bb5  │ I      │ m2-e55-2 │  2509 │ M6 - 41-55edo │   895 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ Cb6  │ I      │ d3-e55-2 │  2618 │ m6 - 37-55edo │   807 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Db6  │ I      │ d4-e55-2 │  2815 │ m6 - 37-55edo │   807 │
    │ Gb5 │ II     │ d4-e55-2 │  2815 │ Eb6  │ I      │ d5-e55-2 │  3011 │ M6 - 41-55edo │   895 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ F6   │ I      │ m6-e55-2 │  3207 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e55    │   502 │ D3   │ III    │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ A2  │ III    │ M2-e55   │   196 │ F3   │ II     │ m3-e55   │   305 │ m6 - 37-55edo │   807 │
    │ Bb2 │ III    │ m3-e55   │   305 │ G3   │ II     │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ C3  │ III    │ 4-e55    │   502 │ A3   │ II     │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e55   │   109 │ m6 - 37-55edo │   807 │
    │ E3  │ II     │ M2-e55   │   196 │ C4   │ I      │ m3-e55   │   305 │ m6 - 37-55edo │   807 │
    │ F3  │ II     │ m3-e55   │   305 │ D4   │ I      │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ G3  │ II     │ 4-e55    │   502 │ E4   │ I      │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ A3  │ II     │ 5-e55    │   698 │ F4   │ I      │ m6-e55   │   807 │ m6 - 37-55edo │   807 │
    │ Bb3 │ II     │ m6-e55   │   807 │ G4   │ I      │ m7-e55   │  1004 │ M6 - 41-55edo │   895 │
    │ C4  │ II     │ m7-e55   │  1004 │ A4   │ I      │ 0-1      │  1200 │ M6 - 41-55edo │   895 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m6 - 37-55edo │   807 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C5   │ I      │ m3-e55-1 │  1505 │ m6 - 37-55edo │   807 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ D5   │ I      │ 4-e55-1  │  1702 │ M6 - 41-55edo │   895 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ E5   │ I      │ 5-e55-1  │  1898 │ M6 - 41-55edo │   895 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F5   │ I      │ m6-e55-1 │  2007 │ m6 - 37-55edo │   807 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ G5   │ I      │ m7-e55-1 │  2204 │ M6 - 41-55edo │   895 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ A5   │ I      │ 0-2      │  2400 │ M6 - 41-55edo │   895 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m6 - 37-55edo │   807 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C6   │ I      │ m3-e55-2 │  2705 │ m6 - 37-55edo │   807 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ D6   │ I      │ 4-e55-2  │  2902 │ M6 - 41-55edo │   895 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ E6   │ I      │ 5-e55-2  │  3098 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_a_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Ab2 │ III    │ m2-e55   │   109 │ F3   │ II     │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ Bb2 │ III    │ m3-e55   │   305 │ G3   │ II     │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ C3  │ III    │ 4-e55    │   502 │ Ab3  │ II     │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ Db3 │ III    │ d5-e55   │   611 │ Bb3  │ II     │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Eb3 │ II     │ m2-e55   │   109 │ C4   │ I      │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ F3  │ II     │ m3-e55   │   305 │ Db4  │ I      │ d4-e55   │   415 │ m6 - 37-55edo │   807 │
    │ G3  │ II     │ 4-e55    │   502 │ Eb4  │ I      │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ Ab3 │ II     │ d5-e55   │   611 │ F4   │ I      │ m6-e55   │   807 │ M6 - 41-55edo │   895 │
    │ Bb3 │ II     │ m6-e55   │   807 │ G4   │ I      │ m7-e55   │  1004 │ M6 - 41-55edo │   895 │
    │ C4  │ II     │ m7-e55   │  1004 │ Ab4  │ I      │ d8-e55   │  1113 │ m6 - 37-55edo │   807 │
    │ Db4 │ II     │ d8-e55   │  1113 │ Bb4  │ I      │ m2-e55-1 │  1309 │ M6 - 41-55edo │   895 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ C5   │ I      │ m3-e55-1 │  1505 │ M6 - 41-55edo │   895 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Db5  │ I      │ d4-e55-1 │  1615 │ m6 - 37-55edo │   807 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m6 - 37-55edo │   807 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ F5   │ I      │ m6-e55-1 │  2007 │ M6 - 41-55edo │   895 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ G5   │ I      │ m7-e55-1 │  2204 │ M6 - 41-55edo │   895 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m6 - 37-55edo │   807 │
    │ Db5 │ II     │ d8-e55-1 │  2313 │ Bb5  │ I      │ m2-e55-2 │  2509 │ M6 - 41-55edo │   895 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ C6   │ I      │ m3-e55-2 │  2705 │ M6 - 41-55edo │   895 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Db6  │ I      │ d4-e55-2 │  2815 │ m6 - 37-55edo │   807 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m6 - 37-55edo │   807 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ F6   │ I      │ m6-e55-2 │  3207 │ M6 - 41-55edo │   895 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ A2  │ III    │ M2-e55   │   196 │ F#3  │ II     │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ B2  │ III    │ M3-e55   │   393 │ G#3  │ II     │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ C#3 │ III    │ A4-e55   │   589 │ A3   │ II     │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e55   │   196 │ M6 - 41-55edo │   895 │
    │ E3  │ II     │ M2-e55   │   196 │ C#4  │ I      │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ F#3 │ II     │ M3-e55   │   393 │ D4   │ I      │ 4-e55    │   502 │ m6 - 37-55edo │   807 │
    │ G#3 │ II     │ A4-e55   │   589 │ E4   │ I      │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ A3  │ II     │ 5-e55    │   698 │ F#4  │ I      │ M6-e55   │   895 │ M6 - 41-55edo │   895 │
    │ B3  │ II     │ M6-e55   │   895 │ G#4  │ I      │ M7-e55   │  1091 │ M6 - 41-55edo │   895 │
    │ C#4 │ II     │ M7-e55   │  1091 │ A4   │ I      │ 0-1      │  1200 │ m6 - 37-55edo │   807 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e55-1 │  1396 │ M6 - 41-55edo │   895 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C#5  │ I      │ M3-e55-1 │  1593 │ M6 - 41-55edo │   895 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ D5   │ I      │ 4-e55-1  │  1702 │ m6 - 37-55edo │   807 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ E5   │ I      │ 5-e55-1  │  1898 │ m6 - 37-55edo │   807 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F#5  │ I      │ M6-e55-1 │  2095 │ M6 - 41-55edo │   895 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ G#5  │ I      │ M7-e55-1 │  2291 │ M6 - 41-55edo │   895 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ A5   │ I      │ 0-2      │  2400 │ m6 - 37-55edo │   807 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e55-2 │  2596 │ M6 - 41-55edo │   895 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C#6  │ I      │ M3-e55-2 │  2793 │ M6 - 41-55edo │   895 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ D6   │ I      │ 4-e55-2  │  2902 │ m6 - 37-55edo │   807 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ E6   │ I      │ 5-e55-2  │  3098 │ m6 - 37-55edo │   807 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_b_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3-e55   │   305 │ G3   │ II     │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ C3  │ III    │ 4-e55    │   502 │ A3   │ II     │ 5-e55    │   698 │ M6 - 41-55edo │   895 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e55   │   109 │ m6 - 37-55edo │   807 │
    │ Eb3 │ II     │ m2-e55   │   109 │ C4   │ I      │ m3-e55   │   305 │ M6 - 41-55edo │   895 │
    │ F3  │ II     │ m3-e55   │   305 │ D4   │ I      │ 4-e55    │   502 │ M6 - 41-55edo │   895 │
    │ G3  │ II     │ 4-e55    │   502 │ Eb4  │ I      │ d5-e55   │   611 │ m6 - 37-55edo │   807 │
    │ A3  │ II     │ 5-e55    │   698 │ F4   │ I      │ m6-e55   │   807 │ m6 - 37-55edo │   807 │
    │ Bb3 │ II     │ m6-e55   │   807 │ G4   │ I      │ m7-e55   │  1004 │ M6 - 41-55edo │   895 │
    │ C4  │ II     │ m7-e55   │  1004 │ A4   │ I      │ 0-1      │  1200 │ M6 - 41-55edo │   895 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m6 - 37-55edo │   807 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ C5   │ I      │ m3-e55-1 │  1505 │ M6 - 41-55edo │   895 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ D5   │ I      │ 4-e55-1  │  1702 │ M6 - 41-55edo │   895 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m6 - 37-55edo │   807 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ F5   │ I      │ m6-e55-1 │  2007 │ m6 - 37-55edo │   807 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ G5   │ I      │ m7-e55-1 │  2204 │ M6 - 41-55edo │   895 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ A5   │ I      │ 0-2      │  2400 │ M6 - 41-55edo │   895 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m6 - 37-55edo │   807 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ C6   │ I      │ m3-e55-2 │  2705 │ M6 - 41-55edo │   895 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ D6   │ I      │ 4-e55-2  │  2902 │ M6 - 41-55edo │   895 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m6 - 37-55edo │   807 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ F6   │ I      │ m6-e55-2 │  3207 │ m6 - 37-55edo │   807 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major" =
  let t = force E55.t in
  let scale = make_major_scale ~from:Scales.lower_b in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3-e55   │   393 │ G#3  │ II     │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ C#3 │ III    │ A4-e55   │   589 │ A#3  │ II     │ A5-e55   │   785 │ M6 - 41-55edo │   895 │
    │ D#3 │ II     │ A1-e55   │    87 │ B3   │ I      │ M2-e55   │   196 │ m6 - 37-55edo │   807 │
    │ E3  │ II     │ M2-e55   │   196 │ C#4  │ I      │ M3-e55   │   393 │ M6 - 41-55edo │   895 │
    │ F#3 │ II     │ M3-e55   │   393 │ D#4  │ I      │ A4-e55   │   589 │ M6 - 41-55edo │   895 │
    │ G#3 │ II     │ A4-e55   │   589 │ E4   │ I      │ 5-e55    │   698 │ m6 - 37-55edo │   807 │
    │ A#3 │ II     │ A5-e55   │   785 │ F#4  │ I      │ M6-e55   │   895 │ m6 - 37-55edo │   807 │
    │ B3  │ II     │ M6-e55   │   895 │ G#4  │ I      │ M7-e55   │  1091 │ M6 - 41-55edo │   895 │
    │ C#4 │ II     │ M7-e55   │  1091 │ A#4  │ I      │ A1-e55-1 │  1287 │ M6 - 41-55edo │   895 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ B4   │ I      │ M2-e55-1 │  1396 │ m6 - 37-55edo │   807 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ C#5  │ I      │ M3-e55-1 │  1593 │ M6 - 41-55edo │   895 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ D#5  │ I      │ A4-e55-1 │  1789 │ M6 - 41-55edo │   895 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ E5   │ I      │ 5-e55-1  │  1898 │ m6 - 37-55edo │   807 │
    │ A#4 │ II     │ A5-e55-1 │  1985 │ F#5  │ I      │ M6-e55-1 │  2095 │ m6 - 37-55edo │   807 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ G#5  │ I      │ M7-e55-1 │  2291 │ M6 - 41-55edo │   895 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ A#5  │ I      │ A1-e55-2 │  2487 │ M6 - 41-55edo │   895 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ B5   │ I      │ M2-e55-2 │  2596 │ m6 - 37-55edo │   807 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ C#6  │ I      │ M3-e55-2 │  2793 │ M6 - 41-55edo │   895 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ D#6  │ I      │ A4-e55-2 │  2989 │ M6 - 41-55edo │   895 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ E6   │ I      │ 5-e55-2  │  3098 │ m6 - 37-55edo │   807 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;
