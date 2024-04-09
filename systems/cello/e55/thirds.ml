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
    ~interval_number:Third
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
    │ E2  │ IV     │ M3-e55   │   393 │ G2   │ III    │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ F2  │ IV     │ 4-e55    │   502 │ A2   │ III    │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ G2  │ IV     │ 5-e55    │   698 │ B2   │ III    │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ A2  │ IV     │ M6-e55   │   895 │ C3   │ III    │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ B2  │ III    │ M3-e55   │   393 │ D3   │ II     │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ C3  │ III    │ 4-e55    │   502 │ E3   │ II     │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ D3  │ III    │ 5-e55    │   698 │ F3   │ II     │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ E3  │ III    │ M6-e55   │   895 │ G3   │ II     │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ F3  │ II     │ m3-e55   │   305 │ A3   │ I      │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ G3  │ II     │ 4-e55    │   502 │ B3   │ I      │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ A3  │ II     │ 5-e55    │   698 │ C4   │ I      │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ B3  │ II     │ M6-e55   │   895 │ D4   │ I      │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ C4  │ II     │ m7-e55   │  1004 │ E4   │ I      │ 5-e55    │   698 │ M3 - 18-55edo │   393 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e55   │   807 │ m3 - 14-55edo │   305 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G4   │ I      │ m7-e55   │  1004 │ m3 - 14-55edo │   305 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ A4   │ I      │ 0-1      │  1200 │ M3 - 18-55edo │   393 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ B4   │ I      │ M2-e55-1 │  1396 │ M3 - 18-55edo │   393 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C5   │ I      │ m3-e55-1 │  1505 │ m3 - 14-55edo │   305 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ D5   │ I      │ 4-e55-1  │  1702 │ m3 - 14-55edo │   305 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ E5   │ I      │ 5-e55-1  │  1898 │ M3 - 18-55edo │   393 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e55-1 │  2007 │ m3 - 14-55edo │   305 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G5   │ I      │ m7-e55-1 │  2204 │ m3 - 14-55edo │   305 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ A5   │ I      │ 0-2      │  2400 │ M3 - 18-55edo │   393 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ B5   │ I      │ M2-e55-2 │  2596 │ M3 - 18-55edo │   393 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C6   │ I      │ m3-e55-2 │  2705 │ m3 - 14-55edo │   305 │
    │ B5  │ II     │ M6-e55-2 │  3295 │ D6   │ I      │ 4-e55-2  │  2902 │ m3 - 14-55edo │   305 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ E6   │ I      │ 5-e55-2  │  3098 │ M3 - 18-55edo │   393 │
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
    │ E#2 │ IV     │ A3-e55   │   480 │ G#2  │ III    │ A1-e55   │    87 │ m3 - 14-55edo │   305 │
    │ F#2 │ IV     │ A4-e55   │   589 │ A#2  │ III    │ A2-e55   │   284 │ M3 - 18-55edo │   393 │
    │ G#2 │ IV     │ A5-e55   │   785 │ B#2  │ III    │ A3-e55   │   480 │ M3 - 18-55edo │   393 │
    │ B#2 │ III    │ A3-e55   │   480 │ D#3  │ II     │ A1-e55   │    87 │ m3 - 14-55edo │   305 │
    │ C#3 │ III    │ A4-e55   │   589 │ E#3  │ II     │ A2-e55   │   284 │ M3 - 18-55edo │   393 │
    │ D#3 │ III    │ A5-e55   │   785 │ F#3  │ II     │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ F#3 │ II     │ M3-e55   │   393 │ A#3  │ I      │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ G#3 │ II     │ A4-e55   │   589 │ B#3  │ I      │ A2-e55   │   284 │ M3 - 18-55edo │   393 │
    │ A#3 │ II     │ A5-e55   │   785 │ C#4  │ I      │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ C#4 │ II     │ M7-e55   │  1091 │ E#4  │ I      │ A5-e55   │   785 │ M3 - 18-55edo │   393 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ F#4  │ I      │ M6-e55   │   895 │ m3 - 14-55edo │   305 │
    │ E#4 │ II     │ A2-e55-1 │  1484 │ G#4  │ I      │ M7-e55   │  1091 │ m3 - 14-55edo │   305 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ A#4  │ I      │ A1-e55-1 │  1287 │ M3 - 18-55edo │   393 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ B#4  │ I      │ A2-e55-1 │  1484 │ M3 - 18-55edo │   393 │
    │ A#4 │ II     │ A5-e55-1 │  1985 │ C#5  │ I      │ M3-e55-1 │  1593 │ m3 - 14-55edo │   305 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ E#5  │ I      │ A5-e55-1 │  1985 │ M3 - 18-55edo │   393 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ F#5  │ I      │ M6-e55-1 │  2095 │ m3 - 14-55edo │   305 │
    │ E#5 │ II     │ A2-e55-2 │  2684 │ G#5  │ I      │ M7-e55-1 │  2291 │ m3 - 14-55edo │   305 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ A#5  │ I      │ A1-e55-2 │  2487 │ M3 - 18-55edo │   393 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ B#5  │ I      │ A2-e55-2 │  2684 │ M3 - 18-55edo │   393 │
    │ A#5 │ II     │ A5-e55-2 │  3185 │ C#6  │ I      │ M3-e55-2 │  2793 │ m3 - 14-55edo │   305 │
    │ C#6 │ II     │ M7-e55-2 │  3491 │ E#6  │ I      │ A5-e55-2 │  3185 │ M3 - 18-55edo │   393 │
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
    │ F2  │ IV     │ 4-e55    │   502 │ Ab2  │ III    │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ Gb2 │ IV     │ d5-e55   │   611 │ Bb2  │ III    │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Ab2 │ IV     │ m6-e55   │   807 │ C3   │ III    │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ Bb2 │ IV     │ m7-e55   │  1004 │ Db3  │ III    │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ C3  │ III    │ 4-e55    │   502 │ Eb3  │ II     │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ Db3 │ III    │ d5-e55   │   611 │ F3   │ II     │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Eb3 │ III    │ m6-e55   │   807 │ Gb3  │ II     │ d4-e55   │   415 │ m3 - 14-55edo │   305 │
    │ F3  │ III    │ m7-e55   │  1004 │ Ab3  │ II     │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ Gb3 │ II     │ d4-e55   │   415 │ Bb3  │ I      │ m2-e55   │   109 │ M3 - 18-55edo │   393 │
    │ Ab3 │ II     │ d5-e55   │   611 │ C4   │ I      │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Bb3 │ II     │ m6-e55   │   807 │ Db4  │ I      │ d4-e55   │   415 │ m3 - 14-55edo │   305 │
    │ C4  │ II     │ m7-e55   │  1004 │ Eb4  │ I      │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ Db4 │ II     │ d8-e55   │  1113 │ F4   │ I      │ m6-e55   │   807 │ M3 - 18-55edo │   393 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ Gb4  │ I      │ d7-e55   │   916 │ m3 - 14-55edo │   305 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Ab4  │ I      │ d8-e55   │  1113 │ m3 - 14-55edo │   305 │
    │ Gb4 │ II     │ d4-e55-1 │  1615 │ Bb4  │ I      │ m2-e55-1 │  1309 │ M3 - 18-55edo │   393 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ C5   │ I      │ m3-e55-1 │  1505 │ M3 - 18-55edo │   393 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ Db5  │ I      │ d4-e55-1 │  1615 │ m3 - 14-55edo │   305 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m3 - 14-55edo │   305 │
    │ Db5 │ II     │ d8-e55-1 │  2313 │ F5   │ I      │ m6-e55-1 │  2007 │ M3 - 18-55edo │   393 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ Gb5  │ I      │ d7-e55-1 │  2116 │ m3 - 14-55edo │   305 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m3 - 14-55edo │   305 │
    │ Gb5 │ II     │ d4-e55-2 │  2815 │ Bb5  │ I      │ m2-e55-2 │  2509 │ M3 - 18-55edo │   393 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ C6   │ I      │ m3-e55-2 │  2705 │ M3 - 18-55edo │   393 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ Db6  │ I      │ d4-e55-2 │  2815 │ m3 - 14-55edo │   305 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m3 - 14-55edo │   305 │
    │ Db6 │ II     │ d8-e55-2 │  3513 │ F6   │ I      │ m6-e55-2 │  3207 │ M3 - 18-55edo │   393 │
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
    │ E2  │ IV     │ M3-e55   │   393 │ G2   │ III    │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ F#2 │ IV     │ A4-e55   │   589 │ A2   │ III    │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ G2  │ IV     │ 5-e55    │   698 │ B2   │ III    │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ A2  │ IV     │ M6-e55   │   895 │ C#3  │ III    │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ B2  │ III    │ M3-e55   │   393 │ D3   │ II     │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ C#3 │ III    │ A4-e55   │   589 │ E3   │ II     │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ D3  │ III    │ 5-e55    │   698 │ F#3  │ II     │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ E3  │ III    │ M6-e55   │   895 │ G3   │ II     │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ F#3 │ II     │ M3-e55   │   393 │ A3   │ I      │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ G3  │ II     │ 4-e55    │   502 │ B3   │ I      │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ A3  │ II     │ 5-e55    │   698 │ C#4  │ I      │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ B3  │ II     │ M6-e55   │   895 │ D4   │ I      │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ C#4 │ II     │ M7-e55   │  1091 │ E4   │ I      │ 5-e55    │   698 │ m3 - 14-55edo │   305 │
    │ D4  │ II     │ 0-1      │  1200 │ F#4  │ I      │ M6-e55   │   895 │ M3 - 18-55edo │   393 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G4   │ I      │ m7-e55   │  1004 │ m3 - 14-55edo │   305 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ A4   │ I      │ 0-1      │  1200 │ m3 - 14-55edo │   305 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ B4   │ I      │ M2-e55-1 │  1396 │ M3 - 18-55edo │   393 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C#5  │ I      │ M3-e55-1 │  1593 │ M3 - 18-55edo │   393 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ D5   │ I      │ 4-e55-1  │  1702 │ m3 - 14-55edo │   305 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ E5   │ I      │ 5-e55-1  │  1898 │ m3 - 14-55edo │   305 │
    │ D5  │ II     │ 0-2      │  2400 │ F#5  │ I      │ M6-e55-1 │  2095 │ M3 - 18-55edo │   393 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G5   │ I      │ m7-e55-1 │  2204 │ m3 - 14-55edo │   305 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ A5   │ I      │ 0-2      │  2400 │ m3 - 14-55edo │   305 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ B5   │ I      │ M2-e55-2 │  2596 │ M3 - 18-55edo │   393 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C#6  │ I      │ M3-e55-2 │  2793 │ M3 - 18-55edo │   393 │
    │ B5  │ II     │ M6-e55-2 │  3295 │ D6   │ I      │ 4-e55-2  │  2902 │ m3 - 14-55edo │   305 │
    │ C#6 │ II     │ M7-e55-2 │  3491 │ E6   │ I      │ 5-e55-2  │  3098 │ m3 - 14-55edo │   305 │
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
    │ Eb2 │ IV     │ m3-e55   │   305 │ G2   │ III    │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ F2  │ IV     │ 4-e55    │   502 │ Ab2  │ III    │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ G2  │ IV     │ 5-e55    │   698 │ Bb2  │ III    │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ Ab2 │ IV     │ m6-e55   │   807 │ C3   │ III    │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ Bb2 │ III    │ m3-e55   │   305 │ D3   │ II     │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ C3  │ III    │ 4-e55    │   502 │ Eb3  │ II     │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ D3  │ III    │ 5-e55    │   698 │ F3   │ II     │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ Eb3 │ III    │ m6-e55   │   807 │ G3   │ II     │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ F3  │ III    │ m7-e55   │  1004 │ Ab3  │ II     │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ G3  │ II     │ 4-e55    │   502 │ Bb3  │ I      │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ Ab3 │ II     │ d5-e55   │   611 │ C4   │ I      │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Bb3 │ II     │ m6-e55   │   807 │ D4   │ I      │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ C4  │ II     │ m7-e55   │  1004 │ Eb4  │ I      │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e55   │   807 │ m3 - 14-55edo │   305 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ G4   │ I      │ m7-e55   │  1004 │ M3 - 18-55edo │   393 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Ab4  │ I      │ d8-e55   │  1113 │ m3 - 14-55edo │   305 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m3 - 14-55edo │   305 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ C5   │ I      │ m3-e55-1 │  1505 │ M3 - 18-55edo │   393 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ D5   │ I      │ 4-e55-1  │  1702 │ M3 - 18-55edo │   393 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m3 - 14-55edo │   305 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e55-1 │  2007 │ m3 - 14-55edo │   305 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ G5   │ I      │ m7-e55-1 │  2204 │ M3 - 18-55edo │   393 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m3 - 14-55edo │   305 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m3 - 14-55edo │   305 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ C6   │ I      │ m3-e55-2 │  2705 │ M3 - 18-55edo │   393 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ D6   │ I      │ 4-e55-2  │  2902 │ M3 - 18-55edo │   393 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m3 - 14-55edo │   305 │
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
    │ E2  │ IV     │ M3-e55   │   393 │ G#2  │ III    │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ F#2 │ IV     │ A4-e55   │   589 │ A2   │ III    │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ G#2 │ IV     │ A5-e55   │   785 │ B2   │ III    │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ A2  │ IV     │ M6-e55   │   895 │ C#3  │ III    │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ B2  │ III    │ M3-e55   │   393 │ D#3  │ II     │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ C#3 │ III    │ A4-e55   │   589 │ E3   │ II     │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ D#3 │ III    │ A5-e55   │   785 │ F#3  │ II     │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ E3  │ III    │ M6-e55   │   895 │ G#3  │ II     │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ F#3 │ II     │ M3-e55   │   393 │ A3   │ I      │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ G#3 │ II     │ A4-e55   │   589 │ B3   │ I      │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ A3  │ II     │ 5-e55    │   698 │ C#4  │ I      │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ B3  │ II     │ M6-e55   │   895 │ D#4  │ I      │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ C#4 │ II     │ M7-e55   │  1091 │ E4   │ I      │ 5-e55    │   698 │ m3 - 14-55edo │   305 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ F#4  │ I      │ M6-e55   │   895 │ m3 - 14-55edo │   305 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G#4  │ I      │ M7-e55   │  1091 │ M3 - 18-55edo │   393 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ A4   │ I      │ 0-1      │  1200 │ m3 - 14-55edo │   305 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ B4   │ I      │ M2-e55-1 │  1396 │ m3 - 14-55edo │   305 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C#5  │ I      │ M3-e55-1 │  1593 │ M3 - 18-55edo │   393 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ D#5  │ I      │ A4-e55-1 │  1789 │ M3 - 18-55edo │   393 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ E5   │ I      │ 5-e55-1  │  1898 │ m3 - 14-55edo │   305 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ F#5  │ I      │ M6-e55-1 │  2095 │ m3 - 14-55edo │   305 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G#5  │ I      │ M7-e55-1 │  2291 │ M3 - 18-55edo │   393 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ A5   │ I      │ 0-2      │  2400 │ m3 - 14-55edo │   305 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ B5   │ I      │ M2-e55-2 │  2596 │ m3 - 14-55edo │   305 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C#6  │ I      │ M3-e55-2 │  2793 │ M3 - 18-55edo │   393 │
    │ B5  │ II     │ M6-e55-2 │  3295 │ D#6  │ I      │ A4-e55-2 │  2989 │ M3 - 18-55edo │   393 │
    │ C#6 │ II     │ M7-e55-2 │  3491 │ E6   │ I      │ 5-e55-2  │  3098 │ m3 - 14-55edo │   305 │
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
    │ F2  │ IV     │ 4-e55    │   502 │ A2   │ III    │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ G2  │ IV     │ 5-e55    │   698 │ Bb2  │ III    │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ A2  │ IV     │ M6-e55   │   895 │ C3   │ III    │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ Bb2 │ III    │ m3-e55   │   305 │ D3   │ II     │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ C3  │ III    │ 4-e55    │   502 │ E3   │ II     │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ D3  │ III    │ 5-e55    │   698 │ F3   │ II     │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ E3  │ III    │ M6-e55   │   895 │ G3   │ II     │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ F3  │ II     │ m3-e55   │   305 │ A3   │ I      │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ G3  │ II     │ 4-e55    │   502 │ Bb3  │ I      │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ A3  │ II     │ 5-e55    │   698 │ C4   │ I      │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ Bb3 │ II     │ m6-e55   │   807 │ D4   │ I      │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ C4  │ II     │ m7-e55   │  1004 │ E4   │ I      │ 5-e55    │   698 │ M3 - 18-55edo │   393 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e55   │   807 │ m3 - 14-55edo │   305 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G4   │ I      │ m7-e55   │  1004 │ m3 - 14-55edo │   305 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ A4   │ I      │ 0-1      │  1200 │ M3 - 18-55edo │   393 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m3 - 14-55edo │   305 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C5   │ I      │ m3-e55-1 │  1505 │ m3 - 14-55edo │   305 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ D5   │ I      │ 4-e55-1  │  1702 │ M3 - 18-55edo │   393 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ E5   │ I      │ 5-e55-1  │  1898 │ M3 - 18-55edo │   393 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e55-1 │  2007 │ m3 - 14-55edo │   305 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G5   │ I      │ m7-e55-1 │  2204 │ m3 - 14-55edo │   305 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ A5   │ I      │ 0-2      │  2400 │ M3 - 18-55edo │   393 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m3 - 14-55edo │   305 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C6   │ I      │ m3-e55-2 │  2705 │ m3 - 14-55edo │   305 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ D6   │ I      │ 4-e55-2  │  2902 │ M3 - 18-55edo │   393 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ E6   │ I      │ 5-e55-2  │  3098 │ M3 - 18-55edo │   393 │
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
    │ F#2 │ IV     │ A4-e55   │   589 │ A#2  │ III    │ A2-e55   │   284 │ M3 - 18-55edo │   393 │
    │ G#2 │ IV     │ A5-e55   │   785 │ B2   │ III    │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ B2  │ III    │ M3-e55   │   393 │ D#3  │ II     │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ C#3 │ III    │ A4-e55   │   589 │ E#3  │ II     │ A2-e55   │   284 │ M3 - 18-55edo │   393 │
    │ D#3 │ III    │ A5-e55   │   785 │ F#3  │ II     │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ F#3 │ II     │ M3-e55   │   393 │ A#3  │ I      │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ G#3 │ II     │ A4-e55   │   589 │ B3   │ I      │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ A#3 │ II     │ A5-e55   │   785 │ C#4  │ I      │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ B3  │ II     │ M6-e55   │   895 │ D#4  │ I      │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ C#4 │ II     │ M7-e55   │  1091 │ E#4  │ I      │ A5-e55   │   785 │ M3 - 18-55edo │   393 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ F#4  │ I      │ M6-e55   │   895 │ m3 - 14-55edo │   305 │
    │ E#4 │ II     │ A2-e55-1 │  1484 │ G#4  │ I      │ M7-e55   │  1091 │ m3 - 14-55edo │   305 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ A#4  │ I      │ A1-e55-1 │  1287 │ M3 - 18-55edo │   393 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ B4   │ I      │ M2-e55-1 │  1396 │ m3 - 14-55edo │   305 │
    │ A#4 │ II     │ A5-e55-1 │  1985 │ C#5  │ I      │ M3-e55-1 │  1593 │ m3 - 14-55edo │   305 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ D#5  │ I      │ A4-e55-1 │  1789 │ M3 - 18-55edo │   393 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ E#5  │ I      │ A5-e55-1 │  1985 │ M3 - 18-55edo │   393 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ F#5  │ I      │ M6-e55-1 │  2095 │ m3 - 14-55edo │   305 │
    │ E#5 │ II     │ A2-e55-2 │  2684 │ G#5  │ I      │ M7-e55-1 │  2291 │ m3 - 14-55edo │   305 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ A#5  │ I      │ A1-e55-2 │  2487 │ M3 - 18-55edo │   393 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ B5   │ I      │ M2-e55-2 │  2596 │ m3 - 14-55edo │   305 │
    │ A#5 │ II     │ A5-e55-2 │  3185 │ C#6  │ I      │ M3-e55-2 │  2793 │ m3 - 14-55edo │   305 │
    │ B5  │ II     │ M6-e55-2 │  3295 │ D#6  │ I      │ A4-e55-2 │  2989 │ M3 - 18-55edo │   393 │
    │ C#6 │ II     │ M7-e55-2 │  3491 │ E#6  │ I      │ A5-e55-2 │  3185 │ M3 - 18-55edo │   393 │
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
    │ Gb2 │ IV     │ d5-e55   │   611 │ Bb2  │ III    │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Ab2 │ IV     │ m6-e55   │   807 │ Cb3  │ III    │ d4-e55   │   415 │ m3 - 14-55edo │   305 │
    │ Bb2 │ IV     │ m7-e55   │  1004 │ Db3  │ III    │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ Cb3 │ III    │ d4-e55   │   415 │ Eb3  │ II     │ m2-e55   │   109 │ M3 - 18-55edo │   393 │
    │ Db3 │ III    │ d5-e55   │   611 │ F3   │ II     │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Eb3 │ III    │ m6-e55   │   807 │ Gb3  │ II     │ d4-e55   │   415 │ m3 - 14-55edo │   305 │
    │ F3  │ III    │ m7-e55   │  1004 │ Ab3  │ II     │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ Gb3 │ II     │ d4-e55   │   415 │ Bb3  │ I      │ m2-e55   │   109 │ M3 - 18-55edo │   393 │
    │ Ab3 │ II     │ d5-e55   │   611 │ Cb4  │ I      │ d3-e55   │   218 │ m3 - 14-55edo │   305 │
    │ Bb3 │ II     │ m6-e55   │   807 │ Db4  │ I      │ d4-e55   │   415 │ m3 - 14-55edo │   305 │
    │ Cb4 │ II     │ d7-e55   │   916 │ Eb4  │ I      │ d5-e55   │   611 │ M3 - 18-55edo │   393 │
    │ Db4 │ II     │ d8-e55   │  1113 │ F4   │ I      │ m6-e55   │   807 │ M3 - 18-55edo │   393 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ Gb4  │ I      │ d7-e55   │   916 │ m3 - 14-55edo │   305 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Ab4  │ I      │ d8-e55   │  1113 │ m3 - 14-55edo │   305 │
    │ Gb4 │ II     │ d4-e55-1 │  1615 │ Bb4  │ I      │ m2-e55-1 │  1309 │ M3 - 18-55edo │   393 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ Cb5  │ I      │ d3-e55-1 │  1418 │ m3 - 14-55edo │   305 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ Db5  │ I      │ d4-e55-1 │  1615 │ m3 - 14-55edo │   305 │
    │ Cb5 │ II     │ d7-e55-1 │  2116 │ Eb5  │ I      │ d5-e55-1 │  1811 │ M3 - 18-55edo │   393 │
    │ Db5 │ II     │ d8-e55-1 │  2313 │ F5   │ I      │ m6-e55-1 │  2007 │ M3 - 18-55edo │   393 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ Gb5  │ I      │ d7-e55-1 │  2116 │ m3 - 14-55edo │   305 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m3 - 14-55edo │   305 │
    │ Gb5 │ II     │ d4-e55-2 │  2815 │ Bb5  │ I      │ m2-e55-2 │  2509 │ M3 - 18-55edo │   393 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ Cb6  │ I      │ d3-e55-2 │  2618 │ m3 - 14-55edo │   305 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ Db6  │ I      │ d4-e55-2 │  2815 │ m3 - 14-55edo │   305 │
    │ Cb6 │ II     │ d7-e55-2 │  3316 │ Eb6  │ I      │ d5-e55-2 │  3011 │ M3 - 18-55edo │   393 │
    │ Db6 │ II     │ d8-e55-2 │  3513 │ F6   │ I      │ m6-e55-2 │  3207 │ M3 - 18-55edo │   393 │
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
    │ F2  │ IV     │ 4-e55    │   502 │ A2   │ III    │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ G2  │ IV     │ 5-e55    │   698 │ Bb2  │ III    │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ A2  │ IV     │ M6-e55   │   895 │ C3   │ III    │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ Bb2 │ III    │ m3-e55   │   305 │ D3   │ II     │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ C3  │ III    │ 4-e55    │   502 │ E3   │ II     │ M2-e55   │   196 │ M3 - 18-55edo │   393 │
    │ D3  │ III    │ 5-e55    │   698 │ F3   │ II     │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ E3  │ III    │ M6-e55   │   895 │ G3   │ II     │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ F3  │ II     │ m3-e55   │   305 │ A3   │ I      │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ G3  │ II     │ 4-e55    │   502 │ Bb3  │ I      │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ A3  │ II     │ 5-e55    │   698 │ C4   │ I      │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ Bb3 │ II     │ m6-e55   │   807 │ D4   │ I      │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ C4  │ II     │ m7-e55   │  1004 │ E4   │ I      │ 5-e55    │   698 │ M3 - 18-55edo │   393 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e55   │   807 │ m3 - 14-55edo │   305 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G4   │ I      │ m7-e55   │  1004 │ m3 - 14-55edo │   305 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ A4   │ I      │ 0-1      │  1200 │ M3 - 18-55edo │   393 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m3 - 14-55edo │   305 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C5   │ I      │ m3-e55-1 │  1505 │ m3 - 14-55edo │   305 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ D5   │ I      │ 4-e55-1  │  1702 │ M3 - 18-55edo │   393 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ E5   │ I      │ 5-e55-1  │  1898 │ M3 - 18-55edo │   393 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e55-1 │  2007 │ m3 - 14-55edo │   305 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G5   │ I      │ m7-e55-1 │  2204 │ m3 - 14-55edo │   305 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ A5   │ I      │ 0-2      │  2400 │ M3 - 18-55edo │   393 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m3 - 14-55edo │   305 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C6   │ I      │ m3-e55-2 │  2705 │ m3 - 14-55edo │   305 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ D6   │ I      │ 4-e55-2  │  2902 │ M3 - 18-55edo │   393 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ E6   │ I      │ 5-e55-2  │  3098 │ M3 - 18-55edo │   393 │
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
    │ Ab2 │ IV     │ m6-e55   │   807 │ C3   │ III    │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ Bb2 │ IV     │ m7-e55   │  1004 │ Db3  │ III    │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ C3  │ III    │ 4-e55    │   502 │ Eb3  │ II     │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ Db3 │ III    │ d5-e55   │   611 │ F3   │ II     │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Eb3 │ III    │ m6-e55   │   807 │ G3   │ II     │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ F3  │ III    │ m7-e55   │  1004 │ Ab3  │ II     │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ G3  │ II     │ 4-e55    │   502 │ Bb3  │ I      │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ Ab3 │ II     │ d5-e55   │   611 │ C4   │ I      │ m3-e55   │   305 │ M3 - 18-55edo │   393 │
    │ Bb3 │ II     │ m6-e55   │   807 │ Db4  │ I      │ d4-e55   │   415 │ m3 - 14-55edo │   305 │
    │ C4  │ II     │ m7-e55   │  1004 │ Eb4  │ I      │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ Db4 │ II     │ d8-e55   │  1113 │ F4   │ I      │ m6-e55   │   807 │ M3 - 18-55edo │   393 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ G4   │ I      │ m7-e55   │  1004 │ M3 - 18-55edo │   393 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ Ab4  │ I      │ d8-e55   │  1113 │ m3 - 14-55edo │   305 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m3 - 14-55edo │   305 │
    │ Ab4 │ II     │ d5-e55-1 │  1811 │ C5   │ I      │ m3-e55-1 │  1505 │ M3 - 18-55edo │   393 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ Db5  │ I      │ d4-e55-1 │  1615 │ m3 - 14-55edo │   305 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m3 - 14-55edo │   305 │
    │ Db5 │ II     │ d8-e55-1 │  2313 │ F5   │ I      │ m6-e55-1 │  2007 │ M3 - 18-55edo │   393 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ G5   │ I      │ m7-e55-1 │  2204 │ M3 - 18-55edo │   393 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ Ab5  │ I      │ d8-e55-1 │  2313 │ m3 - 14-55edo │   305 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m3 - 14-55edo │   305 │
    │ Ab5 │ II     │ d5-e55-2 │  3011 │ C6   │ I      │ m3-e55-2 │  2705 │ M3 - 18-55edo │   393 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ Db6  │ I      │ d4-e55-2 │  2815 │ m3 - 14-55edo │   305 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m3 - 14-55edo │   305 │
    │ Db6 │ II     │ d8-e55-2 │  3513 │ F6   │ I      │ m6-e55-2 │  3207 │ M3 - 18-55edo │   393 │
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
    │ A2  │ IV     │ M6-e55   │   895 │ C#3  │ III    │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ B2  │ III    │ M3-e55   │   393 │ D3   │ II     │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ C#3 │ III    │ A4-e55   │   589 │ E3   │ II     │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ D3  │ III    │ 5-e55    │   698 │ F#3  │ II     │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ E3  │ III    │ M6-e55   │   895 │ G#3  │ II     │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ F#3 │ II     │ M3-e55   │   393 │ A3   │ I      │ 0        │     0 │ m3 - 14-55edo │   305 │
    │ G#3 │ II     │ A4-e55   │   589 │ B3   │ I      │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ A3  │ II     │ 5-e55    │   698 │ C#4  │ I      │ M3-e55   │   393 │ M3 - 18-55edo │   393 │
    │ B3  │ II     │ M6-e55   │   895 │ D4   │ I      │ 4-e55    │   502 │ m3 - 14-55edo │   305 │
    │ C#4 │ II     │ M7-e55   │  1091 │ E4   │ I      │ 5-e55    │   698 │ m3 - 14-55edo │   305 │
    │ D4  │ II     │ 0-1      │  1200 │ F#4  │ I      │ M6-e55   │   895 │ M3 - 18-55edo │   393 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G#4  │ I      │ M7-e55   │  1091 │ M3 - 18-55edo │   393 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ A4   │ I      │ 0-1      │  1200 │ m3 - 14-55edo │   305 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ B4   │ I      │ M2-e55-1 │  1396 │ m3 - 14-55edo │   305 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C#5  │ I      │ M3-e55-1 │  1593 │ M3 - 18-55edo │   393 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ D5   │ I      │ 4-e55-1  │  1702 │ m3 - 14-55edo │   305 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ E5   │ I      │ 5-e55-1  │  1898 │ m3 - 14-55edo │   305 │
    │ D5  │ II     │ 0-2      │  2400 │ F#5  │ I      │ M6-e55-1 │  2095 │ M3 - 18-55edo │   393 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G#5  │ I      │ M7-e55-1 │  2291 │ M3 - 18-55edo │   393 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ A5   │ I      │ 0-2      │  2400 │ m3 - 14-55edo │   305 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ B5   │ I      │ M2-e55-2 │  2596 │ m3 - 14-55edo │   305 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C#6  │ I      │ M3-e55-2 │  2793 │ M3 - 18-55edo │   393 │
    │ B5  │ II     │ M6-e55-2 │  3295 │ D6   │ I      │ 4-e55-2  │  2902 │ m3 - 14-55edo │   305 │
    │ C#6 │ II     │ M7-e55-2 │  3491 │ E6   │ I      │ 5-e55-2  │  3098 │ m3 - 14-55edo │   305 │
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
    │ Bb2 │ III    │ m3-e55   │   305 │ D3   │ II     │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ C3  │ III    │ 4-e55    │   502 │ Eb3  │ II     │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ D3  │ III    │ 5-e55    │   698 │ F3   │ II     │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ Eb3 │ III    │ m6-e55   │   807 │ G3   │ II     │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ F3  │ II     │ m3-e55   │   305 │ A3   │ I      │ 0        │     0 │ M3 - 18-55edo │   393 │
    │ G3  │ II     │ 4-e55    │   502 │ Bb3  │ I      │ m2-e55   │   109 │ m3 - 14-55edo │   305 │
    │ A3  │ II     │ 5-e55    │   698 │ C4   │ I      │ m3-e55   │   305 │ m3 - 14-55edo │   305 │
    │ Bb3 │ II     │ m6-e55   │   807 │ D4   │ I      │ 4-e55    │   502 │ M3 - 18-55edo │   393 │
    │ C4  │ II     │ m7-e55   │  1004 │ Eb4  │ I      │ d5-e55   │   611 │ m3 - 14-55edo │   305 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e55   │   807 │ m3 - 14-55edo │   305 │
    │ Eb4 │ II     │ m2-e55-1 │  1309 │ G4   │ I      │ m7-e55   │  1004 │ M3 - 18-55edo │   393 │
    │ F4  │ II     │ m3-e55-1 │  1505 │ A4   │ I      │ 0-1      │  1200 │ M3 - 18-55edo │   393 │
    │ G4  │ II     │ 4-e55-1  │  1702 │ Bb4  │ I      │ m2-e55-1 │  1309 │ m3 - 14-55edo │   305 │
    │ A4  │ II     │ 5-e55-1  │  1898 │ C5   │ I      │ m3-e55-1 │  1505 │ m3 - 14-55edo │   305 │
    │ Bb4 │ II     │ m6-e55-1 │  2007 │ D5   │ I      │ 4-e55-1  │  1702 │ M3 - 18-55edo │   393 │
    │ C5  │ II     │ m7-e55-1 │  2204 │ Eb5  │ I      │ d5-e55-1 │  1811 │ m3 - 14-55edo │   305 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e55-1 │  2007 │ m3 - 14-55edo │   305 │
    │ Eb5 │ II     │ m2-e55-2 │  2509 │ G5   │ I      │ m7-e55-1 │  2204 │ M3 - 18-55edo │   393 │
    │ F5  │ II     │ m3-e55-2 │  2705 │ A5   │ I      │ 0-2      │  2400 │ M3 - 18-55edo │   393 │
    │ G5  │ II     │ 4-e55-2  │  2902 │ Bb5  │ I      │ m2-e55-2 │  2509 │ m3 - 14-55edo │   305 │
    │ A5  │ II     │ 5-e55-2  │  3098 │ C6   │ I      │ m3-e55-2 │  2705 │ m3 - 14-55edo │   305 │
    │ Bb5 │ II     │ m6-e55-2 │  3207 │ D6   │ I      │ 4-e55-2  │  2902 │ M3 - 18-55edo │   393 │
    │ C6  │ II     │ m7-e55-2 │  3404 │ Eb6  │ I      │ d5-e55-2 │  3011 │ m3 - 14-55edo │   305 │
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
    │ B2  │ III    │ M3-e55   │   393 │ D#3  │ II     │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ C#3 │ III    │ A4-e55   │   589 │ E3   │ II     │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ D#3 │ III    │ A5-e55   │   785 │ F#3  │ II     │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ E3  │ III    │ M6-e55   │   895 │ G#3  │ II     │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ F#3 │ II     │ M3-e55   │   393 │ A#3  │ I      │ A1-e55   │    87 │ M3 - 18-55edo │   393 │
    │ G#3 │ II     │ A4-e55   │   589 │ B3   │ I      │ M2-e55   │   196 │ m3 - 14-55edo │   305 │
    │ A#3 │ II     │ A5-e55   │   785 │ C#4  │ I      │ M3-e55   │   393 │ m3 - 14-55edo │   305 │
    │ B3  │ II     │ M6-e55   │   895 │ D#4  │ I      │ A4-e55   │   589 │ M3 - 18-55edo │   393 │
    │ C#4 │ II     │ M7-e55   │  1091 │ E4   │ I      │ 5-e55    │   698 │ m3 - 14-55edo │   305 │
    │ D#4 │ II     │ A1-e55-1 │  1287 │ F#4  │ I      │ M6-e55   │   895 │ m3 - 14-55edo │   305 │
    │ E4  │ II     │ M2-e55-1 │  1396 │ G#4  │ I      │ M7-e55   │  1091 │ M3 - 18-55edo │   393 │
    │ F#4 │ II     │ M3-e55-1 │  1593 │ A#4  │ I      │ A1-e55-1 │  1287 │ M3 - 18-55edo │   393 │
    │ G#4 │ II     │ A4-e55-1 │  1789 │ B4   │ I      │ M2-e55-1 │  1396 │ m3 - 14-55edo │   305 │
    │ A#4 │ II     │ A5-e55-1 │  1985 │ C#5  │ I      │ M3-e55-1 │  1593 │ m3 - 14-55edo │   305 │
    │ B4  │ II     │ M6-e55-1 │  2095 │ D#5  │ I      │ A4-e55-1 │  1789 │ M3 - 18-55edo │   393 │
    │ C#5 │ II     │ M7-e55-1 │  2291 │ E5   │ I      │ 5-e55-1  │  1898 │ m3 - 14-55edo │   305 │
    │ D#5 │ II     │ A1-e55-2 │  2487 │ F#5  │ I      │ M6-e55-1 │  2095 │ m3 - 14-55edo │   305 │
    │ E5  │ II     │ M2-e55-2 │  2596 │ G#5  │ I      │ M7-e55-1 │  2291 │ M3 - 18-55edo │   393 │
    │ F#5 │ II     │ M3-e55-2 │  2793 │ A#5  │ I      │ A1-e55-2 │  2487 │ M3 - 18-55edo │   393 │
    │ G#5 │ II     │ A4-e55-2 │  2989 │ B5   │ I      │ M2-e55-2 │  2596 │ m3 - 14-55edo │   305 │
    │ A#5 │ II     │ A5-e55-2 │  3185 │ C#6  │ I      │ M3-e55-2 │  2793 │ m3 - 14-55edo │   305 │
    │ B5  │ II     │ M6-e55-2 │  3295 │ D#6  │ I      │ A4-e55-2 │  2989 │ M3 - 18-55edo │   393 │
    │ C#6 │ II     │ M7-e55-2 │  3491 │ E6   │ I      │ 5-e55-2  │  3098 │ m3 - 14-55edo │   305 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;
