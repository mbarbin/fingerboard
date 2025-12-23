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
  let t = Lazy.force E19.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_e19 ~from
;;

let%expect_test "c_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ C2  │ IV     │ 0        │     0 │ A2   │ III    │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ D2  │ IV     │ M2-e19   │   189 │ B2   │ III    │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ E2  │ IV     │ M3-e19   │   379 │ C3   │ III    │ 4-e19    │   505 │ m6 - 13-19edo │   821 │
    │ F2  │ IV     │ 4-e19    │   505 │ D3   │ III    │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ A2  │ III    │ M2-e19   │   189 │ F3   │ II     │ m3-e19   │   316 │ m6 - 13-19edo │   821 │
    │ B2  │ III    │ M3-e19   │   379 │ G3   │ II     │ 4-e19    │   505 │ m6 - 13-19edo │   821 │
    │ C3  │ III    │ 4-e19    │   505 │ A3   │ II     │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ E3  │ II     │ M2-e19   │   189 │ C4   │ I      │ m3-e19   │   316 │ m6 - 13-19edo │   821 │
    │ F3  │ II     │ m3-e19   │   316 │ D4   │ I      │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ G3  │ II     │ 4-e19    │   505 │ E4   │ I      │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ A3  │ II     │ 5-e19    │   695 │ F4   │ I      │ m6-e19   │   821 │ m6 - 13-19edo │   821 │
    │ B3  │ II     │ M6-e19   │   884 │ G4   │ I      │ m7-e19   │  1011 │ m6 - 13-19edo │   821 │
    │ C4  │ II     │ m7-e19   │  1011 │ A4   │ I      │ 0-1      │  1200 │ M6 - 14-19edo │   884 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e19-1 │  1389 │ M6 - 14-19edo │   884 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C5   │ I      │ m3-e19-1 │  1516 │ m6 - 13-19edo │   821 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ D5   │ I      │ 4-e19-1  │  1705 │ M6 - 14-19edo │   884 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ E5   │ I      │ 5-e19-1  │  1895 │ M6 - 14-19edo │   884 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F5   │ I      │ m6-e19-1 │  2021 │ m6 - 13-19edo │   821 │
    │ B4  │ II     │ M6-e19-1 │  2084 │ G5   │ I      │ m7-e19-1 │  2211 │ m6 - 13-19edo │   821 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ A5   │ I      │ 0-2      │  2400 │ M6 - 14-19edo │   884 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e19-2 │  2589 │ M6 - 14-19edo │   884 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C6   │ I      │ m3-e19-2 │  2716 │ m6 - 13-19edo │   821 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ D6   │ I      │ 4-e19-2  │  2905 │ M6 - 14-19edo │   884 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ E6   │ I      │ 5-e19-2  │  3095 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_c_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ C#2 │ IV     │ A1-e19   │    63 │ A#2  │ III    │ A2-e19   │   253 │ M6 - 14-19edo │   884 │
    │ D#2 │ IV     │ A2-e19   │   253 │ B#2  │ III    │ A3-e19   │   442 │ M6 - 14-19edo │   884 │
    │ E#2 │ IV     │ A3-e19   │   442 │ C#3  │ III    │ A4-e19   │   568 │ m6 - 13-19edo │   821 │
    │ F#2 │ IV     │ A4-e19   │   568 │ D#3  │ III    │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ G#2 │ III    │ A1-e19   │    63 │ E#3  │ II     │ A2-e19   │   253 │ M6 - 14-19edo │   884 │
    │ A#2 │ III    │ A2-e19   │   253 │ F#3  │ II     │ M3-e19   │   379 │ m6 - 13-19edo │   821 │
    │ B#2 │ III    │ A3-e19   │   442 │ G#3  │ II     │ A4-e19   │   568 │ m6 - 13-19edo │   821 │
    │ C#3 │ III    │ A4-e19   │   568 │ A#3  │ II     │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ D#3 │ II     │ A1-e19   │    63 │ B#3  │ I      │ A2-e19   │   253 │ M6 - 14-19edo │   884 │
    │ E#3 │ II     │ A2-e19   │   253 │ C#4  │ I      │ M3-e19   │   379 │ m6 - 13-19edo │   821 │
    │ F#3 │ II     │ M3-e19   │   379 │ D#4  │ I      │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ G#3 │ II     │ A4-e19   │   568 │ E#4  │ I      │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ A#3 │ II     │ A5-e19   │   758 │ F#4  │ I      │ M6-e19   │   884 │ m6 - 13-19edo │   821 │
    │ B#3 │ II     │ d7-e19   │   947 │ G#4  │ I      │ M7-e19   │  1074 │ m6 - 13-19edo │   821 │
    │ C#4 │ II     │ M7-e19   │  1074 │ A#4  │ I      │ A1-e19-1 │  1263 │ M6 - 14-19edo │   884 │
    │ D#4 │ II     │ A1-e19-1 │  1263 │ B#4  │ I      │ A2-e19-1 │  1453 │ M6 - 14-19edo │   884 │
    │ E#4 │ II     │ A2-e19-1 │  1453 │ C#5  │ I      │ M3-e19-1 │  1579 │ m6 - 13-19edo │   821 │
    │ F#4 │ II     │ M3-e19-1 │  1579 │ D#5  │ I      │ A4-e19-1 │  1768 │ M6 - 14-19edo │   884 │
    │ G#4 │ II     │ A4-e19-1 │  1768 │ E#5  │ I      │ A5-e19-1 │  1958 │ M6 - 14-19edo │   884 │
    │ A#4 │ II     │ A5-e19-1 │  1958 │ F#5  │ I      │ M6-e19-1 │  2084 │ m6 - 13-19edo │   821 │
    │ B#4 │ II     │ d7-e19-1 │  2147 │ G#5  │ I      │ M7-e19-1 │  2274 │ m6 - 13-19edo │   821 │
    │ C#5 │ II     │ M7-e19-1 │  2274 │ A#5  │ I      │ A1-e19-2 │  2463 │ M6 - 14-19edo │   884 │
    │ D#5 │ II     │ A1-e19-2 │  2463 │ B#5  │ I      │ A2-e19-2 │  2653 │ M6 - 14-19edo │   884 │
    │ E#5 │ II     │ A2-e19-2 │  2653 │ C#6  │ I      │ M3-e19-2 │  2779 │ m6 - 13-19edo │   821 │
    │ F#5 │ II     │ M3-e19-2 │  2779 │ D#6  │ I      │ A4-e19-2 │  2968 │ M6 - 14-19edo │   884 │
    │ G#5 │ II     │ A4-e19-2 │  2968 │ E#6  │ I      │ A5-e19-2 │  3158 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_d_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Db2 │ IV     │ m2-e19   │   126 │ Bb2  │ III    │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ Eb2 │ IV     │ m3-e19   │   316 │ C3   │ III    │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ F2  │ IV     │ 4-e19    │   505 │ Db3  │ III    │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ Gb2 │ IV     │ d5-e19   │   632 │ Eb3  │ III    │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Ab2 │ III    │ m2-e19   │   126 │ F3   │ II     │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ Bb2 │ III    │ m3-e19   │   316 │ Gb3  │ II     │ A3-e19   │   442 │ m6 - 13-19edo │   821 │
    │ C3  │ III    │ 4-e19    │   505 │ Ab3  │ II     │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ Db3 │ III    │ d5-e19   │   632 │ Bb3  │ II     │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Eb3 │ II     │ m2-e19   │   126 │ C4   │ I      │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ F3  │ II     │ m3-e19   │   316 │ Db4  │ I      │ A3-e19   │   442 │ m6 - 13-19edo │   821 │
    │ Gb3 │ II     │ A3-e19   │   442 │ Eb4  │ I      │ d5-e19   │   632 │ M6 - 14-19edo │   884 │
    │ Ab3 │ II     │ d5-e19   │   632 │ F4   │ I      │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Bb3 │ II     │ m6-e19   │   821 │ Gb4  │ I      │ d7-e19   │   947 │ m6 - 13-19edo │   821 │
    │ C4  │ II     │ m7-e19   │  1011 │ Ab4  │ I      │ d8-e19   │  1137 │ m6 - 13-19edo │   821 │
    │ Db4 │ II     │ d8-e19   │  1137 │ Bb4  │ I      │ m2-e19-1 │  1326 │ M6 - 14-19edo │   884 │
    │ Eb4 │ II     │ m2-e19-1 │  1326 │ C5   │ I      │ m3-e19-1 │  1516 │ M6 - 14-19edo │   884 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ Db5  │ I      │ A3-e19-1 │  1642 │ m6 - 13-19edo │   821 │
    │ Gb4 │ II     │ A3-e19-1 │  1642 │ Eb5  │ I      │ d5-e19-1 │  1832 │ M6 - 14-19edo │   884 │
    │ Ab4 │ II     │ d5-e19-1 │  1832 │ F5   │ I      │ m6-e19-1 │  2021 │ M6 - 14-19edo │   884 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ Gb5  │ I      │ d7-e19-1 │  2147 │ m6 - 13-19edo │   821 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ Ab5  │ I      │ d8-e19-1 │  2337 │ m6 - 13-19edo │   821 │
    │ Db5 │ II     │ d8-e19-1 │  2337 │ Bb5  │ I      │ m2-e19-2 │  2526 │ M6 - 14-19edo │   884 │
    │ Eb5 │ II     │ m2-e19-2 │  2526 │ C6   │ I      │ m3-e19-2 │  2716 │ M6 - 14-19edo │   884 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ Db6  │ I      │ A3-e19-2 │  2842 │ m6 - 13-19edo │   821 │
    │ Gb5 │ II     │ A3-e19-2 │  2842 │ Eb6  │ I      │ d5-e19-2 │  3032 │ M6 - 14-19edo │   884 │
    │ Ab5 │ II     │ d5-e19-2 │  3032 │ F6   │ I      │ m6-e19-2 │  3221 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ D2  │ IV     │ M2-e19   │   189 │ B2   │ III    │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ E2  │ IV     │ M3-e19   │   379 │ C#3  │ III    │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ F#2 │ IV     │ A4-e19   │   568 │ D3   │ III    │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ A2  │ III    │ M2-e19   │   189 │ F#3  │ II     │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ B2  │ III    │ M3-e19   │   379 │ G3   │ II     │ 4-e19    │   505 │ m6 - 13-19edo │   821 │
    │ C#3 │ III    │ A4-e19   │   568 │ A3   │ II     │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ E3  │ II     │ M2-e19   │   189 │ C#4  │ I      │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ F#3 │ II     │ M3-e19   │   379 │ D4   │ I      │ 4-e19    │   505 │ m6 - 13-19edo │   821 │
    │ G3  │ II     │ 4-e19    │   505 │ E4   │ I      │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ A3  │ II     │ 5-e19    │   695 │ F#4  │ I      │ M6-e19   │   884 │ M6 - 14-19edo │   884 │
    │ B3  │ II     │ M6-e19   │   884 │ G4   │ I      │ m7-e19   │  1011 │ m6 - 13-19edo │   821 │
    │ C#4 │ II     │ M7-e19   │  1074 │ A4   │ I      │ 0-1      │  1200 │ m6 - 13-19edo │   821 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e19-1 │  1389 │ M6 - 14-19edo │   884 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C#5  │ I      │ M3-e19-1 │  1579 │ M6 - 14-19edo │   884 │
    │ F#4 │ II     │ M3-e19-1 │  1579 │ D5   │ I      │ 4-e19-1  │  1705 │ m6 - 13-19edo │   821 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ E5   │ I      │ 5-e19-1  │  1895 │ M6 - 14-19edo │   884 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F#5  │ I      │ M6-e19-1 │  2084 │ M6 - 14-19edo │   884 │
    │ B4  │ II     │ M6-e19-1 │  2084 │ G5   │ I      │ m7-e19-1 │  2211 │ m6 - 13-19edo │   821 │
    │ C#5 │ II     │ M7-e19-1 │  2274 │ A5   │ I      │ 0-2      │  2400 │ m6 - 13-19edo │   821 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e19-2 │  2589 │ M6 - 14-19edo │   884 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C#6  │ I      │ M3-e19-2 │  2779 │ M6 - 14-19edo │   884 │
    │ F#5 │ II     │ M3-e19-2 │  2779 │ D6   │ I      │ 4-e19-2  │  2905 │ m6 - 13-19edo │   821 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ E6   │ I      │ 5-e19-2  │  3095 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_e_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3-e19   │   316 │ C3   │ III    │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ F2  │ IV     │ 4-e19    │   505 │ D3   │ III    │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ G2  │ III    │ 0        │     0 │ Eb3  │ II     │ m2-e19   │   126 │ m6 - 13-19edo │   821 │
    │ Ab2 │ III    │ m2-e19   │   126 │ F3   │ II     │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ Bb2 │ III    │ m3-e19   │   316 │ G3   │ II     │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ C3  │ III    │ 4-e19    │   505 │ Ab3  │ II     │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e19   │   126 │ m6 - 13-19edo │   821 │
    │ Eb3 │ II     │ m2-e19   │   126 │ C4   │ I      │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ F3  │ II     │ m3-e19   │   316 │ D4   │ I      │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ G3  │ II     │ 4-e19    │   505 │ Eb4  │ I      │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ Ab3 │ II     │ d5-e19   │   632 │ F4   │ I      │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Bb3 │ II     │ m6-e19   │   821 │ G4   │ I      │ m7-e19   │  1011 │ M6 - 14-19edo │   884 │
    │ C4  │ II     │ m7-e19   │  1011 │ Ab4  │ I      │ d8-e19   │  1137 │ m6 - 13-19edo │   821 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e19-1 │  1326 │ m6 - 13-19edo │   821 │
    │ Eb4 │ II     │ m2-e19-1 │  1326 │ C5   │ I      │ m3-e19-1 │  1516 │ M6 - 14-19edo │   884 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ D5   │ I      │ 4-e19-1  │  1705 │ M6 - 14-19edo │   884 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ Eb5  │ I      │ d5-e19-1 │  1832 │ m6 - 13-19edo │   821 │
    │ Ab4 │ II     │ d5-e19-1 │  1832 │ F5   │ I      │ m6-e19-1 │  2021 │ M6 - 14-19edo │   884 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ G5   │ I      │ m7-e19-1 │  2211 │ M6 - 14-19edo │   884 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ Ab5  │ I      │ d8-e19-1 │  2337 │ m6 - 13-19edo │   821 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e19-2 │  2526 │ m6 - 13-19edo │   821 │
    │ Eb5 │ II     │ m2-e19-2 │  2526 │ C6   │ I      │ m3-e19-2 │  2716 │ M6 - 14-19edo │   884 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ D6   │ I      │ 4-e19-2  │  2905 │ M6 - 14-19edo │   884 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ Eb6  │ I      │ d5-e19-2 │  3032 │ m6 - 13-19edo │   821 │
    │ Ab5 │ II     │ d5-e19-2 │  3032 │ F6   │ I      │ m6-e19-2 │  3221 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3-e19   │   379 │ C#3  │ III    │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ F#2 │ IV     │ A4-e19   │   568 │ D#3  │ III    │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ G#2 │ III    │ A1-e19   │    63 │ E3   │ II     │ M2-e19   │   189 │ m6 - 13-19edo │   821 │
    │ A2  │ III    │ M2-e19   │   189 │ F#3  │ II     │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ B2  │ III    │ M3-e19   │   379 │ G#3  │ II     │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ C#3 │ III    │ A4-e19   │   568 │ A3   │ II     │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ D#3 │ II     │ A1-e19   │    63 │ B3   │ I      │ M2-e19   │   189 │ m6 - 13-19edo │   821 │
    │ E3  │ II     │ M2-e19   │   189 │ C#4  │ I      │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ F#3 │ II     │ M3-e19   │   379 │ D#4  │ I      │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ G#3 │ II     │ A4-e19   │   568 │ E4   │ I      │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ A3  │ II     │ 5-e19    │   695 │ F#4  │ I      │ M6-e19   │   884 │ M6 - 14-19edo │   884 │
    │ B3  │ II     │ M6-e19   │   884 │ G#4  │ I      │ M7-e19   │  1074 │ M6 - 14-19edo │   884 │
    │ C#4 │ II     │ M7-e19   │  1074 │ A4   │ I      │ 0-1      │  1200 │ m6 - 13-19edo │   821 │
    │ D#4 │ II     │ A1-e19-1 │  1263 │ B4   │ I      │ M2-e19-1 │  1389 │ m6 - 13-19edo │   821 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C#5  │ I      │ M3-e19-1 │  1579 │ M6 - 14-19edo │   884 │
    │ F#4 │ II     │ M3-e19-1 │  1579 │ D#5  │ I      │ A4-e19-1 │  1768 │ M6 - 14-19edo │   884 │
    │ G#4 │ II     │ A4-e19-1 │  1768 │ E5   │ I      │ 5-e19-1  │  1895 │ m6 - 13-19edo │   821 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F#5  │ I      │ M6-e19-1 │  2084 │ M6 - 14-19edo │   884 │
    │ B4  │ II     │ M6-e19-1 │  2084 │ G#5  │ I      │ M7-e19-1 │  2274 │ M6 - 14-19edo │   884 │
    │ C#5 │ II     │ M7-e19-1 │  2274 │ A5   │ I      │ 0-2      │  2400 │ m6 - 13-19edo │   821 │
    │ D#5 │ II     │ A1-e19-2 │  2463 │ B5   │ I      │ M2-e19-2 │  2589 │ m6 - 13-19edo │   821 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C#6  │ I      │ M3-e19-2 │  2779 │ M6 - 14-19edo │   884 │
    │ F#5 │ II     │ M3-e19-2 │  2779 │ D#6  │ I      │ A4-e19-2 │  2968 │ M6 - 14-19edo │   884 │
    │ G#5 │ II     │ A4-e19-2 │  2968 │ E6   │ I      │ 5-e19-2  │  3095 │ m6 - 13-19edo │   821 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e19    │   505 │ D3   │ III    │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ A2  │ III    │ M2-e19   │   189 │ F3   │ II     │ m3-e19   │   316 │ m6 - 13-19edo │   821 │
    │ Bb2 │ III    │ m3-e19   │   316 │ G3   │ II     │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ C3  │ III    │ 4-e19    │   505 │ A3   │ II     │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e19   │   126 │ m6 - 13-19edo │   821 │
    │ E3  │ II     │ M2-e19   │   189 │ C4   │ I      │ m3-e19   │   316 │ m6 - 13-19edo │   821 │
    │ F3  │ II     │ m3-e19   │   316 │ D4   │ I      │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ G3  │ II     │ 4-e19    │   505 │ E4   │ I      │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ A3  │ II     │ 5-e19    │   695 │ F4   │ I      │ m6-e19   │   821 │ m6 - 13-19edo │   821 │
    │ Bb3 │ II     │ m6-e19   │   821 │ G4   │ I      │ m7-e19   │  1011 │ M6 - 14-19edo │   884 │
    │ C4  │ II     │ m7-e19   │  1011 │ A4   │ I      │ 0-1      │  1200 │ M6 - 14-19edo │   884 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e19-1 │  1326 │ m6 - 13-19edo │   821 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C5   │ I      │ m3-e19-1 │  1516 │ m6 - 13-19edo │   821 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ D5   │ I      │ 4-e19-1  │  1705 │ M6 - 14-19edo │   884 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ E5   │ I      │ 5-e19-1  │  1895 │ M6 - 14-19edo │   884 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F5   │ I      │ m6-e19-1 │  2021 │ m6 - 13-19edo │   821 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ G5   │ I      │ m7-e19-1 │  2211 │ M6 - 14-19edo │   884 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ A5   │ I      │ 0-2      │  2400 │ M6 - 14-19edo │   884 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e19-2 │  2526 │ m6 - 13-19edo │   821 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C6   │ I      │ m3-e19-2 │  2716 │ m6 - 13-19edo │   821 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ D6   │ I      │ 4-e19-2  │  2905 │ M6 - 14-19edo │   884 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ E6   │ I      │ 5-e19-2  │  3095 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_f_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ A4-e19   │   568 │ D#3  │ III    │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ G#2 │ III    │ A1-e19   │    63 │ E#3  │ II     │ A2-e19   │   253 │ M6 - 14-19edo │   884 │
    │ A#2 │ III    │ A2-e19   │   253 │ F#3  │ II     │ M3-e19   │   379 │ m6 - 13-19edo │   821 │
    │ B2  │ III    │ M3-e19   │   379 │ G#3  │ II     │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ C#3 │ III    │ A4-e19   │   568 │ A#3  │ II     │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ D#3 │ II     │ A1-e19   │    63 │ B3   │ I      │ M2-e19   │   189 │ m6 - 13-19edo │   821 │
    │ E#3 │ II     │ A2-e19   │   253 │ C#4  │ I      │ M3-e19   │   379 │ m6 - 13-19edo │   821 │
    │ F#3 │ II     │ M3-e19   │   379 │ D#4  │ I      │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ G#3 │ II     │ A4-e19   │   568 │ E#4  │ I      │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ A#3 │ II     │ A5-e19   │   758 │ F#4  │ I      │ M6-e19   │   884 │ m6 - 13-19edo │   821 │
    │ B3  │ II     │ M6-e19   │   884 │ G#4  │ I      │ M7-e19   │  1074 │ M6 - 14-19edo │   884 │
    │ C#4 │ II     │ M7-e19   │  1074 │ A#4  │ I      │ A1-e19-1 │  1263 │ M6 - 14-19edo │   884 │
    │ D#4 │ II     │ A1-e19-1 │  1263 │ B4   │ I      │ M2-e19-1 │  1389 │ m6 - 13-19edo │   821 │
    │ E#4 │ II     │ A2-e19-1 │  1453 │ C#5  │ I      │ M3-e19-1 │  1579 │ m6 - 13-19edo │   821 │
    │ F#4 │ II     │ M3-e19-1 │  1579 │ D#5  │ I      │ A4-e19-1 │  1768 │ M6 - 14-19edo │   884 │
    │ G#4 │ II     │ A4-e19-1 │  1768 │ E#5  │ I      │ A5-e19-1 │  1958 │ M6 - 14-19edo │   884 │
    │ A#4 │ II     │ A5-e19-1 │  1958 │ F#5  │ I      │ M6-e19-1 │  2084 │ m6 - 13-19edo │   821 │
    │ B4  │ II     │ M6-e19-1 │  2084 │ G#5  │ I      │ M7-e19-1 │  2274 │ M6 - 14-19edo │   884 │
    │ C#5 │ II     │ M7-e19-1 │  2274 │ A#5  │ I      │ A1-e19-2 │  2463 │ M6 - 14-19edo │   884 │
    │ D#5 │ II     │ A1-e19-2 │  2463 │ B5   │ I      │ M2-e19-2 │  2589 │ m6 - 13-19edo │   821 │
    │ E#5 │ II     │ A2-e19-2 │  2653 │ C#6  │ I      │ M3-e19-2 │  2779 │ m6 - 13-19edo │   821 │
    │ F#5 │ II     │ M3-e19-2 │  2779 │ D#6  │ I      │ A4-e19-2 │  2968 │ M6 - 14-19edo │   884 │
    │ G#5 │ II     │ A4-e19-2 │  2968 │ E#6  │ I      │ A5-e19-2 │  3158 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_g_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ d5-e19   │   632 │ Eb3  │ III    │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Ab2 │ III    │ m2-e19   │   126 │ F3   │ II     │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ Bb2 │ III    │ m3-e19   │   316 │ Gb3  │ II     │ A3-e19   │   442 │ m6 - 13-19edo │   821 │
    │ Cb3 │ III    │ A3-e19   │   442 │ Ab3  │ II     │ d5-e19   │   632 │ M6 - 14-19edo │   884 │
    │ Db3 │ III    │ d5-e19   │   632 │ Bb3  │ II     │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Eb3 │ II     │ m2-e19   │   126 │ Cb4  │ I      │ A2-e19   │   253 │ m6 - 13-19edo │   821 │
    │ F3  │ II     │ m3-e19   │   316 │ Db4  │ I      │ A3-e19   │   442 │ m6 - 13-19edo │   821 │
    │ Gb3 │ II     │ A3-e19   │   442 │ Eb4  │ I      │ d5-e19   │   632 │ M6 - 14-19edo │   884 │
    │ Ab3 │ II     │ d5-e19   │   632 │ F4   │ I      │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Bb3 │ II     │ m6-e19   │   821 │ Gb4  │ I      │ d7-e19   │   947 │ m6 - 13-19edo │   821 │
    │ Cb4 │ II     │ d7-e19   │   947 │ Ab4  │ I      │ d8-e19   │  1137 │ M6 - 14-19edo │   884 │
    │ Db4 │ II     │ d8-e19   │  1137 │ Bb4  │ I      │ m2-e19-1 │  1326 │ M6 - 14-19edo │   884 │
    │ Eb4 │ II     │ m2-e19-1 │  1326 │ Cb5  │ I      │ A2-e19-1 │  1453 │ m6 - 13-19edo │   821 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ Db5  │ I      │ A3-e19-1 │  1642 │ m6 - 13-19edo │   821 │
    │ Gb4 │ II     │ A3-e19-1 │  1642 │ Eb5  │ I      │ d5-e19-1 │  1832 │ M6 - 14-19edo │   884 │
    │ Ab4 │ II     │ d5-e19-1 │  1832 │ F5   │ I      │ m6-e19-1 │  2021 │ M6 - 14-19edo │   884 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ Gb5  │ I      │ d7-e19-1 │  2147 │ m6 - 13-19edo │   821 │
    │ Cb5 │ II     │ d7-e19-1 │  2147 │ Ab5  │ I      │ d8-e19-1 │  2337 │ M6 - 14-19edo │   884 │
    │ Db5 │ II     │ d8-e19-1 │  2337 │ Bb5  │ I      │ m2-e19-2 │  2526 │ M6 - 14-19edo │   884 │
    │ Eb5 │ II     │ m2-e19-2 │  2526 │ Cb6  │ I      │ A2-e19-2 │  2653 │ m6 - 13-19edo │   821 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ Db6  │ I      │ A3-e19-2 │  2842 │ m6 - 13-19edo │   821 │
    │ Gb5 │ II     │ A3-e19-2 │  2842 │ Eb6  │ I      │ d5-e19-2 │  3032 │ M6 - 14-19edo │   884 │
    │ Ab5 │ II     │ d5-e19-2 │  3032 │ F6   │ I      │ m6-e19-2 │  3221 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e19    │   505 │ D3   │ III    │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ A2  │ III    │ M2-e19   │   189 │ F3   │ II     │ m3-e19   │   316 │ m6 - 13-19edo │   821 │
    │ Bb2 │ III    │ m3-e19   │   316 │ G3   │ II     │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ C3  │ III    │ 4-e19    │   505 │ A3   │ II     │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e19   │   126 │ m6 - 13-19edo │   821 │
    │ E3  │ II     │ M2-e19   │   189 │ C4   │ I      │ m3-e19   │   316 │ m6 - 13-19edo │   821 │
    │ F3  │ II     │ m3-e19   │   316 │ D4   │ I      │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ G3  │ II     │ 4-e19    │   505 │ E4   │ I      │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ A3  │ II     │ 5-e19    │   695 │ F4   │ I      │ m6-e19   │   821 │ m6 - 13-19edo │   821 │
    │ Bb3 │ II     │ m6-e19   │   821 │ G4   │ I      │ m7-e19   │  1011 │ M6 - 14-19edo │   884 │
    │ C4  │ II     │ m7-e19   │  1011 │ A4   │ I      │ 0-1      │  1200 │ M6 - 14-19edo │   884 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e19-1 │  1326 │ m6 - 13-19edo │   821 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C5   │ I      │ m3-e19-1 │  1516 │ m6 - 13-19edo │   821 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ D5   │ I      │ 4-e19-1  │  1705 │ M6 - 14-19edo │   884 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ E5   │ I      │ 5-e19-1  │  1895 │ M6 - 14-19edo │   884 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F5   │ I      │ m6-e19-1 │  2021 │ m6 - 13-19edo │   821 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ G5   │ I      │ m7-e19-1 │  2211 │ M6 - 14-19edo │   884 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ A5   │ I      │ 0-2      │  2400 │ M6 - 14-19edo │   884 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e19-2 │  2526 │ m6 - 13-19edo │   821 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C6   │ I      │ m3-e19-2 │  2716 │ m6 - 13-19edo │   821 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ D6   │ I      │ 4-e19-2  │  2905 │ M6 - 14-19edo │   884 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ E6   │ I      │ 5-e19-2  │  3095 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_a_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Ab2 │ III    │ m2-e19   │   126 │ F3   │ II     │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ Bb2 │ III    │ m3-e19   │   316 │ G3   │ II     │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ C3  │ III    │ 4-e19    │   505 │ Ab3  │ II     │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ Db3 │ III    │ d5-e19   │   632 │ Bb3  │ II     │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Eb3 │ II     │ m2-e19   │   126 │ C4   │ I      │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ F3  │ II     │ m3-e19   │   316 │ Db4  │ I      │ A3-e19   │   442 │ m6 - 13-19edo │   821 │
    │ G3  │ II     │ 4-e19    │   505 │ Eb4  │ I      │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ Ab3 │ II     │ d5-e19   │   632 │ F4   │ I      │ m6-e19   │   821 │ M6 - 14-19edo │   884 │
    │ Bb3 │ II     │ m6-e19   │   821 │ G4   │ I      │ m7-e19   │  1011 │ M6 - 14-19edo │   884 │
    │ C4  │ II     │ m7-e19   │  1011 │ Ab4  │ I      │ d8-e19   │  1137 │ m6 - 13-19edo │   821 │
    │ Db4 │ II     │ d8-e19   │  1137 │ Bb4  │ I      │ m2-e19-1 │  1326 │ M6 - 14-19edo │   884 │
    │ Eb4 │ II     │ m2-e19-1 │  1326 │ C5   │ I      │ m3-e19-1 │  1516 │ M6 - 14-19edo │   884 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ Db5  │ I      │ A3-e19-1 │  1642 │ m6 - 13-19edo │   821 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ Eb5  │ I      │ d5-e19-1 │  1832 │ m6 - 13-19edo │   821 │
    │ Ab4 │ II     │ d5-e19-1 │  1832 │ F5   │ I      │ m6-e19-1 │  2021 │ M6 - 14-19edo │   884 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ G5   │ I      │ m7-e19-1 │  2211 │ M6 - 14-19edo │   884 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ Ab5  │ I      │ d8-e19-1 │  2337 │ m6 - 13-19edo │   821 │
    │ Db5 │ II     │ d8-e19-1 │  2337 │ Bb5  │ I      │ m2-e19-2 │  2526 │ M6 - 14-19edo │   884 │
    │ Eb5 │ II     │ m2-e19-2 │  2526 │ C6   │ I      │ m3-e19-2 │  2716 │ M6 - 14-19edo │   884 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ Db6  │ I      │ A3-e19-2 │  2842 │ m6 - 13-19edo │   821 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ Eb6  │ I      │ d5-e19-2 │  3032 │ m6 - 13-19edo │   821 │
    │ Ab5 │ II     │ d5-e19-2 │  3032 │ F6   │ I      │ m6-e19-2 │  3221 │ M6 - 14-19edo │   884 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ A2  │ III    │ M2-e19   │   189 │ F#3  │ II     │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ B2  │ III    │ M3-e19   │   379 │ G#3  │ II     │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ C#3 │ III    │ A4-e19   │   568 │ A3   │ II     │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e19   │   189 │ M6 - 14-19edo │   884 │
    │ E3  │ II     │ M2-e19   │   189 │ C#4  │ I      │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ F#3 │ II     │ M3-e19   │   379 │ D4   │ I      │ 4-e19    │   505 │ m6 - 13-19edo │   821 │
    │ G#3 │ II     │ A4-e19   │   568 │ E4   │ I      │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ A3  │ II     │ 5-e19    │   695 │ F#4  │ I      │ M6-e19   │   884 │ M6 - 14-19edo │   884 │
    │ B3  │ II     │ M6-e19   │   884 │ G#4  │ I      │ M7-e19   │  1074 │ M6 - 14-19edo │   884 │
    │ C#4 │ II     │ M7-e19   │  1074 │ A4   │ I      │ 0-1      │  1200 │ m6 - 13-19edo │   821 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e19-1 │  1389 │ M6 - 14-19edo │   884 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C#5  │ I      │ M3-e19-1 │  1579 │ M6 - 14-19edo │   884 │
    │ F#4 │ II     │ M3-e19-1 │  1579 │ D5   │ I      │ 4-e19-1  │  1705 │ m6 - 13-19edo │   821 │
    │ G#4 │ II     │ A4-e19-1 │  1768 │ E5   │ I      │ 5-e19-1  │  1895 │ m6 - 13-19edo │   821 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F#5  │ I      │ M6-e19-1 │  2084 │ M6 - 14-19edo │   884 │
    │ B4  │ II     │ M6-e19-1 │  2084 │ G#5  │ I      │ M7-e19-1 │  2274 │ M6 - 14-19edo │   884 │
    │ C#5 │ II     │ M7-e19-1 │  2274 │ A5   │ I      │ 0-2      │  2400 │ m6 - 13-19edo │   821 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e19-2 │  2589 │ M6 - 14-19edo │   884 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C#6  │ I      │ M3-e19-2 │  2779 │ M6 - 14-19edo │   884 │
    │ F#5 │ II     │ M3-e19-2 │  2779 │ D6   │ I      │ 4-e19-2  │  2905 │ m6 - 13-19edo │   821 │
    │ G#5 │ II     │ A4-e19-2 │  2968 │ E6   │ I      │ 5-e19-2  │  3095 │ m6 - 13-19edo │   821 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_b_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3-e19   │   316 │ G3   │ II     │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ C3  │ III    │ 4-e19    │   505 │ A3   │ II     │ 5-e19    │   695 │ M6 - 14-19edo │   884 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e19   │   126 │ m6 - 13-19edo │   821 │
    │ Eb3 │ II     │ m2-e19   │   126 │ C4   │ I      │ m3-e19   │   316 │ M6 - 14-19edo │   884 │
    │ F3  │ II     │ m3-e19   │   316 │ D4   │ I      │ 4-e19    │   505 │ M6 - 14-19edo │   884 │
    │ G3  │ II     │ 4-e19    │   505 │ Eb4  │ I      │ d5-e19   │   632 │ m6 - 13-19edo │   821 │
    │ A3  │ II     │ 5-e19    │   695 │ F4   │ I      │ m6-e19   │   821 │ m6 - 13-19edo │   821 │
    │ Bb3 │ II     │ m6-e19   │   821 │ G4   │ I      │ m7-e19   │  1011 │ M6 - 14-19edo │   884 │
    │ C4  │ II     │ m7-e19   │  1011 │ A4   │ I      │ 0-1      │  1200 │ M6 - 14-19edo │   884 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e19-1 │  1326 │ m6 - 13-19edo │   821 │
    │ Eb4 │ II     │ m2-e19-1 │  1326 │ C5   │ I      │ m3-e19-1 │  1516 │ M6 - 14-19edo │   884 │
    │ F4  │ II     │ m3-e19-1 │  1516 │ D5   │ I      │ 4-e19-1  │  1705 │ M6 - 14-19edo │   884 │
    │ G4  │ II     │ 4-e19-1  │  1705 │ Eb5  │ I      │ d5-e19-1 │  1832 │ m6 - 13-19edo │   821 │
    │ A4  │ II     │ 5-e19-1  │  1895 │ F5   │ I      │ m6-e19-1 │  2021 │ m6 - 13-19edo │   821 │
    │ Bb4 │ II     │ m6-e19-1 │  2021 │ G5   │ I      │ m7-e19-1 │  2211 │ M6 - 14-19edo │   884 │
    │ C5  │ II     │ m7-e19-1 │  2211 │ A5   │ I      │ 0-2      │  2400 │ M6 - 14-19edo │   884 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e19-2 │  2526 │ m6 - 13-19edo │   821 │
    │ Eb5 │ II     │ m2-e19-2 │  2526 │ C6   │ I      │ m3-e19-2 │  2716 │ M6 - 14-19edo │   884 │
    │ F5  │ II     │ m3-e19-2 │  2716 │ D6   │ I      │ 4-e19-2  │  2905 │ M6 - 14-19edo │   884 │
    │ G5  │ II     │ 4-e19-2  │  2905 │ Eb6  │ I      │ d5-e19-2 │  3032 │ m6 - 13-19edo │   821 │
    │ A5  │ II     │ 5-e19-2  │  3095 │ F6   │ I      │ m6-e19-2 │  3221 │ m6 - 13-19edo │   821 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major" =
  let t = Lazy.force E19.t in
  let scale = make_major_scale ~from:Scales.lower_b in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3-e19   │   379 │ G#3  │ II     │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ C#3 │ III    │ A4-e19   │   568 │ A#3  │ II     │ A5-e19   │   758 │ M6 - 14-19edo │   884 │
    │ D#3 │ II     │ A1-e19   │    63 │ B3   │ I      │ M2-e19   │   189 │ m6 - 13-19edo │   821 │
    │ E3  │ II     │ M2-e19   │   189 │ C#4  │ I      │ M3-e19   │   379 │ M6 - 14-19edo │   884 │
    │ F#3 │ II     │ M3-e19   │   379 │ D#4  │ I      │ A4-e19   │   568 │ M6 - 14-19edo │   884 │
    │ G#3 │ II     │ A4-e19   │   568 │ E4   │ I      │ 5-e19    │   695 │ m6 - 13-19edo │   821 │
    │ A#3 │ II     │ A5-e19   │   758 │ F#4  │ I      │ M6-e19   │   884 │ m6 - 13-19edo │   821 │
    │ B3  │ II     │ M6-e19   │   884 │ G#4  │ I      │ M7-e19   │  1074 │ M6 - 14-19edo │   884 │
    │ C#4 │ II     │ M7-e19   │  1074 │ A#4  │ I      │ A1-e19-1 │  1263 │ M6 - 14-19edo │   884 │
    │ D#4 │ II     │ A1-e19-1 │  1263 │ B4   │ I      │ M2-e19-1 │  1389 │ m6 - 13-19edo │   821 │
    │ E4  │ II     │ M2-e19-1 │  1389 │ C#5  │ I      │ M3-e19-1 │  1579 │ M6 - 14-19edo │   884 │
    │ F#4 │ II     │ M3-e19-1 │  1579 │ D#5  │ I      │ A4-e19-1 │  1768 │ M6 - 14-19edo │   884 │
    │ G#4 │ II     │ A4-e19-1 │  1768 │ E5   │ I      │ 5-e19-1  │  1895 │ m6 - 13-19edo │   821 │
    │ A#4 │ II     │ A5-e19-1 │  1958 │ F#5  │ I      │ M6-e19-1 │  2084 │ m6 - 13-19edo │   821 │
    │ B4  │ II     │ M6-e19-1 │  2084 │ G#5  │ I      │ M7-e19-1 │  2274 │ M6 - 14-19edo │   884 │
    │ C#5 │ II     │ M7-e19-1 │  2274 │ A#5  │ I      │ A1-e19-2 │  2463 │ M6 - 14-19edo │   884 │
    │ D#5 │ II     │ A1-e19-2 │  2463 │ B5   │ I      │ M2-e19-2 │  2589 │ m6 - 13-19edo │   821 │
    │ E5  │ II     │ M2-e19-2 │  2589 │ C#6  │ I      │ M3-e19-2 │  2779 │ M6 - 14-19edo │   884 │
    │ F#5 │ II     │ M3-e19-2 │  2779 │ D#6  │ I      │ A4-e19-2 │  2968 │ M6 - 14-19edo │   884 │
    │ G#5 │ II     │ A4-e19-2 │  2968 │ E6   │ I      │ 5-e19-2  │  3095 │ m6 - 13-19edo │   821 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;
