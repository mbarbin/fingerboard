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
  let t = force E19.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Third
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_e19 ~from
;;

let%expect_test "c_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ E2  │ IV     │ M3-e19   │ 379   │ G2   │ III    │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ F2  │ IV     │ 4-e19    │ 505   │ A2   │ III    │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ G2  │ IV     │ 5-e19    │ 695   │ B2   │ III    │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ A2  │ IV     │ M6-e19   │ 884   │ C3   │ III    │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ B2  │ III    │ M3-e19   │ 379   │ D3   │ II     │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ C3  │ III    │ 4-e19    │ 505   │ E3   │ II     │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ D3  │ III    │ 5-e19    │ 695   │ F3   │ II     │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ E3  │ III    │ M6-e19   │ 884   │ G3   │ II     │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ F3  │ II     │ m3-e19   │ 316   │ A3   │ I      │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ G3  │ II     │ 4-e19    │ 505   │ B3   │ I      │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ A3  │ II     │ 5-e19    │ 695   │ C4   │ I      │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ B3  │ II     │ M6-e19   │ 884   │ D4   │ I      │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ C4  │ II     │ m7-e19   │ 1011  │ E4   │ I      │ 5-e19    │ 695   │ M3 - 6-19edo │ 379   │
    │ D4  │ II     │ 0-1      │ 1200  │ F4   │ I      │ m6-e19   │ 821   │ m3 - 5-19edo │ 316   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G4   │ I      │ m7-e19   │ 1011  │ m3 - 5-19edo │ 316   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ A4   │ I      │ 0-1      │ 1200  │ M3 - 6-19edo │ 379   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ B4   │ I      │ M2-e19-1 │ 1389  │ M3 - 6-19edo │ 379   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C5   │ I      │ m3-e19-1 │ 1516  │ m3 - 5-19edo │ 316   │
    │ B4  │ II     │ M6-e19-1 │ 2084  │ D5   │ I      │ 4-e19-1  │ 1705  │ m3 - 5-19edo │ 316   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ E5   │ I      │ 5-e19-1  │ 1895  │ M3 - 6-19edo │ 379   │
    │ D5  │ II     │ 0-2      │ 2400  │ F5   │ I      │ m6-e19-1 │ 2021  │ m3 - 5-19edo │ 316   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G5   │ I      │ m7-e19-1 │ 2211  │ m3 - 5-19edo │ 316   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ A5   │ I      │ 0-2      │ 2400  │ M3 - 6-19edo │ 379   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ B5   │ I      │ M2-e19-2 │ 2589  │ M3 - 6-19edo │ 379   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C6   │ I      │ m3-e19-2 │ 2716  │ m3 - 5-19edo │ 316   │
    │ B5  │ II     │ M6-e19-2 │ 3284  │ D6   │ I      │ 4-e19-2  │ 2905  │ m3 - 5-19edo │ 316   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ E6   │ I      │ 5-e19-2  │ 3095  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_c_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ E#2 │ IV     │ A3-e19   │ 442   │ G#2  │ III    │ A1-e19   │ 63    │ m3 - 5-19edo │ 316   │
    │ F#2 │ IV     │ A4-e19   │ 568   │ A#2  │ III    │ A2-e19   │ 253   │ M3 - 6-19edo │ 379   │
    │ G#2 │ IV     │ A5-e19   │ 758   │ B#2  │ III    │ A3-e19   │ 442   │ M3 - 6-19edo │ 379   │
    │ A#2 │ IV     │ d7-e19   │ 947   │ C#3  │ III    │ A4-e19   │ 568   │ m3 - 5-19edo │ 316   │
    │ B#2 │ III    │ A3-e19   │ 442   │ D#3  │ II     │ A1-e19   │ 63    │ m3 - 5-19edo │ 316   │
    │ C#3 │ III    │ A4-e19   │ 568   │ E#3  │ II     │ A2-e19   │ 253   │ M3 - 6-19edo │ 379   │
    │ D#3 │ III    │ A5-e19   │ 758   │ F#3  │ II     │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ E#3 │ III    │ d7-e19   │ 947   │ G#3  │ II     │ A4-e19   │ 568   │ m3 - 5-19edo │ 316   │
    │ F#3 │ II     │ M3-e19   │ 379   │ A#3  │ I      │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ G#3 │ II     │ A4-e19   │ 568   │ B#3  │ I      │ A2-e19   │ 253   │ M3 - 6-19edo │ 379   │
    │ A#3 │ II     │ A5-e19   │ 758   │ C#4  │ I      │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ B#3 │ II     │ d7-e19   │ 947   │ D#4  │ I      │ A4-e19   │ 568   │ m3 - 5-19edo │ 316   │
    │ C#4 │ II     │ M7-e19   │ 1074  │ E#4  │ I      │ A5-e19   │ 758   │ M3 - 6-19edo │ 379   │
    │ D#4 │ II     │ A1-e19-1 │ 1263  │ F#4  │ I      │ M6-e19   │ 884   │ m3 - 5-19edo │ 316   │
    │ E#4 │ II     │ A2-e19-1 │ 1453  │ G#4  │ I      │ M7-e19   │ 1074  │ m3 - 5-19edo │ 316   │
    │ F#4 │ II     │ M3-e19-1 │ 1579  │ A#4  │ I      │ A1-e19-1 │ 1263  │ M3 - 6-19edo │ 379   │
    │ G#4 │ II     │ A4-e19-1 │ 1768  │ B#4  │ I      │ A2-e19-1 │ 1453  │ M3 - 6-19edo │ 379   │
    │ A#4 │ II     │ A5-e19-1 │ 1958  │ C#5  │ I      │ M3-e19-1 │ 1579  │ m3 - 5-19edo │ 316   │
    │ B#4 │ II     │ d7-e19-1 │ 2147  │ D#5  │ I      │ A4-e19-1 │ 1768  │ m3 - 5-19edo │ 316   │
    │ C#5 │ II     │ M7-e19-1 │ 2274  │ E#5  │ I      │ A5-e19-1 │ 1958  │ M3 - 6-19edo │ 379   │
    │ D#5 │ II     │ A1-e19-2 │ 2463  │ F#5  │ I      │ M6-e19-1 │ 2084  │ m3 - 5-19edo │ 316   │
    │ E#5 │ II     │ A2-e19-2 │ 2653  │ G#5  │ I      │ M7-e19-1 │ 2274  │ m3 - 5-19edo │ 316   │
    │ F#5 │ II     │ M3-e19-2 │ 2779  │ A#5  │ I      │ A1-e19-2 │ 2463  │ M3 - 6-19edo │ 379   │
    │ G#5 │ II     │ A4-e19-2 │ 2968  │ B#5  │ I      │ A2-e19-2 │ 2653  │ M3 - 6-19edo │ 379   │
    │ A#5 │ II     │ A5-e19-2 │ 3158  │ C#6  │ I      │ M3-e19-2 │ 2779  │ m3 - 5-19edo │ 316   │
    │ B#5 │ II     │ d7-e19-2 │ 3347  │ D#6  │ I      │ A4-e19-2 │ 2968  │ m3 - 5-19edo │ 316   │
    │ C#6 │ II     │ M7-e19-2 │ 3474  │ E#6  │ I      │ A5-e19-2 │ 3158  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_d_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ F2  │ IV     │ 4-e19    │ 505   │ Ab2  │ III    │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ Gb2 │ IV     │ d5-e19   │ 632   │ Bb2  │ III    │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Ab2 │ IV     │ m6-e19   │ 821   │ C3   │ III    │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ Bb2 │ IV     │ m7-e19   │ 1011  │ Db3  │ III    │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ C3  │ III    │ 4-e19    │ 505   │ Eb3  │ II     │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ Db3 │ III    │ d5-e19   │ 632   │ F3   │ II     │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Eb3 │ III    │ m6-e19   │ 821   │ Gb3  │ II     │ A3-e19   │ 442   │ m3 - 5-19edo │ 316   │
    │ F3  │ III    │ m7-e19   │ 1011  │ Ab3  │ II     │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ Gb3 │ II     │ A3-e19   │ 442   │ Bb3  │ I      │ m2-e19   │ 126   │ M3 - 6-19edo │ 379   │
    │ Ab3 │ II     │ d5-e19   │ 632   │ C4   │ I      │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ Db4  │ I      │ A3-e19   │ 442   │ m3 - 5-19edo │ 316   │
    │ C4  │ II     │ m7-e19   │ 1011  │ Eb4  │ I      │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ Db4 │ II     │ d8-e19   │ 1137  │ F4   │ I      │ m6-e19   │ 821   │ M3 - 6-19edo │ 379   │
    │ Eb4 │ II     │ m2-e19-1 │ 1326  │ Gb4  │ I      │ d7-e19   │ 947   │ m3 - 5-19edo │ 316   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ Ab4  │ I      │ d8-e19   │ 1137  │ m3 - 5-19edo │ 316   │
    │ Gb4 │ II     │ A3-e19-1 │ 1642  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ M3 - 6-19edo │ 379   │
    │ Ab4 │ II     │ d5-e19-1 │ 1832  │ C5   │ I      │ m3-e19-1 │ 1516  │ M3 - 6-19edo │ 379   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ Db5  │ I      │ A3-e19-1 │ 1642  │ m3 - 5-19edo │ 316   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ Eb5  │ I      │ d5-e19-1 │ 1832  │ m3 - 5-19edo │ 316   │
    │ Db5 │ II     │ d8-e19-1 │ 2337  │ F5   │ I      │ m6-e19-1 │ 2021  │ M3 - 6-19edo │ 379   │
    │ Eb5 │ II     │ m2-e19-2 │ 2526  │ Gb5  │ I      │ d7-e19-1 │ 2147  │ m3 - 5-19edo │ 316   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ Ab5  │ I      │ d8-e19-1 │ 2337  │ m3 - 5-19edo │ 316   │
    │ Gb5 │ II     │ A3-e19-2 │ 2842  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ M3 - 6-19edo │ 379   │
    │ Ab5 │ II     │ d5-e19-2 │ 3032  │ C6   │ I      │ m3-e19-2 │ 2716  │ M3 - 6-19edo │ 379   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ Db6  │ I      │ A3-e19-2 │ 2842  │ m3 - 5-19edo │ 316   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ Eb6  │ I      │ d5-e19-2 │ 3032  │ m3 - 5-19edo │ 316   │
    │ Db6 │ II     │ d8-e19-2 │ 3537  │ F6   │ I      │ m6-e19-2 │ 3221  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "d_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ E2  │ IV     │ M3-e19   │ 379   │ G2   │ III    │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ F#2 │ IV     │ A4-e19   │ 568   │ A2   │ III    │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ G2  │ IV     │ 5-e19    │ 695   │ B2   │ III    │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ A2  │ IV     │ M6-e19   │ 884   │ C#3  │ III    │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ B2  │ III    │ M3-e19   │ 379   │ D3   │ II     │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ C#3 │ III    │ A4-e19   │ 568   │ E3   │ II     │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ D3  │ III    │ 5-e19    │ 695   │ F#3  │ II     │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ E3  │ III    │ M6-e19   │ 884   │ G3   │ II     │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ F#3 │ II     │ M3-e19   │ 379   │ A3   │ I      │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ G3  │ II     │ 4-e19    │ 505   │ B3   │ I      │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ A3  │ II     │ 5-e19    │ 695   │ C#4  │ I      │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ B3  │ II     │ M6-e19   │ 884   │ D4   │ I      │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ C#4 │ II     │ M7-e19   │ 1074  │ E4   │ I      │ 5-e19    │ 695   │ m3 - 5-19edo │ 316   │
    │ D4  │ II     │ 0-1      │ 1200  │ F#4  │ I      │ M6-e19   │ 884   │ M3 - 6-19edo │ 379   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G4   │ I      │ m7-e19   │ 1011  │ m3 - 5-19edo │ 316   │
    │ F#4 │ II     │ M3-e19-1 │ 1579  │ A4   │ I      │ 0-1      │ 1200  │ m3 - 5-19edo │ 316   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ B4   │ I      │ M2-e19-1 │ 1389  │ M3 - 6-19edo │ 379   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C#5  │ I      │ M3-e19-1 │ 1579  │ M3 - 6-19edo │ 379   │
    │ B4  │ II     │ M6-e19-1 │ 2084  │ D5   │ I      │ 4-e19-1  │ 1705  │ m3 - 5-19edo │ 316   │
    │ C#5 │ II     │ M7-e19-1 │ 2274  │ E5   │ I      │ 5-e19-1  │ 1895  │ m3 - 5-19edo │ 316   │
    │ D5  │ II     │ 0-2      │ 2400  │ F#5  │ I      │ M6-e19-1 │ 2084  │ M3 - 6-19edo │ 379   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G5   │ I      │ m7-e19-1 │ 2211  │ m3 - 5-19edo │ 316   │
    │ F#5 │ II     │ M3-e19-2 │ 2779  │ A5   │ I      │ 0-2      │ 2400  │ m3 - 5-19edo │ 316   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ B5   │ I      │ M2-e19-2 │ 2589  │ M3 - 6-19edo │ 379   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C#6  │ I      │ M3-e19-2 │ 2779  │ M3 - 6-19edo │ 379   │
    │ B5  │ II     │ M6-e19-2 │ 3284  │ D6   │ I      │ 4-e19-2  │ 2905  │ m3 - 5-19edo │ 316   │
    │ C#6 │ II     │ M7-e19-2 │ 3474  │ E6   │ I      │ 5-e19-2  │ 3095  │ m3 - 5-19edo │ 316   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_e_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ Eb2 │ IV     │ m3-e19   │ 316   │ G2   │ III    │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ F2  │ IV     │ 4-e19    │ 505   │ Ab2  │ III    │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ G2  │ IV     │ 5-e19    │ 695   │ Bb2  │ III    │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ Ab2 │ IV     │ m6-e19   │ 821   │ C3   │ III    │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ Bb2 │ III    │ m3-e19   │ 316   │ D3   │ II     │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ C3  │ III    │ 4-e19    │ 505   │ Eb3  │ II     │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ D3  │ III    │ 5-e19    │ 695   │ F3   │ II     │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ Eb3 │ III    │ m6-e19   │ 821   │ G3   │ II     │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ F3  │ III    │ m7-e19   │ 1011  │ Ab3  │ II     │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ G3  │ II     │ 4-e19    │ 505   │ Bb3  │ I      │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ Ab3 │ II     │ d5-e19   │ 632   │ C4   │ I      │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ D4   │ I      │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ C4  │ II     │ m7-e19   │ 1011  │ Eb4  │ I      │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ D4  │ II     │ 0-1      │ 1200  │ F4   │ I      │ m6-e19   │ 821   │ m3 - 5-19edo │ 316   │
    │ Eb4 │ II     │ m2-e19-1 │ 1326  │ G4   │ I      │ m7-e19   │ 1011  │ M3 - 6-19edo │ 379   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ Ab4  │ I      │ d8-e19   │ 1137  │ m3 - 5-19edo │ 316   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ m3 - 5-19edo │ 316   │
    │ Ab4 │ II     │ d5-e19-1 │ 1832  │ C5   │ I      │ m3-e19-1 │ 1516  │ M3 - 6-19edo │ 379   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ D5   │ I      │ 4-e19-1  │ 1705  │ M3 - 6-19edo │ 379   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ Eb5  │ I      │ d5-e19-1 │ 1832  │ m3 - 5-19edo │ 316   │
    │ D5  │ II     │ 0-2      │ 2400  │ F5   │ I      │ m6-e19-1 │ 2021  │ m3 - 5-19edo │ 316   │
    │ Eb5 │ II     │ m2-e19-2 │ 2526  │ G5   │ I      │ m7-e19-1 │ 2211  │ M3 - 6-19edo │ 379   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ Ab5  │ I      │ d8-e19-1 │ 2337  │ m3 - 5-19edo │ 316   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ m3 - 5-19edo │ 316   │
    │ Ab5 │ II     │ d5-e19-2 │ 3032  │ C6   │ I      │ m3-e19-2 │ 2716  │ M3 - 6-19edo │ 379   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ D6   │ I      │ 4-e19-2  │ 2905  │ M3 - 6-19edo │ 379   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ Eb6  │ I      │ d5-e19-2 │ 3032  │ m3 - 5-19edo │ 316   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "e_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ E2  │ IV     │ M3-e19   │ 379   │ G#2  │ III    │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ F#2 │ IV     │ A4-e19   │ 568   │ A2   │ III    │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ G#2 │ IV     │ A5-e19   │ 758   │ B2   │ III    │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ A2  │ IV     │ M6-e19   │ 884   │ C#3  │ III    │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ B2  │ III    │ M3-e19   │ 379   │ D#3  │ II     │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ C#3 │ III    │ A4-e19   │ 568   │ E3   │ II     │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ D#3 │ III    │ A5-e19   │ 758   │ F#3  │ II     │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ E3  │ III    │ M6-e19   │ 884   │ G#3  │ II     │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ F#3 │ II     │ M3-e19   │ 379   │ A3   │ I      │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ G#3 │ II     │ A4-e19   │ 568   │ B3   │ I      │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ A3  │ II     │ 5-e19    │ 695   │ C#4  │ I      │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ B3  │ II     │ M6-e19   │ 884   │ D#4  │ I      │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ C#4 │ II     │ M7-e19   │ 1074  │ E4   │ I      │ 5-e19    │ 695   │ m3 - 5-19edo │ 316   │
    │ D#4 │ II     │ A1-e19-1 │ 1263  │ F#4  │ I      │ M6-e19   │ 884   │ m3 - 5-19edo │ 316   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G#4  │ I      │ M7-e19   │ 1074  │ M3 - 6-19edo │ 379   │
    │ F#4 │ II     │ M3-e19-1 │ 1579  │ A4   │ I      │ 0-1      │ 1200  │ m3 - 5-19edo │ 316   │
    │ G#4 │ II     │ A4-e19-1 │ 1768  │ B4   │ I      │ M2-e19-1 │ 1389  │ m3 - 5-19edo │ 316   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C#5  │ I      │ M3-e19-1 │ 1579  │ M3 - 6-19edo │ 379   │
    │ B4  │ II     │ M6-e19-1 │ 2084  │ D#5  │ I      │ A4-e19-1 │ 1768  │ M3 - 6-19edo │ 379   │
    │ C#5 │ II     │ M7-e19-1 │ 2274  │ E5   │ I      │ 5-e19-1  │ 1895  │ m3 - 5-19edo │ 316   │
    │ D#5 │ II     │ A1-e19-2 │ 2463  │ F#5  │ I      │ M6-e19-1 │ 2084  │ m3 - 5-19edo │ 316   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G#5  │ I      │ M7-e19-1 │ 2274  │ M3 - 6-19edo │ 379   │
    │ F#5 │ II     │ M3-e19-2 │ 2779  │ A5   │ I      │ 0-2      │ 2400  │ m3 - 5-19edo │ 316   │
    │ G#5 │ II     │ A4-e19-2 │ 2968  │ B5   │ I      │ M2-e19-2 │ 2589  │ m3 - 5-19edo │ 316   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C#6  │ I      │ M3-e19-2 │ 2779  │ M3 - 6-19edo │ 379   │
    │ B5  │ II     │ M6-e19-2 │ 3284  │ D#6  │ I      │ A4-e19-2 │ 2968  │ M3 - 6-19edo │ 379   │
    │ C#6 │ II     │ M7-e19-2 │ 3474  │ E6   │ I      │ 5-e19-2  │ 3095  │ m3 - 5-19edo │ 316   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "f_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ F2  │ IV     │ 4-e19    │ 505   │ A2   │ III    │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ G2  │ IV     │ 5-e19    │ 695   │ Bb2  │ III    │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ A2  │ IV     │ M6-e19   │ 884   │ C3   │ III    │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ Bb2 │ III    │ m3-e19   │ 316   │ D3   │ II     │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ C3  │ III    │ 4-e19    │ 505   │ E3   │ II     │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ D3  │ III    │ 5-e19    │ 695   │ F3   │ II     │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ E3  │ III    │ M6-e19   │ 884   │ G3   │ II     │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ F3  │ II     │ m3-e19   │ 316   │ A3   │ I      │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ G3  │ II     │ 4-e19    │ 505   │ Bb3  │ I      │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ A3  │ II     │ 5-e19    │ 695   │ C4   │ I      │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ D4   │ I      │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ C4  │ II     │ m7-e19   │ 1011  │ E4   │ I      │ 5-e19    │ 695   │ M3 - 6-19edo │ 379   │
    │ D4  │ II     │ 0-1      │ 1200  │ F4   │ I      │ m6-e19   │ 821   │ m3 - 5-19edo │ 316   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G4   │ I      │ m7-e19   │ 1011  │ m3 - 5-19edo │ 316   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ A4   │ I      │ 0-1      │ 1200  │ M3 - 6-19edo │ 379   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ m3 - 5-19edo │ 316   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C5   │ I      │ m3-e19-1 │ 1516  │ m3 - 5-19edo │ 316   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ D5   │ I      │ 4-e19-1  │ 1705  │ M3 - 6-19edo │ 379   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ E5   │ I      │ 5-e19-1  │ 1895  │ M3 - 6-19edo │ 379   │
    │ D5  │ II     │ 0-2      │ 2400  │ F5   │ I      │ m6-e19-1 │ 2021  │ m3 - 5-19edo │ 316   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G5   │ I      │ m7-e19-1 │ 2211  │ m3 - 5-19edo │ 316   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ A5   │ I      │ 0-2      │ 2400  │ M3 - 6-19edo │ 379   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ m3 - 5-19edo │ 316   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C6   │ I      │ m3-e19-2 │ 2716  │ m3 - 5-19edo │ 316   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ D6   │ I      │ 4-e19-2  │ 2905  │ M3 - 6-19edo │ 379   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ E6   │ I      │ 5-e19-2  │ 3095  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_f_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ F#2 │ IV     │ A4-e19   │ 568   │ A#2  │ III    │ A2-e19   │ 253   │ M3 - 6-19edo │ 379   │
    │ G#2 │ IV     │ A5-e19   │ 758   │ B2   │ III    │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ A#2 │ IV     │ d7-e19   │ 947   │ C#3  │ III    │ A4-e19   │ 568   │ m3 - 5-19edo │ 316   │
    │ B2  │ III    │ M3-e19   │ 379   │ D#3  │ II     │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ C#3 │ III    │ A4-e19   │ 568   │ E#3  │ II     │ A2-e19   │ 253   │ M3 - 6-19edo │ 379   │
    │ D#3 │ III    │ A5-e19   │ 758   │ F#3  │ II     │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ E#3 │ III    │ d7-e19   │ 947   │ G#3  │ II     │ A4-e19   │ 568   │ m3 - 5-19edo │ 316   │
    │ F#3 │ II     │ M3-e19   │ 379   │ A#3  │ I      │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ G#3 │ II     │ A4-e19   │ 568   │ B3   │ I      │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ A#3 │ II     │ A5-e19   │ 758   │ C#4  │ I      │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ B3  │ II     │ M6-e19   │ 884   │ D#4  │ I      │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ C#4 │ II     │ M7-e19   │ 1074  │ E#4  │ I      │ A5-e19   │ 758   │ M3 - 6-19edo │ 379   │
    │ D#4 │ II     │ A1-e19-1 │ 1263  │ F#4  │ I      │ M6-e19   │ 884   │ m3 - 5-19edo │ 316   │
    │ E#4 │ II     │ A2-e19-1 │ 1453  │ G#4  │ I      │ M7-e19   │ 1074  │ m3 - 5-19edo │ 316   │
    │ F#4 │ II     │ M3-e19-1 │ 1579  │ A#4  │ I      │ A1-e19-1 │ 1263  │ M3 - 6-19edo │ 379   │
    │ G#4 │ II     │ A4-e19-1 │ 1768  │ B4   │ I      │ M2-e19-1 │ 1389  │ m3 - 5-19edo │ 316   │
    │ A#4 │ II     │ A5-e19-1 │ 1958  │ C#5  │ I      │ M3-e19-1 │ 1579  │ m3 - 5-19edo │ 316   │
    │ B4  │ II     │ M6-e19-1 │ 2084  │ D#5  │ I      │ A4-e19-1 │ 1768  │ M3 - 6-19edo │ 379   │
    │ C#5 │ II     │ M7-e19-1 │ 2274  │ E#5  │ I      │ A5-e19-1 │ 1958  │ M3 - 6-19edo │ 379   │
    │ D#5 │ II     │ A1-e19-2 │ 2463  │ F#5  │ I      │ M6-e19-1 │ 2084  │ m3 - 5-19edo │ 316   │
    │ E#5 │ II     │ A2-e19-2 │ 2653  │ G#5  │ I      │ M7-e19-1 │ 2274  │ m3 - 5-19edo │ 316   │
    │ F#5 │ II     │ M3-e19-2 │ 2779  │ A#5  │ I      │ A1-e19-2 │ 2463  │ M3 - 6-19edo │ 379   │
    │ G#5 │ II     │ A4-e19-2 │ 2968  │ B5   │ I      │ M2-e19-2 │ 2589  │ m3 - 5-19edo │ 316   │
    │ A#5 │ II     │ A5-e19-2 │ 3158  │ C#6  │ I      │ M3-e19-2 │ 2779  │ m3 - 5-19edo │ 316   │
    │ B5  │ II     │ M6-e19-2 │ 3284  │ D#6  │ I      │ A4-e19-2 │ 2968  │ M3 - 6-19edo │ 379   │
    │ C#6 │ II     │ M7-e19-2 │ 3474  │ E#6  │ I      │ A5-e19-2 │ 3158  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_g_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ Gb2 │ IV     │ d5-e19   │ 632   │ Bb2  │ III    │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Ab2 │ IV     │ m6-e19   │ 821   │ Cb3  │ III    │ A3-e19   │ 442   │ m3 - 5-19edo │ 316   │
    │ Bb2 │ IV     │ m7-e19   │ 1011  │ Db3  │ III    │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ Cb3 │ III    │ A3-e19   │ 442   │ Eb3  │ II     │ m2-e19   │ 126   │ M3 - 6-19edo │ 379   │
    │ Db3 │ III    │ d5-e19   │ 632   │ F3   │ II     │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Eb3 │ III    │ m6-e19   │ 821   │ Gb3  │ II     │ A3-e19   │ 442   │ m3 - 5-19edo │ 316   │
    │ F3  │ III    │ m7-e19   │ 1011  │ Ab3  │ II     │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ Gb3 │ II     │ A3-e19   │ 442   │ Bb3  │ I      │ m2-e19   │ 126   │ M3 - 6-19edo │ 379   │
    │ Ab3 │ II     │ d5-e19   │ 632   │ Cb4  │ I      │ A2-e19   │ 253   │ m3 - 5-19edo │ 316   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ Db4  │ I      │ A3-e19   │ 442   │ m3 - 5-19edo │ 316   │
    │ Cb4 │ II     │ d7-e19   │ 947   │ Eb4  │ I      │ d5-e19   │ 632   │ M3 - 6-19edo │ 379   │
    │ Db4 │ II     │ d8-e19   │ 1137  │ F4   │ I      │ m6-e19   │ 821   │ M3 - 6-19edo │ 379   │
    │ Eb4 │ II     │ m2-e19-1 │ 1326  │ Gb4  │ I      │ d7-e19   │ 947   │ m3 - 5-19edo │ 316   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ Ab4  │ I      │ d8-e19   │ 1137  │ m3 - 5-19edo │ 316   │
    │ Gb4 │ II     │ A3-e19-1 │ 1642  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ M3 - 6-19edo │ 379   │
    │ Ab4 │ II     │ d5-e19-1 │ 1832  │ Cb5  │ I      │ A2-e19-1 │ 1453  │ m3 - 5-19edo │ 316   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ Db5  │ I      │ A3-e19-1 │ 1642  │ m3 - 5-19edo │ 316   │
    │ Cb5 │ II     │ d7-e19-1 │ 2147  │ Eb5  │ I      │ d5-e19-1 │ 1832  │ M3 - 6-19edo │ 379   │
    │ Db5 │ II     │ d8-e19-1 │ 2337  │ F5   │ I      │ m6-e19-1 │ 2021  │ M3 - 6-19edo │ 379   │
    │ Eb5 │ II     │ m2-e19-2 │ 2526  │ Gb5  │ I      │ d7-e19-1 │ 2147  │ m3 - 5-19edo │ 316   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ Ab5  │ I      │ d8-e19-1 │ 2337  │ m3 - 5-19edo │ 316   │
    │ Gb5 │ II     │ A3-e19-2 │ 2842  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ M3 - 6-19edo │ 379   │
    │ Ab5 │ II     │ d5-e19-2 │ 3032  │ Cb6  │ I      │ A2-e19-2 │ 2653  │ m3 - 5-19edo │ 316   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ Db6  │ I      │ A3-e19-2 │ 2842  │ m3 - 5-19edo │ 316   │
    │ Cb6 │ II     │ d7-e19-2 │ 3347  │ Eb6  │ I      │ d5-e19-2 │ 3032  │ M3 - 6-19edo │ 379   │
    │ Db6 │ II     │ d8-e19-2 │ 3537  │ F6   │ I      │ m6-e19-2 │ 3221  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "g_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ F2  │ IV     │ 4-e19    │ 505   │ A2   │ III    │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ G2  │ IV     │ 5-e19    │ 695   │ Bb2  │ III    │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ A2  │ IV     │ M6-e19   │ 884   │ C3   │ III    │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ Bb2 │ III    │ m3-e19   │ 316   │ D3   │ II     │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ C3  │ III    │ 4-e19    │ 505   │ E3   │ II     │ M2-e19   │ 189   │ M3 - 6-19edo │ 379   │
    │ D3  │ III    │ 5-e19    │ 695   │ F3   │ II     │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ E3  │ III    │ M6-e19   │ 884   │ G3   │ II     │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ F3  │ II     │ m3-e19   │ 316   │ A3   │ I      │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ G3  │ II     │ 4-e19    │ 505   │ Bb3  │ I      │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ A3  │ II     │ 5-e19    │ 695   │ C4   │ I      │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ D4   │ I      │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ C4  │ II     │ m7-e19   │ 1011  │ E4   │ I      │ 5-e19    │ 695   │ M3 - 6-19edo │ 379   │
    │ D4  │ II     │ 0-1      │ 1200  │ F4   │ I      │ m6-e19   │ 821   │ m3 - 5-19edo │ 316   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G4   │ I      │ m7-e19   │ 1011  │ m3 - 5-19edo │ 316   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ A4   │ I      │ 0-1      │ 1200  │ M3 - 6-19edo │ 379   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ m3 - 5-19edo │ 316   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C5   │ I      │ m3-e19-1 │ 1516  │ m3 - 5-19edo │ 316   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ D5   │ I      │ 4-e19-1  │ 1705  │ M3 - 6-19edo │ 379   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ E5   │ I      │ 5-e19-1  │ 1895  │ M3 - 6-19edo │ 379   │
    │ D5  │ II     │ 0-2      │ 2400  │ F5   │ I      │ m6-e19-1 │ 2021  │ m3 - 5-19edo │ 316   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G5   │ I      │ m7-e19-1 │ 2211  │ m3 - 5-19edo │ 316   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ A5   │ I      │ 0-2      │ 2400  │ M3 - 6-19edo │ 379   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ m3 - 5-19edo │ 316   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C6   │ I      │ m3-e19-2 │ 2716  │ m3 - 5-19edo │ 316   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ D6   │ I      │ 4-e19-2  │ 2905  │ M3 - 6-19edo │ 379   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ E6   │ I      │ 5-e19-2  │ 3095  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_a_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ Ab2 │ IV     │ m6-e19   │ 821   │ C3   │ III    │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ Bb2 │ IV     │ m7-e19   │ 1011  │ Db3  │ III    │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ C3  │ III    │ 4-e19    │ 505   │ Eb3  │ II     │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ Db3 │ III    │ d5-e19   │ 632   │ F3   │ II     │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Eb3 │ III    │ m6-e19   │ 821   │ G3   │ II     │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ F3  │ III    │ m7-e19   │ 1011  │ Ab3  │ II     │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ G3  │ II     │ 4-e19    │ 505   │ Bb3  │ I      │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ Ab3 │ II     │ d5-e19   │ 632   │ C4   │ I      │ m3-e19   │ 316   │ M3 - 6-19edo │ 379   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ Db4  │ I      │ A3-e19   │ 442   │ m3 - 5-19edo │ 316   │
    │ C4  │ II     │ m7-e19   │ 1011  │ Eb4  │ I      │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ Db4 │ II     │ d8-e19   │ 1137  │ F4   │ I      │ m6-e19   │ 821   │ M3 - 6-19edo │ 379   │
    │ Eb4 │ II     │ m2-e19-1 │ 1326  │ G4   │ I      │ m7-e19   │ 1011  │ M3 - 6-19edo │ 379   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ Ab4  │ I      │ d8-e19   │ 1137  │ m3 - 5-19edo │ 316   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ m3 - 5-19edo │ 316   │
    │ Ab4 │ II     │ d5-e19-1 │ 1832  │ C5   │ I      │ m3-e19-1 │ 1516  │ M3 - 6-19edo │ 379   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ Db5  │ I      │ A3-e19-1 │ 1642  │ m3 - 5-19edo │ 316   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ Eb5  │ I      │ d5-e19-1 │ 1832  │ m3 - 5-19edo │ 316   │
    │ Db5 │ II     │ d8-e19-1 │ 2337  │ F5   │ I      │ m6-e19-1 │ 2021  │ M3 - 6-19edo │ 379   │
    │ Eb5 │ II     │ m2-e19-2 │ 2526  │ G5   │ I      │ m7-e19-1 │ 2211  │ M3 - 6-19edo │ 379   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ Ab5  │ I      │ d8-e19-1 │ 2337  │ m3 - 5-19edo │ 316   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ m3 - 5-19edo │ 316   │
    │ Ab5 │ II     │ d5-e19-2 │ 3032  │ C6   │ I      │ m3-e19-2 │ 2716  │ M3 - 6-19edo │ 379   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ Db6  │ I      │ A3-e19-2 │ 2842  │ m3 - 5-19edo │ 316   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ Eb6  │ I      │ d5-e19-2 │ 3032  │ m3 - 5-19edo │ 316   │
    │ Db6 │ II     │ d8-e19-2 │ 3537  │ F6   │ I      │ m6-e19-2 │ 3221  │ M3 - 6-19edo │ 379   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "a_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ A2  │ IV     │ M6-e19   │ 884   │ C#3  │ III    │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ B2  │ III    │ M3-e19   │ 379   │ D3   │ II     │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ C#3 │ III    │ A4-e19   │ 568   │ E3   │ II     │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ D3  │ III    │ 5-e19    │ 695   │ F#3  │ II     │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ E3  │ III    │ M6-e19   │ 884   │ G#3  │ II     │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ F#3 │ II     │ M3-e19   │ 379   │ A3   │ I      │ 0        │ 0     │ m3 - 5-19edo │ 316   │
    │ G#3 │ II     │ A4-e19   │ 568   │ B3   │ I      │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ A3  │ II     │ 5-e19    │ 695   │ C#4  │ I      │ M3-e19   │ 379   │ M3 - 6-19edo │ 379   │
    │ B3  │ II     │ M6-e19   │ 884   │ D4   │ I      │ 4-e19    │ 505   │ m3 - 5-19edo │ 316   │
    │ C#4 │ II     │ M7-e19   │ 1074  │ E4   │ I      │ 5-e19    │ 695   │ m3 - 5-19edo │ 316   │
    │ D4  │ II     │ 0-1      │ 1200  │ F#4  │ I      │ M6-e19   │ 884   │ M3 - 6-19edo │ 379   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G#4  │ I      │ M7-e19   │ 1074  │ M3 - 6-19edo │ 379   │
    │ F#4 │ II     │ M3-e19-1 │ 1579  │ A4   │ I      │ 0-1      │ 1200  │ m3 - 5-19edo │ 316   │
    │ G#4 │ II     │ A4-e19-1 │ 1768  │ B4   │ I      │ M2-e19-1 │ 1389  │ m3 - 5-19edo │ 316   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C#5  │ I      │ M3-e19-1 │ 1579  │ M3 - 6-19edo │ 379   │
    │ B4  │ II     │ M6-e19-1 │ 2084  │ D5   │ I      │ 4-e19-1  │ 1705  │ m3 - 5-19edo │ 316   │
    │ C#5 │ II     │ M7-e19-1 │ 2274  │ E5   │ I      │ 5-e19-1  │ 1895  │ m3 - 5-19edo │ 316   │
    │ D5  │ II     │ 0-2      │ 2400  │ F#5  │ I      │ M6-e19-1 │ 2084  │ M3 - 6-19edo │ 379   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G#5  │ I      │ M7-e19-1 │ 2274  │ M3 - 6-19edo │ 379   │
    │ F#5 │ II     │ M3-e19-2 │ 2779  │ A5   │ I      │ 0-2      │ 2400  │ m3 - 5-19edo │ 316   │
    │ G#5 │ II     │ A4-e19-2 │ 2968  │ B5   │ I      │ M2-e19-2 │ 2589  │ m3 - 5-19edo │ 316   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C#6  │ I      │ M3-e19-2 │ 2779  │ M3 - 6-19edo │ 379   │
    │ B5  │ II     │ M6-e19-2 │ 3284  │ D6   │ I      │ 4-e19-2  │ 2905  │ m3 - 5-19edo │ 316   │
    │ C#6 │ II     │ M7-e19-2 │ 3474  │ E6   │ I      │ 5-e19-2  │ 3095  │ m3 - 5-19edo │ 316   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_b_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ Bb2 │ III    │ m3-e19   │ 316   │ D3   │ II     │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ C3  │ III    │ 4-e19    │ 505   │ Eb3  │ II     │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ D3  │ III    │ 5-e19    │ 695   │ F3   │ II     │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ Eb3 │ III    │ m6-e19   │ 821   │ G3   │ II     │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ F3  │ II     │ m3-e19   │ 316   │ A3   │ I      │ 0        │ 0     │ M3 - 6-19edo │ 379   │
    │ G3  │ II     │ 4-e19    │ 505   │ Bb3  │ I      │ m2-e19   │ 126   │ m3 - 5-19edo │ 316   │
    │ A3  │ II     │ 5-e19    │ 695   │ C4   │ I      │ m3-e19   │ 316   │ m3 - 5-19edo │ 316   │
    │ Bb3 │ II     │ m6-e19   │ 821   │ D4   │ I      │ 4-e19    │ 505   │ M3 - 6-19edo │ 379   │
    │ C4  │ II     │ m7-e19   │ 1011  │ Eb4  │ I      │ d5-e19   │ 632   │ m3 - 5-19edo │ 316   │
    │ D4  │ II     │ 0-1      │ 1200  │ F4   │ I      │ m6-e19   │ 821   │ m3 - 5-19edo │ 316   │
    │ Eb4 │ II     │ m2-e19-1 │ 1326  │ G4   │ I      │ m7-e19   │ 1011  │ M3 - 6-19edo │ 379   │
    │ F4  │ II     │ m3-e19-1 │ 1516  │ A4   │ I      │ 0-1      │ 1200  │ M3 - 6-19edo │ 379   │
    │ G4  │ II     │ 4-e19-1  │ 1705  │ Bb4  │ I      │ m2-e19-1 │ 1326  │ m3 - 5-19edo │ 316   │
    │ A4  │ II     │ 5-e19-1  │ 1895  │ C5   │ I      │ m3-e19-1 │ 1516  │ m3 - 5-19edo │ 316   │
    │ Bb4 │ II     │ m6-e19-1 │ 2021  │ D5   │ I      │ 4-e19-1  │ 1705  │ M3 - 6-19edo │ 379   │
    │ C5  │ II     │ m7-e19-1 │ 2211  │ Eb5  │ I      │ d5-e19-1 │ 1832  │ m3 - 5-19edo │ 316   │
    │ D5  │ II     │ 0-2      │ 2400  │ F5   │ I      │ m6-e19-1 │ 2021  │ m3 - 5-19edo │ 316   │
    │ Eb5 │ II     │ m2-e19-2 │ 2526  │ G5   │ I      │ m7-e19-1 │ 2211  │ M3 - 6-19edo │ 379   │
    │ F5  │ II     │ m3-e19-2 │ 2716  │ A5   │ I      │ 0-2      │ 2400  │ M3 - 6-19edo │ 379   │
    │ G5  │ II     │ 4-e19-2  │ 2905  │ Bb5  │ I      │ m2-e19-2 │ 2526  │ m3 - 5-19edo │ 316   │
    │ A5  │ II     │ 5-e19-2  │ 3095  │ C6   │ I      │ m3-e19-2 │ 2716  │ m3 - 5-19edo │ 316   │
    │ Bb5 │ II     │ m6-e19-2 │ 3221  │ D6   │ I      │ 4-e19-2  │ 2905  │ M3 - 6-19edo │ 379   │
    │ C6  │ II     │ m7-e19-2 │ 3411  │ Eb6  │ I      │ d5-e19-2 │ 3032  │ m3 - 5-19edo │ 316   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "b_major" =
  let t = force E19.t in
  let scale = make_major_scale ~from:Scales.lower_b in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval     │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼──────────────┼───────┤
    │ B2  │ III    │ M3-e19   │ 379   │ D#3  │ II     │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ C#3 │ III    │ A4-e19   │ 568   │ E3   │ II     │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ D#3 │ III    │ A5-e19   │ 758   │ F#3  │ II     │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ E3  │ III    │ M6-e19   │ 884   │ G#3  │ II     │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ F#3 │ II     │ M3-e19   │ 379   │ A#3  │ I      │ A1-e19   │ 63    │ M3 - 6-19edo │ 379   │
    │ G#3 │ II     │ A4-e19   │ 568   │ B3   │ I      │ M2-e19   │ 189   │ m3 - 5-19edo │ 316   │
    │ A#3 │ II     │ A5-e19   │ 758   │ C#4  │ I      │ M3-e19   │ 379   │ m3 - 5-19edo │ 316   │
    │ B3  │ II     │ M6-e19   │ 884   │ D#4  │ I      │ A4-e19   │ 568   │ M3 - 6-19edo │ 379   │
    │ C#4 │ II     │ M7-e19   │ 1074  │ E4   │ I      │ 5-e19    │ 695   │ m3 - 5-19edo │ 316   │
    │ D#4 │ II     │ A1-e19-1 │ 1263  │ F#4  │ I      │ M6-e19   │ 884   │ m3 - 5-19edo │ 316   │
    │ E4  │ II     │ M2-e19-1 │ 1389  │ G#4  │ I      │ M7-e19   │ 1074  │ M3 - 6-19edo │ 379   │
    │ F#4 │ II     │ M3-e19-1 │ 1579  │ A#4  │ I      │ A1-e19-1 │ 1263  │ M3 - 6-19edo │ 379   │
    │ G#4 │ II     │ A4-e19-1 │ 1768  │ B4   │ I      │ M2-e19-1 │ 1389  │ m3 - 5-19edo │ 316   │
    │ A#4 │ II     │ A5-e19-1 │ 1958  │ C#5  │ I      │ M3-e19-1 │ 1579  │ m3 - 5-19edo │ 316   │
    │ B4  │ II     │ M6-e19-1 │ 2084  │ D#5  │ I      │ A4-e19-1 │ 1768  │ M3 - 6-19edo │ 379   │
    │ C#5 │ II     │ M7-e19-1 │ 2274  │ E5   │ I      │ 5-e19-1  │ 1895  │ m3 - 5-19edo │ 316   │
    │ D#5 │ II     │ A1-e19-2 │ 2463  │ F#5  │ I      │ M6-e19-1 │ 2084  │ m3 - 5-19edo │ 316   │
    │ E5  │ II     │ M2-e19-2 │ 2589  │ G#5  │ I      │ M7-e19-1 │ 2274  │ M3 - 6-19edo │ 379   │
    │ F#5 │ II     │ M3-e19-2 │ 2779  │ A#5  │ I      │ A1-e19-2 │ 2463  │ M3 - 6-19edo │ 379   │
    │ G#5 │ II     │ A4-e19-2 │ 2968  │ B5   │ I      │ M2-e19-2 │ 2589  │ m3 - 5-19edo │ 316   │
    │ A#5 │ II     │ A5-e19-2 │ 3158  │ C#6  │ I      │ M3-e19-2 │ 2779  │ m3 - 5-19edo │ 316   │
    │ B5  │ II     │ M6-e19-2 │ 3284  │ D#6  │ I      │ A4-e19-2 │ 2968  │ M3 - 6-19edo │ 379   │
    │ C#6 │ II     │ M7-e19-2 │ 3474  │ E6   │ I      │ 5-e19-2  │ 3095  │ m3 - 5-19edo │ 316   │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴──────────────┴───────┘ |}]
;;
