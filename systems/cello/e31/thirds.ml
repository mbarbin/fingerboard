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
  let t = force E31.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Third
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_e31 ~from
;;

let%expect_test "c_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3-e31   │   387 │ G2   │ III    │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ F2  │ IV     │ 4-e31    │   503 │ A2   │ III    │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ G2  │ IV     │ 5-e31    │   697 │ B2   │ III    │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ A2  │ IV     │ M6-e31   │   890 │ C3   │ III    │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ B2  │ III    │ M3-e31   │   387 │ D3   │ II     │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ C3  │ III    │ 4-e31    │   503 │ E3   │ II     │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ D3  │ III    │ 5-e31    │   697 │ F3   │ II     │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ E3  │ III    │ M6-e31   │   890 │ G3   │ II     │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ F3  │ II     │ m3-e31   │   310 │ A3   │ I      │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ G3  │ II     │ 4-e31    │   503 │ B3   │ I      │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ A3  │ II     │ 5-e31    │   697 │ C4   │ I      │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ B3  │ II     │ M6-e31   │   890 │ D4   │ I      │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ C4  │ II     │ m7-e31   │  1006 │ E4   │ I      │ 5-e31    │   697 │ M3 - 10-31edo │   387 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e31   │   813 │ m3 - 8-31edo  │   310 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G4   │ I      │ m7-e31   │  1006 │ m3 - 8-31edo  │   310 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ A4   │ I      │ 0-1      │  1200 │ M3 - 10-31edo │   387 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ B4   │ I      │ M2-e31-1 │  1394 │ M3 - 10-31edo │   387 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C5   │ I      │ m3-e31-1 │  1510 │ m3 - 8-31edo  │   310 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ D5   │ I      │ 4-e31-1  │  1703 │ m3 - 8-31edo  │   310 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ E5   │ I      │ 5-e31-1  │  1897 │ M3 - 10-31edo │   387 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e31-1 │  2013 │ m3 - 8-31edo  │   310 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G5   │ I      │ m7-e31-1 │  2206 │ m3 - 8-31edo  │   310 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ A5   │ I      │ 0-2      │  2400 │ M3 - 10-31edo │   387 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ B5   │ I      │ M2-e31-2 │  2594 │ M3 - 10-31edo │   387 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C6   │ I      │ m3-e31-2 │  2710 │ m3 - 8-31edo  │   310 │
    │ B5  │ II     │ M6-e31-2 │  3290 │ D6   │ I      │ 4-e31-2  │  2903 │ m3 - 8-31edo  │   310 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ E6   │ I      │ 5-e31-2  │  3097 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_c_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E#2 │ IV     │ A3-e31   │   465 │ G#2  │ III    │ A1-e31   │    77 │ m3 - 8-31edo  │   310 │
    │ F#2 │ IV     │ A4-e31   │   581 │ A#2  │ III    │ A2-e31   │   271 │ M3 - 10-31edo │   387 │
    │ G#2 │ IV     │ A5-e31   │   774 │ B#2  │ III    │ A3-e31   │   465 │ M3 - 10-31edo │   387 │
    │ B#2 │ III    │ A3-e31   │   465 │ D#3  │ II     │ A1-e31   │    77 │ m3 - 8-31edo  │   310 │
    │ C#3 │ III    │ A4-e31   │   581 │ E#3  │ II     │ A2-e31   │   271 │ M3 - 10-31edo │   387 │
    │ D#3 │ III    │ A5-e31   │   774 │ F#3  │ II     │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ F#3 │ II     │ M3-e31   │   387 │ A#3  │ I      │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ G#3 │ II     │ A4-e31   │   581 │ B#3  │ I      │ A2-e31   │   271 │ M3 - 10-31edo │   387 │
    │ A#3 │ II     │ A5-e31   │   774 │ C#4  │ I      │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ C#4 │ II     │ M7-e31   │  1084 │ E#4  │ I      │ A5-e31   │   774 │ M3 - 10-31edo │   387 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ F#4  │ I      │ M6-e31   │   890 │ m3 - 8-31edo  │   310 │
    │ E#4 │ II     │ A2-e31-1 │  1471 │ G#4  │ I      │ M7-e31   │  1084 │ m3 - 8-31edo  │   310 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ A#4  │ I      │ A1-e31-1 │  1277 │ M3 - 10-31edo │   387 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ B#4  │ I      │ A2-e31-1 │  1471 │ M3 - 10-31edo │   387 │
    │ A#4 │ II     │ A5-e31-1 │  1974 │ C#5  │ I      │ M3-e31-1 │  1587 │ m3 - 8-31edo  │   310 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ E#5  │ I      │ A5-e31-1 │  1974 │ M3 - 10-31edo │   387 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ F#5  │ I      │ M6-e31-1 │  2090 │ m3 - 8-31edo  │   310 │
    │ E#5 │ II     │ A2-e31-2 │  2671 │ G#5  │ I      │ M7-e31-1 │  2284 │ m3 - 8-31edo  │   310 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ A#5  │ I      │ A1-e31-2 │  2477 │ M3 - 10-31edo │   387 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ B#5  │ I      │ A2-e31-2 │  2671 │ M3 - 10-31edo │   387 │
    │ A#5 │ II     │ A5-e31-2 │  3174 │ C#6  │ I      │ M3-e31-2 │  2787 │ m3 - 8-31edo  │   310 │
    │ C#6 │ II     │ M7-e31-2 │  3484 │ E#6  │ I      │ A5-e31-2 │  3174 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_d_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e31    │   503 │ Ab2  │ III    │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ Gb2 │ IV     │ d5-e31   │   619 │ Bb2  │ III    │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Ab2 │ IV     │ m6-e31   │   813 │ C3   │ III    │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ Bb2 │ IV     │ m7-e31   │  1006 │ Db3  │ III    │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ C3  │ III    │ 4-e31    │   503 │ Eb3  │ II     │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ Db3 │ III    │ d5-e31   │   619 │ F3   │ II     │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Eb3 │ III    │ m6-e31   │   813 │ Gb3  │ II     │ d4-e31   │   426 │ m3 - 8-31edo  │   310 │
    │ F3  │ III    │ m7-e31   │  1006 │ Ab3  │ II     │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ Gb3 │ II     │ d4-e31   │   426 │ Bb3  │ I      │ m2-e31   │   116 │ M3 - 10-31edo │   387 │
    │ Ab3 │ II     │ d5-e31   │   619 │ C4   │ I      │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Bb3 │ II     │ m6-e31   │   813 │ Db4  │ I      │ d4-e31   │   426 │ m3 - 8-31edo  │   310 │
    │ C4  │ II     │ m7-e31   │  1006 │ Eb4  │ I      │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ Db4 │ II     │ d8-e31   │  1123 │ F4   │ I      │ m6-e31   │   813 │ M3 - 10-31edo │   387 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ Gb4  │ I      │ d7-e31   │   929 │ m3 - 8-31edo  │   310 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Ab4  │ I      │ d8-e31   │  1123 │ m3 - 8-31edo  │   310 │
    │ Gb4 │ II     │ d4-e31-1 │  1626 │ Bb4  │ I      │ m2-e31-1 │  1316 │ M3 - 10-31edo │   387 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ C5   │ I      │ m3-e31-1 │  1510 │ M3 - 10-31edo │   387 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ Db5  │ I      │ d4-e31-1 │  1626 │ m3 - 8-31edo  │   310 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m3 - 8-31edo  │   310 │
    │ Db5 │ II     │ d8-e31-1 │  2323 │ F5   │ I      │ m6-e31-1 │  2013 │ M3 - 10-31edo │   387 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ Gb5  │ I      │ d7-e31-1 │  2129 │ m3 - 8-31edo  │   310 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m3 - 8-31edo  │   310 │
    │ Gb5 │ II     │ d4-e31-2 │  2826 │ Bb5  │ I      │ m2-e31-2 │  2516 │ M3 - 10-31edo │   387 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ C6   │ I      │ m3-e31-2 │  2710 │ M3 - 10-31edo │   387 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ Db6  │ I      │ d4-e31-2 │  2826 │ m3 - 8-31edo  │   310 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m3 - 8-31edo  │   310 │
    │ Db6 │ II     │ d8-e31-2 │  3523 │ F6   │ I      │ m6-e31-2 │  3213 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3-e31   │   387 │ G2   │ III    │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ F#2 │ IV     │ A4-e31   │   581 │ A2   │ III    │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ G2  │ IV     │ 5-e31    │   697 │ B2   │ III    │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ A2  │ IV     │ M6-e31   │   890 │ C#3  │ III    │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ B2  │ III    │ M3-e31   │   387 │ D3   │ II     │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ C#3 │ III    │ A4-e31   │   581 │ E3   │ II     │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ D3  │ III    │ 5-e31    │   697 │ F#3  │ II     │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ E3  │ III    │ M6-e31   │   890 │ G3   │ II     │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ F#3 │ II     │ M3-e31   │   387 │ A3   │ I      │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ G3  │ II     │ 4-e31    │   503 │ B3   │ I      │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ A3  │ II     │ 5-e31    │   697 │ C#4  │ I      │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ B3  │ II     │ M6-e31   │   890 │ D4   │ I      │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ C#4 │ II     │ M7-e31   │  1084 │ E4   │ I      │ 5-e31    │   697 │ m3 - 8-31edo  │   310 │
    │ D4  │ II     │ 0-1      │  1200 │ F#4  │ I      │ M6-e31   │   890 │ M3 - 10-31edo │   387 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G4   │ I      │ m7-e31   │  1006 │ m3 - 8-31edo  │   310 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ A4   │ I      │ 0-1      │  1200 │ m3 - 8-31edo  │   310 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ B4   │ I      │ M2-e31-1 │  1394 │ M3 - 10-31edo │   387 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C#5  │ I      │ M3-e31-1 │  1587 │ M3 - 10-31edo │   387 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ D5   │ I      │ 4-e31-1  │  1703 │ m3 - 8-31edo  │   310 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ E5   │ I      │ 5-e31-1  │  1897 │ m3 - 8-31edo  │   310 │
    │ D5  │ II     │ 0-2      │  2400 │ F#5  │ I      │ M6-e31-1 │  2090 │ M3 - 10-31edo │   387 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G5   │ I      │ m7-e31-1 │  2206 │ m3 - 8-31edo  │   310 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ A5   │ I      │ 0-2      │  2400 │ m3 - 8-31edo  │   310 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ B5   │ I      │ M2-e31-2 │  2594 │ M3 - 10-31edo │   387 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C#6  │ I      │ M3-e31-2 │  2787 │ M3 - 10-31edo │   387 │
    │ B5  │ II     │ M6-e31-2 │  3290 │ D6   │ I      │ 4-e31-2  │  2903 │ m3 - 8-31edo  │   310 │
    │ C#6 │ II     │ M7-e31-2 │  3484 │ E6   │ I      │ 5-e31-2  │  3097 │ m3 - 8-31edo  │   310 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_e_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3-e31   │   310 │ G2   │ III    │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ F2  │ IV     │ 4-e31    │   503 │ Ab2  │ III    │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ G2  │ IV     │ 5-e31    │   697 │ Bb2  │ III    │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ Ab2 │ IV     │ m6-e31   │   813 │ C3   │ III    │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ Bb2 │ III    │ m3-e31   │   310 │ D3   │ II     │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ C3  │ III    │ 4-e31    │   503 │ Eb3  │ II     │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ D3  │ III    │ 5-e31    │   697 │ F3   │ II     │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ Eb3 │ III    │ m6-e31   │   813 │ G3   │ II     │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ F3  │ III    │ m7-e31   │  1006 │ Ab3  │ II     │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ G3  │ II     │ 4-e31    │   503 │ Bb3  │ I      │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ Ab3 │ II     │ d5-e31   │   619 │ C4   │ I      │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Bb3 │ II     │ m6-e31   │   813 │ D4   │ I      │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ C4  │ II     │ m7-e31   │  1006 │ Eb4  │ I      │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e31   │   813 │ m3 - 8-31edo  │   310 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ G4   │ I      │ m7-e31   │  1006 │ M3 - 10-31edo │   387 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Ab4  │ I      │ d8-e31   │  1123 │ m3 - 8-31edo  │   310 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m3 - 8-31edo  │   310 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ C5   │ I      │ m3-e31-1 │  1510 │ M3 - 10-31edo │   387 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ D5   │ I      │ 4-e31-1  │  1703 │ M3 - 10-31edo │   387 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m3 - 8-31edo  │   310 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e31-1 │  2013 │ m3 - 8-31edo  │   310 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ G5   │ I      │ m7-e31-1 │  2206 │ M3 - 10-31edo │   387 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m3 - 8-31edo  │   310 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m3 - 8-31edo  │   310 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ C6   │ I      │ m3-e31-2 │  2710 │ M3 - 10-31edo │   387 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ D6   │ I      │ 4-e31-2  │  2903 │ M3 - 10-31edo │   387 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m3 - 8-31edo  │   310 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3-e31   │   387 │ G#2  │ III    │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ F#2 │ IV     │ A4-e31   │   581 │ A2   │ III    │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ G#2 │ IV     │ A5-e31   │   774 │ B2   │ III    │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ A2  │ IV     │ M6-e31   │   890 │ C#3  │ III    │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ B2  │ III    │ M3-e31   │   387 │ D#3  │ II     │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ C#3 │ III    │ A4-e31   │   581 │ E3   │ II     │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ D#3 │ III    │ A5-e31   │   774 │ F#3  │ II     │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ E3  │ III    │ M6-e31   │   890 │ G#3  │ II     │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ F#3 │ II     │ M3-e31   │   387 │ A3   │ I      │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ G#3 │ II     │ A4-e31   │   581 │ B3   │ I      │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ A3  │ II     │ 5-e31    │   697 │ C#4  │ I      │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ B3  │ II     │ M6-e31   │   890 │ D#4  │ I      │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ C#4 │ II     │ M7-e31   │  1084 │ E4   │ I      │ 5-e31    │   697 │ m3 - 8-31edo  │   310 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ F#4  │ I      │ M6-e31   │   890 │ m3 - 8-31edo  │   310 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G#4  │ I      │ M7-e31   │  1084 │ M3 - 10-31edo │   387 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ A4   │ I      │ 0-1      │  1200 │ m3 - 8-31edo  │   310 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ B4   │ I      │ M2-e31-1 │  1394 │ m3 - 8-31edo  │   310 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C#5  │ I      │ M3-e31-1 │  1587 │ M3 - 10-31edo │   387 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ D#5  │ I      │ A4-e31-1 │  1781 │ M3 - 10-31edo │   387 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ E5   │ I      │ 5-e31-1  │  1897 │ m3 - 8-31edo  │   310 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ F#5  │ I      │ M6-e31-1 │  2090 │ m3 - 8-31edo  │   310 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G#5  │ I      │ M7-e31-1 │  2284 │ M3 - 10-31edo │   387 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ A5   │ I      │ 0-2      │  2400 │ m3 - 8-31edo  │   310 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ B5   │ I      │ M2-e31-2 │  2594 │ m3 - 8-31edo  │   310 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C#6  │ I      │ M3-e31-2 │  2787 │ M3 - 10-31edo │   387 │
    │ B5  │ II     │ M6-e31-2 │  3290 │ D#6  │ I      │ A4-e31-2 │  2981 │ M3 - 10-31edo │   387 │
    │ C#6 │ II     │ M7-e31-2 │  3484 │ E6   │ I      │ 5-e31-2  │  3097 │ m3 - 8-31edo  │   310 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e31    │   503 │ A2   │ III    │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ G2  │ IV     │ 5-e31    │   697 │ Bb2  │ III    │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ A2  │ IV     │ M6-e31   │   890 │ C3   │ III    │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ Bb2 │ III    │ m3-e31   │   310 │ D3   │ II     │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ C3  │ III    │ 4-e31    │   503 │ E3   │ II     │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ D3  │ III    │ 5-e31    │   697 │ F3   │ II     │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ E3  │ III    │ M6-e31   │   890 │ G3   │ II     │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ F3  │ II     │ m3-e31   │   310 │ A3   │ I      │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ G3  │ II     │ 4-e31    │   503 │ Bb3  │ I      │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ A3  │ II     │ 5-e31    │   697 │ C4   │ I      │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ Bb3 │ II     │ m6-e31   │   813 │ D4   │ I      │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ C4  │ II     │ m7-e31   │  1006 │ E4   │ I      │ 5-e31    │   697 │ M3 - 10-31edo │   387 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e31   │   813 │ m3 - 8-31edo  │   310 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G4   │ I      │ m7-e31   │  1006 │ m3 - 8-31edo  │   310 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ A4   │ I      │ 0-1      │  1200 │ M3 - 10-31edo │   387 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m3 - 8-31edo  │   310 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C5   │ I      │ m3-e31-1 │  1510 │ m3 - 8-31edo  │   310 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ D5   │ I      │ 4-e31-1  │  1703 │ M3 - 10-31edo │   387 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ E5   │ I      │ 5-e31-1  │  1897 │ M3 - 10-31edo │   387 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e31-1 │  2013 │ m3 - 8-31edo  │   310 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G5   │ I      │ m7-e31-1 │  2206 │ m3 - 8-31edo  │   310 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ A5   │ I      │ 0-2      │  2400 │ M3 - 10-31edo │   387 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m3 - 8-31edo  │   310 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C6   │ I      │ m3-e31-2 │  2710 │ m3 - 8-31edo  │   310 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ D6   │ I      │ 4-e31-2  │  2903 │ M3 - 10-31edo │   387 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ E6   │ I      │ 5-e31-2  │  3097 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_f_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ A4-e31   │   581 │ A#2  │ III    │ A2-e31   │   271 │ M3 - 10-31edo │   387 │
    │ G#2 │ IV     │ A5-e31   │   774 │ B2   │ III    │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ B2  │ III    │ M3-e31   │   387 │ D#3  │ II     │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ C#3 │ III    │ A4-e31   │   581 │ E#3  │ II     │ A2-e31   │   271 │ M3 - 10-31edo │   387 │
    │ D#3 │ III    │ A5-e31   │   774 │ F#3  │ II     │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ F#3 │ II     │ M3-e31   │   387 │ A#3  │ I      │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ G#3 │ II     │ A4-e31   │   581 │ B3   │ I      │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ A#3 │ II     │ A5-e31   │   774 │ C#4  │ I      │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ B3  │ II     │ M6-e31   │   890 │ D#4  │ I      │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ C#4 │ II     │ M7-e31   │  1084 │ E#4  │ I      │ A5-e31   │   774 │ M3 - 10-31edo │   387 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ F#4  │ I      │ M6-e31   │   890 │ m3 - 8-31edo  │   310 │
    │ E#4 │ II     │ A2-e31-1 │  1471 │ G#4  │ I      │ M7-e31   │  1084 │ m3 - 8-31edo  │   310 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ A#4  │ I      │ A1-e31-1 │  1277 │ M3 - 10-31edo │   387 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ B4   │ I      │ M2-e31-1 │  1394 │ m3 - 8-31edo  │   310 │
    │ A#4 │ II     │ A5-e31-1 │  1974 │ C#5  │ I      │ M3-e31-1 │  1587 │ m3 - 8-31edo  │   310 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ D#5  │ I      │ A4-e31-1 │  1781 │ M3 - 10-31edo │   387 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ E#5  │ I      │ A5-e31-1 │  1974 │ M3 - 10-31edo │   387 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ F#5  │ I      │ M6-e31-1 │  2090 │ m3 - 8-31edo  │   310 │
    │ E#5 │ II     │ A2-e31-2 │  2671 │ G#5  │ I      │ M7-e31-1 │  2284 │ m3 - 8-31edo  │   310 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ A#5  │ I      │ A1-e31-2 │  2477 │ M3 - 10-31edo │   387 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ B5   │ I      │ M2-e31-2 │  2594 │ m3 - 8-31edo  │   310 │
    │ A#5 │ II     │ A5-e31-2 │  3174 │ C#6  │ I      │ M3-e31-2 │  2787 │ m3 - 8-31edo  │   310 │
    │ B5  │ II     │ M6-e31-2 │  3290 │ D#6  │ I      │ A4-e31-2 │  2981 │ M3 - 10-31edo │   387 │
    │ C#6 │ II     │ M7-e31-2 │  3484 │ E#6  │ I      │ A5-e31-2 │  3174 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_g_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ d5-e31   │   619 │ Bb2  │ III    │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Ab2 │ IV     │ m6-e31   │   813 │ Cb3  │ III    │ d4-e31   │   426 │ m3 - 8-31edo  │   310 │
    │ Bb2 │ IV     │ m7-e31   │  1006 │ Db3  │ III    │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ Cb3 │ III    │ d4-e31   │   426 │ Eb3  │ II     │ m2-e31   │   116 │ M3 - 10-31edo │   387 │
    │ Db3 │ III    │ d5-e31   │   619 │ F3   │ II     │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Eb3 │ III    │ m6-e31   │   813 │ Gb3  │ II     │ d4-e31   │   426 │ m3 - 8-31edo  │   310 │
    │ F3  │ III    │ m7-e31   │  1006 │ Ab3  │ II     │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ Gb3 │ II     │ d4-e31   │   426 │ Bb3  │ I      │ m2-e31   │   116 │ M3 - 10-31edo │   387 │
    │ Ab3 │ II     │ d5-e31   │   619 │ Cb4  │ I      │ d3-e31   │   232 │ m3 - 8-31edo  │   310 │
    │ Bb3 │ II     │ m6-e31   │   813 │ Db4  │ I      │ d4-e31   │   426 │ m3 - 8-31edo  │   310 │
    │ Cb4 │ II     │ d7-e31   │   929 │ Eb4  │ I      │ d5-e31   │   619 │ M3 - 10-31edo │   387 │
    │ Db4 │ II     │ d8-e31   │  1123 │ F4   │ I      │ m6-e31   │   813 │ M3 - 10-31edo │   387 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ Gb4  │ I      │ d7-e31   │   929 │ m3 - 8-31edo  │   310 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Ab4  │ I      │ d8-e31   │  1123 │ m3 - 8-31edo  │   310 │
    │ Gb4 │ II     │ d4-e31-1 │  1626 │ Bb4  │ I      │ m2-e31-1 │  1316 │ M3 - 10-31edo │   387 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ Cb5  │ I      │ d3-e31-1 │  1432 │ m3 - 8-31edo  │   310 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ Db5  │ I      │ d4-e31-1 │  1626 │ m3 - 8-31edo  │   310 │
    │ Cb5 │ II     │ d7-e31-1 │  2129 │ Eb5  │ I      │ d5-e31-1 │  1819 │ M3 - 10-31edo │   387 │
    │ Db5 │ II     │ d8-e31-1 │  2323 │ F5   │ I      │ m6-e31-1 │  2013 │ M3 - 10-31edo │   387 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ Gb5  │ I      │ d7-e31-1 │  2129 │ m3 - 8-31edo  │   310 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m3 - 8-31edo  │   310 │
    │ Gb5 │ II     │ d4-e31-2 │  2826 │ Bb5  │ I      │ m2-e31-2 │  2516 │ M3 - 10-31edo │   387 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ Cb6  │ I      │ d3-e31-2 │  2632 │ m3 - 8-31edo  │   310 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ Db6  │ I      │ d4-e31-2 │  2826 │ m3 - 8-31edo  │   310 │
    │ Cb6 │ II     │ d7-e31-2 │  3329 │ Eb6  │ I      │ d5-e31-2 │  3019 │ M3 - 10-31edo │   387 │
    │ Db6 │ II     │ d8-e31-2 │  3523 │ F6   │ I      │ m6-e31-2 │  3213 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e31    │   503 │ A2   │ III    │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ G2  │ IV     │ 5-e31    │   697 │ Bb2  │ III    │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ A2  │ IV     │ M6-e31   │   890 │ C3   │ III    │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ Bb2 │ III    │ m3-e31   │   310 │ D3   │ II     │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ C3  │ III    │ 4-e31    │   503 │ E3   │ II     │ M2-e31   │   194 │ M3 - 10-31edo │   387 │
    │ D3  │ III    │ 5-e31    │   697 │ F3   │ II     │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ E3  │ III    │ M6-e31   │   890 │ G3   │ II     │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ F3  │ II     │ m3-e31   │   310 │ A3   │ I      │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ G3  │ II     │ 4-e31    │   503 │ Bb3  │ I      │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ A3  │ II     │ 5-e31    │   697 │ C4   │ I      │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ Bb3 │ II     │ m6-e31   │   813 │ D4   │ I      │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ C4  │ II     │ m7-e31   │  1006 │ E4   │ I      │ 5-e31    │   697 │ M3 - 10-31edo │   387 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e31   │   813 │ m3 - 8-31edo  │   310 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G4   │ I      │ m7-e31   │  1006 │ m3 - 8-31edo  │   310 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ A4   │ I      │ 0-1      │  1200 │ M3 - 10-31edo │   387 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m3 - 8-31edo  │   310 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C5   │ I      │ m3-e31-1 │  1510 │ m3 - 8-31edo  │   310 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ D5   │ I      │ 4-e31-1  │  1703 │ M3 - 10-31edo │   387 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ E5   │ I      │ 5-e31-1  │  1897 │ M3 - 10-31edo │   387 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e31-1 │  2013 │ m3 - 8-31edo  │   310 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G5   │ I      │ m7-e31-1 │  2206 │ m3 - 8-31edo  │   310 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ A5   │ I      │ 0-2      │  2400 │ M3 - 10-31edo │   387 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m3 - 8-31edo  │   310 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C6   │ I      │ m3-e31-2 │  2710 │ m3 - 8-31edo  │   310 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ D6   │ I      │ 4-e31-2  │  2903 │ M3 - 10-31edo │   387 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ E6   │ I      │ 5-e31-2  │  3097 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_a_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Ab2 │ IV     │ m6-e31   │   813 │ C3   │ III    │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ Bb2 │ IV     │ m7-e31   │  1006 │ Db3  │ III    │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ C3  │ III    │ 4-e31    │   503 │ Eb3  │ II     │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ Db3 │ III    │ d5-e31   │   619 │ F3   │ II     │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Eb3 │ III    │ m6-e31   │   813 │ G3   │ II     │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ F3  │ III    │ m7-e31   │  1006 │ Ab3  │ II     │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ G3  │ II     │ 4-e31    │   503 │ Bb3  │ I      │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ Ab3 │ II     │ d5-e31   │   619 │ C4   │ I      │ m3-e31   │   310 │ M3 - 10-31edo │   387 │
    │ Bb3 │ II     │ m6-e31   │   813 │ Db4  │ I      │ d4-e31   │   426 │ m3 - 8-31edo  │   310 │
    │ C4  │ II     │ m7-e31   │  1006 │ Eb4  │ I      │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ Db4 │ II     │ d8-e31   │  1123 │ F4   │ I      │ m6-e31   │   813 │ M3 - 10-31edo │   387 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ G4   │ I      │ m7-e31   │  1006 │ M3 - 10-31edo │   387 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Ab4  │ I      │ d8-e31   │  1123 │ m3 - 8-31edo  │   310 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m3 - 8-31edo  │   310 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ C5   │ I      │ m3-e31-1 │  1510 │ M3 - 10-31edo │   387 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ Db5  │ I      │ d4-e31-1 │  1626 │ m3 - 8-31edo  │   310 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m3 - 8-31edo  │   310 │
    │ Db5 │ II     │ d8-e31-1 │  2323 │ F5   │ I      │ m6-e31-1 │  2013 │ M3 - 10-31edo │   387 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ G5   │ I      │ m7-e31-1 │  2206 │ M3 - 10-31edo │   387 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m3 - 8-31edo  │   310 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m3 - 8-31edo  │   310 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ C6   │ I      │ m3-e31-2 │  2710 │ M3 - 10-31edo │   387 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ Db6  │ I      │ d4-e31-2 │  2826 │ m3 - 8-31edo  │   310 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m3 - 8-31edo  │   310 │
    │ Db6 │ II     │ d8-e31-2 │  3523 │ F6   │ I      │ m6-e31-2 │  3213 │ M3 - 10-31edo │   387 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ A2  │ IV     │ M6-e31   │   890 │ C#3  │ III    │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ B2  │ III    │ M3-e31   │   387 │ D3   │ II     │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ C#3 │ III    │ A4-e31   │   581 │ E3   │ II     │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ D3  │ III    │ 5-e31    │   697 │ F#3  │ II     │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ E3  │ III    │ M6-e31   │   890 │ G#3  │ II     │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ F#3 │ II     │ M3-e31   │   387 │ A3   │ I      │ 0        │     0 │ m3 - 8-31edo  │   310 │
    │ G#3 │ II     │ A4-e31   │   581 │ B3   │ I      │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ A3  │ II     │ 5-e31    │   697 │ C#4  │ I      │ M3-e31   │   387 │ M3 - 10-31edo │   387 │
    │ B3  │ II     │ M6-e31   │   890 │ D4   │ I      │ 4-e31    │   503 │ m3 - 8-31edo  │   310 │
    │ C#4 │ II     │ M7-e31   │  1084 │ E4   │ I      │ 5-e31    │   697 │ m3 - 8-31edo  │   310 │
    │ D4  │ II     │ 0-1      │  1200 │ F#4  │ I      │ M6-e31   │   890 │ M3 - 10-31edo │   387 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G#4  │ I      │ M7-e31   │  1084 │ M3 - 10-31edo │   387 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ A4   │ I      │ 0-1      │  1200 │ m3 - 8-31edo  │   310 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ B4   │ I      │ M2-e31-1 │  1394 │ m3 - 8-31edo  │   310 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C#5  │ I      │ M3-e31-1 │  1587 │ M3 - 10-31edo │   387 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ D5   │ I      │ 4-e31-1  │  1703 │ m3 - 8-31edo  │   310 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ E5   │ I      │ 5-e31-1  │  1897 │ m3 - 8-31edo  │   310 │
    │ D5  │ II     │ 0-2      │  2400 │ F#5  │ I      │ M6-e31-1 │  2090 │ M3 - 10-31edo │   387 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G#5  │ I      │ M7-e31-1 │  2284 │ M3 - 10-31edo │   387 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ A5   │ I      │ 0-2      │  2400 │ m3 - 8-31edo  │   310 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ B5   │ I      │ M2-e31-2 │  2594 │ m3 - 8-31edo  │   310 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C#6  │ I      │ M3-e31-2 │  2787 │ M3 - 10-31edo │   387 │
    │ B5  │ II     │ M6-e31-2 │  3290 │ D6   │ I      │ 4-e31-2  │  2903 │ m3 - 8-31edo  │   310 │
    │ C#6 │ II     │ M7-e31-2 │  3484 │ E6   │ I      │ 5-e31-2  │  3097 │ m3 - 8-31edo  │   310 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_b_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3-e31   │   310 │ D3   │ II     │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ C3  │ III    │ 4-e31    │   503 │ Eb3  │ II     │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ D3  │ III    │ 5-e31    │   697 │ F3   │ II     │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ Eb3 │ III    │ m6-e31   │   813 │ G3   │ II     │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ F3  │ II     │ m3-e31   │   310 │ A3   │ I      │ 0        │     0 │ M3 - 10-31edo │   387 │
    │ G3  │ II     │ 4-e31    │   503 │ Bb3  │ I      │ m2-e31   │   116 │ m3 - 8-31edo  │   310 │
    │ A3  │ II     │ 5-e31    │   697 │ C4   │ I      │ m3-e31   │   310 │ m3 - 8-31edo  │   310 │
    │ Bb3 │ II     │ m6-e31   │   813 │ D4   │ I      │ 4-e31    │   503 │ M3 - 10-31edo │   387 │
    │ C4  │ II     │ m7-e31   │  1006 │ Eb4  │ I      │ d5-e31   │   619 │ m3 - 8-31edo  │   310 │
    │ D4  │ II     │ 0-1      │  1200 │ F4   │ I      │ m6-e31   │   813 │ m3 - 8-31edo  │   310 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ G4   │ I      │ m7-e31   │  1006 │ M3 - 10-31edo │   387 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ A4   │ I      │ 0-1      │  1200 │ M3 - 10-31edo │   387 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m3 - 8-31edo  │   310 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ C5   │ I      │ m3-e31-1 │  1510 │ m3 - 8-31edo  │   310 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ D5   │ I      │ 4-e31-1  │  1703 │ M3 - 10-31edo │   387 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m3 - 8-31edo  │   310 │
    │ D5  │ II     │ 0-2      │  2400 │ F5   │ I      │ m6-e31-1 │  2013 │ m3 - 8-31edo  │   310 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ G5   │ I      │ m7-e31-1 │  2206 │ M3 - 10-31edo │   387 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ A5   │ I      │ 0-2      │  2400 │ M3 - 10-31edo │   387 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m3 - 8-31edo  │   310 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ C6   │ I      │ m3-e31-2 │  2710 │ m3 - 8-31edo  │   310 │
    │ Bb5 │ II     │ m6-e31-2 │  3213 │ D6   │ I      │ 4-e31-2  │  2903 │ M3 - 10-31edo │   387 │
    │ C6  │ II     │ m7-e31-2 │  3406 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m3 - 8-31edo  │   310 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major" =
  let t = force E31.t in
  let scale = make_major_scale ~from:Scales.lower_b in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3-e31   │   387 │ D#3  │ II     │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ C#3 │ III    │ A4-e31   │   581 │ E3   │ II     │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ D#3 │ III    │ A5-e31   │   774 │ F#3  │ II     │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ E3  │ III    │ M6-e31   │   890 │ G#3  │ II     │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ F#3 │ II     │ M3-e31   │   387 │ A#3  │ I      │ A1-e31   │    77 │ M3 - 10-31edo │   387 │
    │ G#3 │ II     │ A4-e31   │   581 │ B3   │ I      │ M2-e31   │   194 │ m3 - 8-31edo  │   310 │
    │ A#3 │ II     │ A5-e31   │   774 │ C#4  │ I      │ M3-e31   │   387 │ m3 - 8-31edo  │   310 │
    │ B3  │ II     │ M6-e31   │   890 │ D#4  │ I      │ A4-e31   │   581 │ M3 - 10-31edo │   387 │
    │ C#4 │ II     │ M7-e31   │  1084 │ E4   │ I      │ 5-e31    │   697 │ m3 - 8-31edo  │   310 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ F#4  │ I      │ M6-e31   │   890 │ m3 - 8-31edo  │   310 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ G#4  │ I      │ M7-e31   │  1084 │ M3 - 10-31edo │   387 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ A#4  │ I      │ A1-e31-1 │  1277 │ M3 - 10-31edo │   387 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ B4   │ I      │ M2-e31-1 │  1394 │ m3 - 8-31edo  │   310 │
    │ A#4 │ II     │ A5-e31-1 │  1974 │ C#5  │ I      │ M3-e31-1 │  1587 │ m3 - 8-31edo  │   310 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ D#5  │ I      │ A4-e31-1 │  1781 │ M3 - 10-31edo │   387 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ E5   │ I      │ 5-e31-1  │  1897 │ m3 - 8-31edo  │   310 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ F#5  │ I      │ M6-e31-1 │  2090 │ m3 - 8-31edo  │   310 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ G#5  │ I      │ M7-e31-1 │  2284 │ M3 - 10-31edo │   387 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ A#5  │ I      │ A1-e31-2 │  2477 │ M3 - 10-31edo │   387 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ B5   │ I      │ M2-e31-2 │  2594 │ m3 - 8-31edo  │   310 │
    │ A#5 │ II     │ A5-e31-2 │  3174 │ C#6  │ I      │ M3-e31-2 │  2787 │ m3 - 8-31edo  │   310 │
    │ B5  │ II     │ M6-e31-2 │  3290 │ D#6  │ I      │ A4-e31-2 │  2981 │ M3 - 10-31edo │   387 │
    │ C#6 │ II     │ M7-e31-2 │  3484 │ E6   │ I      │ 5-e31-2  │  3097 │ m3 - 8-31edo  │   310 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;
