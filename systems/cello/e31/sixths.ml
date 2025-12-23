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
  let t = Lazy.force E31.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_e31 ~from
;;

let%expect_test "c_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ C2  │ IV     │ 0        │     0 │ A2   │ III    │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ D2  │ IV     │ M2-e31   │   194 │ B2   │ III    │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ E2  │ IV     │ M3-e31   │   387 │ C3   │ III    │ 4-e31    │   503 │ m6 - 21-31edo │   813 │
    │ F2  │ IV     │ 4-e31    │   503 │ D3   │ III    │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ A2  │ III    │ M2-e31   │   194 │ F3   │ II     │ m3-e31   │   310 │ m6 - 21-31edo │   813 │
    │ B2  │ III    │ M3-e31   │   387 │ G3   │ II     │ 4-e31    │   503 │ m6 - 21-31edo │   813 │
    │ C3  │ III    │ 4-e31    │   503 │ A3   │ II     │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ E3  │ II     │ M2-e31   │   194 │ C4   │ I      │ m3-e31   │   310 │ m6 - 21-31edo │   813 │
    │ F3  │ II     │ m3-e31   │   310 │ D4   │ I      │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ G3  │ II     │ 4-e31    │   503 │ E4   │ I      │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ A3  │ II     │ 5-e31    │   697 │ F4   │ I      │ m6-e31   │   813 │ m6 - 21-31edo │   813 │
    │ B3  │ II     │ M6-e31   │   890 │ G4   │ I      │ m7-e31   │  1006 │ m6 - 21-31edo │   813 │
    │ C4  │ II     │ m7-e31   │  1006 │ A4   │ I      │ 0-1      │  1200 │ M6 - 23-31edo │   890 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e31-1 │  1394 │ M6 - 23-31edo │   890 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C5   │ I      │ m3-e31-1 │  1510 │ m6 - 21-31edo │   813 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ D5   │ I      │ 4-e31-1  │  1703 │ M6 - 23-31edo │   890 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ E5   │ I      │ 5-e31-1  │  1897 │ M6 - 23-31edo │   890 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F5   │ I      │ m6-e31-1 │  2013 │ m6 - 21-31edo │   813 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ G5   │ I      │ m7-e31-1 │  2206 │ m6 - 21-31edo │   813 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ A5   │ I      │ 0-2      │  2400 │ M6 - 23-31edo │   890 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e31-2 │  2594 │ M6 - 23-31edo │   890 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C6   │ I      │ m3-e31-2 │  2710 │ m6 - 21-31edo │   813 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ D6   │ I      │ 4-e31-2  │  2903 │ M6 - 23-31edo │   890 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ E6   │ I      │ 5-e31-2  │  3097 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_c_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ C#2 │ IV     │ A1-e31   │    77 │ A#2  │ III    │ A2-e31   │   271 │ M6 - 23-31edo │   890 │
    │ D#2 │ IV     │ A2-e31   │   271 │ B#2  │ III    │ A3-e31   │   465 │ M6 - 23-31edo │   890 │
    │ E#2 │ IV     │ A3-e31   │   465 │ C#3  │ III    │ A4-e31   │   581 │ m6 - 21-31edo │   813 │
    │ F#2 │ IV     │ A4-e31   │   581 │ D#3  │ III    │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ G#2 │ III    │ A1-e31   │    77 │ E#3  │ II     │ A2-e31   │   271 │ M6 - 23-31edo │   890 │
    │ A#2 │ III    │ A2-e31   │   271 │ F#3  │ II     │ M3-e31   │   387 │ m6 - 21-31edo │   813 │
    │ B#2 │ III    │ A3-e31   │   465 │ G#3  │ II     │ A4-e31   │   581 │ m6 - 21-31edo │   813 │
    │ C#3 │ III    │ A4-e31   │   581 │ A#3  │ II     │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ D#3 │ II     │ A1-e31   │    77 │ B#3  │ I      │ A2-e31   │   271 │ M6 - 23-31edo │   890 │
    │ E#3 │ II     │ A2-e31   │   271 │ C#4  │ I      │ M3-e31   │   387 │ m6 - 21-31edo │   813 │
    │ F#3 │ II     │ M3-e31   │   387 │ D#4  │ I      │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ G#3 │ II     │ A4-e31   │   581 │ E#4  │ I      │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ A#3 │ II     │ A5-e31   │   774 │ F#4  │ I      │ M6-e31   │   890 │ m6 - 21-31edo │   813 │
    │ C#4 │ II     │ M7-e31   │  1084 │ A#4  │ I      │ A1-e31-1 │  1277 │ M6 - 23-31edo │   890 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ B#4  │ I      │ A2-e31-1 │  1471 │ M6 - 23-31edo │   890 │
    │ E#4 │ II     │ A2-e31-1 │  1471 │ C#5  │ I      │ M3-e31-1 │  1587 │ m6 - 21-31edo │   813 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ D#5  │ I      │ A4-e31-1 │  1781 │ M6 - 23-31edo │   890 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ E#5  │ I      │ A5-e31-1 │  1974 │ M6 - 23-31edo │   890 │
    │ A#4 │ II     │ A5-e31-1 │  1974 │ F#5  │ I      │ M6-e31-1 │  2090 │ m6 - 21-31edo │   813 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ A#5  │ I      │ A1-e31-2 │  2477 │ M6 - 23-31edo │   890 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ B#5  │ I      │ A2-e31-2 │  2671 │ M6 - 23-31edo │   890 │
    │ E#5 │ II     │ A2-e31-2 │  2671 │ C#6  │ I      │ M3-e31-2 │  2787 │ m6 - 21-31edo │   813 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ D#6  │ I      │ A4-e31-2 │  2981 │ M6 - 23-31edo │   890 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ E#6  │ I      │ A5-e31-2 │  3174 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_d_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Db2 │ IV     │ m2-e31   │   116 │ Bb2  │ III    │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ Eb2 │ IV     │ m3-e31   │   310 │ C3   │ III    │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ F2  │ IV     │ 4-e31    │   503 │ Db3  │ III    │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ Gb2 │ IV     │ d5-e31   │   619 │ Eb3  │ III    │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Ab2 │ III    │ m2-e31   │   116 │ F3   │ II     │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ Bb2 │ III    │ m3-e31   │   310 │ Gb3  │ II     │ d4-e31   │   426 │ m6 - 21-31edo │   813 │
    │ C3  │ III    │ 4-e31    │   503 │ Ab3  │ II     │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ Db3 │ III    │ d5-e31   │   619 │ Bb3  │ II     │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Eb3 │ II     │ m2-e31   │   116 │ C4   │ I      │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ F3  │ II     │ m3-e31   │   310 │ Db4  │ I      │ d4-e31   │   426 │ m6 - 21-31edo │   813 │
    │ Gb3 │ II     │ d4-e31   │   426 │ Eb4  │ I      │ d5-e31   │   619 │ M6 - 23-31edo │   890 │
    │ Ab3 │ II     │ d5-e31   │   619 │ F4   │ I      │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Bb3 │ II     │ m6-e31   │   813 │ Gb4  │ I      │ d7-e31   │   929 │ m6 - 21-31edo │   813 │
    │ C4  │ II     │ m7-e31   │  1006 │ Ab4  │ I      │ d8-e31   │  1123 │ m6 - 21-31edo │   813 │
    │ Db4 │ II     │ d8-e31   │  1123 │ Bb4  │ I      │ m2-e31-1 │  1316 │ M6 - 23-31edo │   890 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ C5   │ I      │ m3-e31-1 │  1510 │ M6 - 23-31edo │   890 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Db5  │ I      │ d4-e31-1 │  1626 │ m6 - 21-31edo │   813 │
    │ Gb4 │ II     │ d4-e31-1 │  1626 │ Eb5  │ I      │ d5-e31-1 │  1819 │ M6 - 23-31edo │   890 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ F5   │ I      │ m6-e31-1 │  2013 │ M6 - 23-31edo │   890 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ Gb5  │ I      │ d7-e31-1 │  2129 │ m6 - 21-31edo │   813 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m6 - 21-31edo │   813 │
    │ Db5 │ II     │ d8-e31-1 │  2323 │ Bb5  │ I      │ m2-e31-2 │  2516 │ M6 - 23-31edo │   890 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ C6   │ I      │ m3-e31-2 │  2710 │ M6 - 23-31edo │   890 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Db6  │ I      │ d4-e31-2 │  2826 │ m6 - 21-31edo │   813 │
    │ Gb5 │ II     │ d4-e31-2 │  2826 │ Eb6  │ I      │ d5-e31-2 │  3019 │ M6 - 23-31edo │   890 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ F6   │ I      │ m6-e31-2 │  3213 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ D2  │ IV     │ M2-e31   │   194 │ B2   │ III    │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ E2  │ IV     │ M3-e31   │   387 │ C#3  │ III    │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ F#2 │ IV     │ A4-e31   │   581 │ D3   │ III    │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ A2  │ III    │ M2-e31   │   194 │ F#3  │ II     │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ B2  │ III    │ M3-e31   │   387 │ G3   │ II     │ 4-e31    │   503 │ m6 - 21-31edo │   813 │
    │ C#3 │ III    │ A4-e31   │   581 │ A3   │ II     │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ E3  │ II     │ M2-e31   │   194 │ C#4  │ I      │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ F#3 │ II     │ M3-e31   │   387 │ D4   │ I      │ 4-e31    │   503 │ m6 - 21-31edo │   813 │
    │ G3  │ II     │ 4-e31    │   503 │ E4   │ I      │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ A3  │ II     │ 5-e31    │   697 │ F#4  │ I      │ M6-e31   │   890 │ M6 - 23-31edo │   890 │
    │ B3  │ II     │ M6-e31   │   890 │ G4   │ I      │ m7-e31   │  1006 │ m6 - 21-31edo │   813 │
    │ C#4 │ II     │ M7-e31   │  1084 │ A4   │ I      │ 0-1      │  1200 │ m6 - 21-31edo │   813 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e31-1 │  1394 │ M6 - 23-31edo │   890 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C#5  │ I      │ M3-e31-1 │  1587 │ M6 - 23-31edo │   890 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ D5   │ I      │ 4-e31-1  │  1703 │ m6 - 21-31edo │   813 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ E5   │ I      │ 5-e31-1  │  1897 │ M6 - 23-31edo │   890 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F#5  │ I      │ M6-e31-1 │  2090 │ M6 - 23-31edo │   890 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ G5   │ I      │ m7-e31-1 │  2206 │ m6 - 21-31edo │   813 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ A5   │ I      │ 0-2      │  2400 │ m6 - 21-31edo │   813 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e31-2 │  2594 │ M6 - 23-31edo │   890 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C#6  │ I      │ M3-e31-2 │  2787 │ M6 - 23-31edo │   890 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ D6   │ I      │ 4-e31-2  │  2903 │ m6 - 21-31edo │   813 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ E6   │ I      │ 5-e31-2  │  3097 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_e_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3-e31   │   310 │ C3   │ III    │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ F2  │ IV     │ 4-e31    │   503 │ D3   │ III    │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ G2  │ III    │ 0        │     0 │ Eb3  │ II     │ m2-e31   │   116 │ m6 - 21-31edo │   813 │
    │ Ab2 │ III    │ m2-e31   │   116 │ F3   │ II     │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ Bb2 │ III    │ m3-e31   │   310 │ G3   │ II     │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ C3  │ III    │ 4-e31    │   503 │ Ab3  │ II     │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e31   │   116 │ m6 - 21-31edo │   813 │
    │ Eb3 │ II     │ m2-e31   │   116 │ C4   │ I      │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ F3  │ II     │ m3-e31   │   310 │ D4   │ I      │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ G3  │ II     │ 4-e31    │   503 │ Eb4  │ I      │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ Ab3 │ II     │ d5-e31   │   619 │ F4   │ I      │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Bb3 │ II     │ m6-e31   │   813 │ G4   │ I      │ m7-e31   │  1006 │ M6 - 23-31edo │   890 │
    │ C4  │ II     │ m7-e31   │  1006 │ Ab4  │ I      │ d8-e31   │  1123 │ m6 - 21-31edo │   813 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m6 - 21-31edo │   813 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ C5   │ I      │ m3-e31-1 │  1510 │ M6 - 23-31edo │   890 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ D5   │ I      │ 4-e31-1  │  1703 │ M6 - 23-31edo │   890 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m6 - 21-31edo │   813 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ F5   │ I      │ m6-e31-1 │  2013 │ M6 - 23-31edo │   890 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ G5   │ I      │ m7-e31-1 │  2206 │ M6 - 23-31edo │   890 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m6 - 21-31edo │   813 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m6 - 21-31edo │   813 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ C6   │ I      │ m3-e31-2 │  2710 │ M6 - 23-31edo │   890 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ D6   │ I      │ 4-e31-2  │  2903 │ M6 - 23-31edo │   890 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m6 - 21-31edo │   813 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ F6   │ I      │ m6-e31-2 │  3213 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3-e31   │   387 │ C#3  │ III    │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ F#2 │ IV     │ A4-e31   │   581 │ D#3  │ III    │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ G#2 │ III    │ A1-e31   │    77 │ E3   │ II     │ M2-e31   │   194 │ m6 - 21-31edo │   813 │
    │ A2  │ III    │ M2-e31   │   194 │ F#3  │ II     │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ B2  │ III    │ M3-e31   │   387 │ G#3  │ II     │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ C#3 │ III    │ A4-e31   │   581 │ A3   │ II     │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ D#3 │ II     │ A1-e31   │    77 │ B3   │ I      │ M2-e31   │   194 │ m6 - 21-31edo │   813 │
    │ E3  │ II     │ M2-e31   │   194 │ C#4  │ I      │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ F#3 │ II     │ M3-e31   │   387 │ D#4  │ I      │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ G#3 │ II     │ A4-e31   │   581 │ E4   │ I      │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ A3  │ II     │ 5-e31    │   697 │ F#4  │ I      │ M6-e31   │   890 │ M6 - 23-31edo │   890 │
    │ B3  │ II     │ M6-e31   │   890 │ G#4  │ I      │ M7-e31   │  1084 │ M6 - 23-31edo │   890 │
    │ C#4 │ II     │ M7-e31   │  1084 │ A4   │ I      │ 0-1      │  1200 │ m6 - 21-31edo │   813 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ B4   │ I      │ M2-e31-1 │  1394 │ m6 - 21-31edo │   813 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C#5  │ I      │ M3-e31-1 │  1587 │ M6 - 23-31edo │   890 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ D#5  │ I      │ A4-e31-1 │  1781 │ M6 - 23-31edo │   890 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ E5   │ I      │ 5-e31-1  │  1897 │ m6 - 21-31edo │   813 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F#5  │ I      │ M6-e31-1 │  2090 │ M6 - 23-31edo │   890 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ G#5  │ I      │ M7-e31-1 │  2284 │ M6 - 23-31edo │   890 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ A5   │ I      │ 0-2      │  2400 │ m6 - 21-31edo │   813 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ B5   │ I      │ M2-e31-2 │  2594 │ m6 - 21-31edo │   813 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C#6  │ I      │ M3-e31-2 │  2787 │ M6 - 23-31edo │   890 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ D#6  │ I      │ A4-e31-2 │  2981 │ M6 - 23-31edo │   890 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ E6   │ I      │ 5-e31-2  │  3097 │ m6 - 21-31edo │   813 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e31    │   503 │ D3   │ III    │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ A2  │ III    │ M2-e31   │   194 │ F3   │ II     │ m3-e31   │   310 │ m6 - 21-31edo │   813 │
    │ Bb2 │ III    │ m3-e31   │   310 │ G3   │ II     │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ C3  │ III    │ 4-e31    │   503 │ A3   │ II     │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e31   │   116 │ m6 - 21-31edo │   813 │
    │ E3  │ II     │ M2-e31   │   194 │ C4   │ I      │ m3-e31   │   310 │ m6 - 21-31edo │   813 │
    │ F3  │ II     │ m3-e31   │   310 │ D4   │ I      │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ G3  │ II     │ 4-e31    │   503 │ E4   │ I      │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ A3  │ II     │ 5-e31    │   697 │ F4   │ I      │ m6-e31   │   813 │ m6 - 21-31edo │   813 │
    │ Bb3 │ II     │ m6-e31   │   813 │ G4   │ I      │ m7-e31   │  1006 │ M6 - 23-31edo │   890 │
    │ C4  │ II     │ m7-e31   │  1006 │ A4   │ I      │ 0-1      │  1200 │ M6 - 23-31edo │   890 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m6 - 21-31edo │   813 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C5   │ I      │ m3-e31-1 │  1510 │ m6 - 21-31edo │   813 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ D5   │ I      │ 4-e31-1  │  1703 │ M6 - 23-31edo │   890 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ E5   │ I      │ 5-e31-1  │  1897 │ M6 - 23-31edo │   890 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F5   │ I      │ m6-e31-1 │  2013 │ m6 - 21-31edo │   813 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ G5   │ I      │ m7-e31-1 │  2206 │ M6 - 23-31edo │   890 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ A5   │ I      │ 0-2      │  2400 │ M6 - 23-31edo │   890 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m6 - 21-31edo │   813 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C6   │ I      │ m3-e31-2 │  2710 │ m6 - 21-31edo │   813 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ D6   │ I      │ 4-e31-2  │  2903 │ M6 - 23-31edo │   890 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ E6   │ I      │ 5-e31-2  │  3097 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_f_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ A4-e31   │   581 │ D#3  │ III    │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ G#2 │ III    │ A1-e31   │    77 │ E#3  │ II     │ A2-e31   │   271 │ M6 - 23-31edo │   890 │
    │ A#2 │ III    │ A2-e31   │   271 │ F#3  │ II     │ M3-e31   │   387 │ m6 - 21-31edo │   813 │
    │ B2  │ III    │ M3-e31   │   387 │ G#3  │ II     │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ C#3 │ III    │ A4-e31   │   581 │ A#3  │ II     │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ D#3 │ II     │ A1-e31   │    77 │ B3   │ I      │ M2-e31   │   194 │ m6 - 21-31edo │   813 │
    │ E#3 │ II     │ A2-e31   │   271 │ C#4  │ I      │ M3-e31   │   387 │ m6 - 21-31edo │   813 │
    │ F#3 │ II     │ M3-e31   │   387 │ D#4  │ I      │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ G#3 │ II     │ A4-e31   │   581 │ E#4  │ I      │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ A#3 │ II     │ A5-e31   │   774 │ F#4  │ I      │ M6-e31   │   890 │ m6 - 21-31edo │   813 │
    │ B3  │ II     │ M6-e31   │   890 │ G#4  │ I      │ M7-e31   │  1084 │ M6 - 23-31edo │   890 │
    │ C#4 │ II     │ M7-e31   │  1084 │ A#4  │ I      │ A1-e31-1 │  1277 │ M6 - 23-31edo │   890 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ B4   │ I      │ M2-e31-1 │  1394 │ m6 - 21-31edo │   813 │
    │ E#4 │ II     │ A2-e31-1 │  1471 │ C#5  │ I      │ M3-e31-1 │  1587 │ m6 - 21-31edo │   813 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ D#5  │ I      │ A4-e31-1 │  1781 │ M6 - 23-31edo │   890 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ E#5  │ I      │ A5-e31-1 │  1974 │ M6 - 23-31edo │   890 │
    │ A#4 │ II     │ A5-e31-1 │  1974 │ F#5  │ I      │ M6-e31-1 │  2090 │ m6 - 21-31edo │   813 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ G#5  │ I      │ M7-e31-1 │  2284 │ M6 - 23-31edo │   890 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ A#5  │ I      │ A1-e31-2 │  2477 │ M6 - 23-31edo │   890 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ B5   │ I      │ M2-e31-2 │  2594 │ m6 - 21-31edo │   813 │
    │ E#5 │ II     │ A2-e31-2 │  2671 │ C#6  │ I      │ M3-e31-2 │  2787 │ m6 - 21-31edo │   813 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ D#6  │ I      │ A4-e31-2 │  2981 │ M6 - 23-31edo │   890 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ E#6  │ I      │ A5-e31-2 │  3174 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_g_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ d5-e31   │   619 │ Eb3  │ III    │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Ab2 │ III    │ m2-e31   │   116 │ F3   │ II     │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ Bb2 │ III    │ m3-e31   │   310 │ Gb3  │ II     │ d4-e31   │   426 │ m6 - 21-31edo │   813 │
    │ Cb3 │ III    │ d4-e31   │   426 │ Ab3  │ II     │ d5-e31   │   619 │ M6 - 23-31edo │   890 │
    │ Db3 │ III    │ d5-e31   │   619 │ Bb3  │ II     │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Eb3 │ II     │ m2-e31   │   116 │ Cb4  │ I      │ d3-e31   │   232 │ m6 - 21-31edo │   813 │
    │ F3  │ II     │ m3-e31   │   310 │ Db4  │ I      │ d4-e31   │   426 │ m6 - 21-31edo │   813 │
    │ Gb3 │ II     │ d4-e31   │   426 │ Eb4  │ I      │ d5-e31   │   619 │ M6 - 23-31edo │   890 │
    │ Ab3 │ II     │ d5-e31   │   619 │ F4   │ I      │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Bb3 │ II     │ m6-e31   │   813 │ Gb4  │ I      │ d7-e31   │   929 │ m6 - 21-31edo │   813 │
    │ Cb4 │ II     │ d7-e31   │   929 │ Ab4  │ I      │ d8-e31   │  1123 │ M6 - 23-31edo │   890 │
    │ Db4 │ II     │ d8-e31   │  1123 │ Bb4  │ I      │ m2-e31-1 │  1316 │ M6 - 23-31edo │   890 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ Cb5  │ I      │ d3-e31-1 │  1432 │ m6 - 21-31edo │   813 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Db5  │ I      │ d4-e31-1 │  1626 │ m6 - 21-31edo │   813 │
    │ Gb4 │ II     │ d4-e31-1 │  1626 │ Eb5  │ I      │ d5-e31-1 │  1819 │ M6 - 23-31edo │   890 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ F5   │ I      │ m6-e31-1 │  2013 │ M6 - 23-31edo │   890 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ Gb5  │ I      │ d7-e31-1 │  2129 │ m6 - 21-31edo │   813 │
    │ Cb5 │ II     │ d7-e31-1 │  2129 │ Ab5  │ I      │ d8-e31-1 │  2323 │ M6 - 23-31edo │   890 │
    │ Db5 │ II     │ d8-e31-1 │  2323 │ Bb5  │ I      │ m2-e31-2 │  2516 │ M6 - 23-31edo │   890 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ Cb6  │ I      │ d3-e31-2 │  2632 │ m6 - 21-31edo │   813 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Db6  │ I      │ d4-e31-2 │  2826 │ m6 - 21-31edo │   813 │
    │ Gb5 │ II     │ d4-e31-2 │  2826 │ Eb6  │ I      │ d5-e31-2 │  3019 │ M6 - 23-31edo │   890 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ F6   │ I      │ m6-e31-2 │  3213 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_f in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4-e31    │   503 │ D3   │ III    │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ G2  │ III    │ 0        │     0 │ E3   │ II     │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ A2  │ III    │ M2-e31   │   194 │ F3   │ II     │ m3-e31   │   310 │ m6 - 21-31edo │   813 │
    │ Bb2 │ III    │ m3-e31   │   310 │ G3   │ II     │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ C3  │ III    │ 4-e31    │   503 │ A3   │ II     │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e31   │   116 │ m6 - 21-31edo │   813 │
    │ E3  │ II     │ M2-e31   │   194 │ C4   │ I      │ m3-e31   │   310 │ m6 - 21-31edo │   813 │
    │ F3  │ II     │ m3-e31   │   310 │ D4   │ I      │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ G3  │ II     │ 4-e31    │   503 │ E4   │ I      │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ A3  │ II     │ 5-e31    │   697 │ F4   │ I      │ m6-e31   │   813 │ m6 - 21-31edo │   813 │
    │ Bb3 │ II     │ m6-e31   │   813 │ G4   │ I      │ m7-e31   │  1006 │ M6 - 23-31edo │   890 │
    │ C4  │ II     │ m7-e31   │  1006 │ A4   │ I      │ 0-1      │  1200 │ M6 - 23-31edo │   890 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m6 - 21-31edo │   813 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C5   │ I      │ m3-e31-1 │  1510 │ m6 - 21-31edo │   813 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ D5   │ I      │ 4-e31-1  │  1703 │ M6 - 23-31edo │   890 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ E5   │ I      │ 5-e31-1  │  1897 │ M6 - 23-31edo │   890 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F5   │ I      │ m6-e31-1 │  2013 │ m6 - 21-31edo │   813 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ G5   │ I      │ m7-e31-1 │  2206 │ M6 - 23-31edo │   890 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ A5   │ I      │ 0-2      │  2400 │ M6 - 23-31edo │   890 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m6 - 21-31edo │   813 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C6   │ I      │ m3-e31-2 │  2710 │ m6 - 21-31edo │   813 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ D6   │ I      │ 4-e31-2  │  2903 │ M6 - 23-31edo │   890 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ E6   │ I      │ 5-e31-2  │  3097 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_a_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Ab2 │ III    │ m2-e31   │   116 │ F3   │ II     │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ Bb2 │ III    │ m3-e31   │   310 │ G3   │ II     │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ C3  │ III    │ 4-e31    │   503 │ Ab3  │ II     │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ Db3 │ III    │ d5-e31   │   619 │ Bb3  │ II     │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Eb3 │ II     │ m2-e31   │   116 │ C4   │ I      │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ F3  │ II     │ m3-e31   │   310 │ Db4  │ I      │ d4-e31   │   426 │ m6 - 21-31edo │   813 │
    │ G3  │ II     │ 4-e31    │   503 │ Eb4  │ I      │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ Ab3 │ II     │ d5-e31   │   619 │ F4   │ I      │ m6-e31   │   813 │ M6 - 23-31edo │   890 │
    │ Bb3 │ II     │ m6-e31   │   813 │ G4   │ I      │ m7-e31   │  1006 │ M6 - 23-31edo │   890 │
    │ C4  │ II     │ m7-e31   │  1006 │ Ab4  │ I      │ d8-e31   │  1123 │ m6 - 21-31edo │   813 │
    │ Db4 │ II     │ d8-e31   │  1123 │ Bb4  │ I      │ m2-e31-1 │  1316 │ M6 - 23-31edo │   890 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ C5   │ I      │ m3-e31-1 │  1510 │ M6 - 23-31edo │   890 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ Db5  │ I      │ d4-e31-1 │  1626 │ m6 - 21-31edo │   813 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m6 - 21-31edo │   813 │
    │ Ab4 │ II     │ d5-e31-1 │  1819 │ F5   │ I      │ m6-e31-1 │  2013 │ M6 - 23-31edo │   890 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ G5   │ I      │ m7-e31-1 │  2206 │ M6 - 23-31edo │   890 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ Ab5  │ I      │ d8-e31-1 │  2323 │ m6 - 21-31edo │   813 │
    │ Db5 │ II     │ d8-e31-1 │  2323 │ Bb5  │ I      │ m2-e31-2 │  2516 │ M6 - 23-31edo │   890 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ C6   │ I      │ m3-e31-2 │  2710 │ M6 - 23-31edo │   890 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ Db6  │ I      │ d4-e31-2 │  2826 │ m6 - 21-31edo │   813 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m6 - 21-31edo │   813 │
    │ Ab5 │ II     │ d5-e31-2 │  3019 │ F6   │ I      │ m6-e31-2 │  3213 │ M6 - 23-31edo │   890 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ A2  │ III    │ M2-e31   │   194 │ F#3  │ II     │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ B2  │ III    │ M3-e31   │   387 │ G#3  │ II     │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ C#3 │ III    │ A4-e31   │   581 │ A3   │ II     │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ D3  │ II     │ 0        │     0 │ B3   │ I      │ M2-e31   │   194 │ M6 - 23-31edo │   890 │
    │ E3  │ II     │ M2-e31   │   194 │ C#4  │ I      │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ F#3 │ II     │ M3-e31   │   387 │ D4   │ I      │ 4-e31    │   503 │ m6 - 21-31edo │   813 │
    │ G#3 │ II     │ A4-e31   │   581 │ E4   │ I      │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ A3  │ II     │ 5-e31    │   697 │ F#4  │ I      │ M6-e31   │   890 │ M6 - 23-31edo │   890 │
    │ B3  │ II     │ M6-e31   │   890 │ G#4  │ I      │ M7-e31   │  1084 │ M6 - 23-31edo │   890 │
    │ C#4 │ II     │ M7-e31   │  1084 │ A4   │ I      │ 0-1      │  1200 │ m6 - 21-31edo │   813 │
    │ D4  │ II     │ 0-1      │  1200 │ B4   │ I      │ M2-e31-1 │  1394 │ M6 - 23-31edo │   890 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C#5  │ I      │ M3-e31-1 │  1587 │ M6 - 23-31edo │   890 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ D5   │ I      │ 4-e31-1  │  1703 │ m6 - 21-31edo │   813 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ E5   │ I      │ 5-e31-1  │  1897 │ m6 - 21-31edo │   813 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F#5  │ I      │ M6-e31-1 │  2090 │ M6 - 23-31edo │   890 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ G#5  │ I      │ M7-e31-1 │  2284 │ M6 - 23-31edo │   890 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ A5   │ I      │ 0-2      │  2400 │ m6 - 21-31edo │   813 │
    │ D5  │ II     │ 0-2      │  2400 │ B5   │ I      │ M2-e31-2 │  2594 │ M6 - 23-31edo │   890 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C#6  │ I      │ M3-e31-2 │  2787 │ M6 - 23-31edo │   890 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ D6   │ I      │ 4-e31-2  │  2903 │ m6 - 21-31edo │   813 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ E6   │ I      │ 5-e31-2  │  3097 │ m6 - 21-31edo │   813 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_b_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3-e31   │   310 │ G3   │ II     │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ C3  │ III    │ 4-e31    │   503 │ A3   │ II     │ 5-e31    │   697 │ M6 - 23-31edo │   890 │
    │ D3  │ II     │ 0        │     0 │ Bb3  │ I      │ m2-e31   │   116 │ m6 - 21-31edo │   813 │
    │ Eb3 │ II     │ m2-e31   │   116 │ C4   │ I      │ m3-e31   │   310 │ M6 - 23-31edo │   890 │
    │ F3  │ II     │ m3-e31   │   310 │ D4   │ I      │ 4-e31    │   503 │ M6 - 23-31edo │   890 │
    │ G3  │ II     │ 4-e31    │   503 │ Eb4  │ I      │ d5-e31   │   619 │ m6 - 21-31edo │   813 │
    │ A3  │ II     │ 5-e31    │   697 │ F4   │ I      │ m6-e31   │   813 │ m6 - 21-31edo │   813 │
    │ Bb3 │ II     │ m6-e31   │   813 │ G4   │ I      │ m7-e31   │  1006 │ M6 - 23-31edo │   890 │
    │ C4  │ II     │ m7-e31   │  1006 │ A4   │ I      │ 0-1      │  1200 │ M6 - 23-31edo │   890 │
    │ D4  │ II     │ 0-1      │  1200 │ Bb4  │ I      │ m2-e31-1 │  1316 │ m6 - 21-31edo │   813 │
    │ Eb4 │ II     │ m2-e31-1 │  1316 │ C5   │ I      │ m3-e31-1 │  1510 │ M6 - 23-31edo │   890 │
    │ F4  │ II     │ m3-e31-1 │  1510 │ D5   │ I      │ 4-e31-1  │  1703 │ M6 - 23-31edo │   890 │
    │ G4  │ II     │ 4-e31-1  │  1703 │ Eb5  │ I      │ d5-e31-1 │  1819 │ m6 - 21-31edo │   813 │
    │ A4  │ II     │ 5-e31-1  │  1897 │ F5   │ I      │ m6-e31-1 │  2013 │ m6 - 21-31edo │   813 │
    │ Bb4 │ II     │ m6-e31-1 │  2013 │ G5   │ I      │ m7-e31-1 │  2206 │ M6 - 23-31edo │   890 │
    │ C5  │ II     │ m7-e31-1 │  2206 │ A5   │ I      │ 0-2      │  2400 │ M6 - 23-31edo │   890 │
    │ D5  │ II     │ 0-2      │  2400 │ Bb5  │ I      │ m2-e31-2 │  2516 │ m6 - 21-31edo │   813 │
    │ Eb5 │ II     │ m2-e31-2 │  2516 │ C6   │ I      │ m3-e31-2 │  2710 │ M6 - 23-31edo │   890 │
    │ F5  │ II     │ m3-e31-2 │  2710 │ D6   │ I      │ 4-e31-2  │  2903 │ M6 - 23-31edo │   890 │
    │ G5  │ II     │ 4-e31-2  │  2903 │ Eb6  │ I      │ d5-e31-2 │  3019 │ m6 - 21-31edo │   813 │
    │ A5  │ II     │ 5-e31-2  │  3097 │ F6   │ I      │ m6-e31-2 │  3213 │ m6 - 21-31edo │   813 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major" =
  let t = Lazy.force E31.t in
  let scale = make_major_scale ~from:Scales.lower_b in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬──────────┬───────┬──────┬────────┬──────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos      │ Cents │ High │ String │ Pos      │ Cents │ Interval      │ Cents │
    ├─────┼────────┼──────────┼───────┼──────┼────────┼──────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3-e31   │   387 │ G#3  │ II     │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ C#3 │ III    │ A4-e31   │   581 │ A#3  │ II     │ A5-e31   │   774 │ M6 - 23-31edo │   890 │
    │ D#3 │ II     │ A1-e31   │    77 │ B3   │ I      │ M2-e31   │   194 │ m6 - 21-31edo │   813 │
    │ E3  │ II     │ M2-e31   │   194 │ C#4  │ I      │ M3-e31   │   387 │ M6 - 23-31edo │   890 │
    │ F#3 │ II     │ M3-e31   │   387 │ D#4  │ I      │ A4-e31   │   581 │ M6 - 23-31edo │   890 │
    │ G#3 │ II     │ A4-e31   │   581 │ E4   │ I      │ 5-e31    │   697 │ m6 - 21-31edo │   813 │
    │ A#3 │ II     │ A5-e31   │   774 │ F#4  │ I      │ M6-e31   │   890 │ m6 - 21-31edo │   813 │
    │ B3  │ II     │ M6-e31   │   890 │ G#4  │ I      │ M7-e31   │  1084 │ M6 - 23-31edo │   890 │
    │ C#4 │ II     │ M7-e31   │  1084 │ A#4  │ I      │ A1-e31-1 │  1277 │ M6 - 23-31edo │   890 │
    │ D#4 │ II     │ A1-e31-1 │  1277 │ B4   │ I      │ M2-e31-1 │  1394 │ m6 - 21-31edo │   813 │
    │ E4  │ II     │ M2-e31-1 │  1394 │ C#5  │ I      │ M3-e31-1 │  1587 │ M6 - 23-31edo │   890 │
    │ F#4 │ II     │ M3-e31-1 │  1587 │ D#5  │ I      │ A4-e31-1 │  1781 │ M6 - 23-31edo │   890 │
    │ G#4 │ II     │ A4-e31-1 │  1781 │ E5   │ I      │ 5-e31-1  │  1897 │ m6 - 21-31edo │   813 │
    │ A#4 │ II     │ A5-e31-1 │  1974 │ F#5  │ I      │ M6-e31-1 │  2090 │ m6 - 21-31edo │   813 │
    │ B4  │ II     │ M6-e31-1 │  2090 │ G#5  │ I      │ M7-e31-1 │  2284 │ M6 - 23-31edo │   890 │
    │ C#5 │ II     │ M7-e31-1 │  2284 │ A#5  │ I      │ A1-e31-2 │  2477 │ M6 - 23-31edo │   890 │
    │ D#5 │ II     │ A1-e31-2 │  2477 │ B5   │ I      │ M2-e31-2 │  2594 │ m6 - 21-31edo │   813 │
    │ E5  │ II     │ M2-e31-2 │  2594 │ C#6  │ I      │ M3-e31-2 │  2787 │ M6 - 23-31edo │   890 │
    │ F#5 │ II     │ M3-e31-2 │  2787 │ D#6  │ I      │ A4-e31-2 │  2981 │ M6 - 23-31edo │   890 │
    │ G#5 │ II     │ A4-e31-2 │  2981 │ E6   │ I      │ 5-e31-2  │  3097 │ m6 - 21-31edo │   813 │
    └─────┴────────┴──────────┴───────┴──────┴────────┴──────────┴───────┴───────────────┴───────┘ |}]
;;
