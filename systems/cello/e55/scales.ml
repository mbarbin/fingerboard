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

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  let t = Lazy.force E55.t in
  make_scale t ~characterized_scale:Characterized_scale.major_e55 ~from
;;

let lower_c =
  let t = Lazy.force E55.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let%expect_test "c_major" =
  let scale = make_major_scale ~from:lower_c in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV (
       (C2 0)
       (D2 M2-e55)
       (E2 M3-e55)
       (F2 4-e55)))
     (III (
       (G2 0)
       (A2 M2-e55)
       (B2 M3-e55)
       (C3 4-e55)))
     (II (
       (D3 0)
       (E3 M2-e55)
       (F3 m3-e55)
       (G3 4-e55)))
     (I (
       (A3 0)
       (B3 M2-e55)
       (C4 m3-e55)
       (D4 4-e55)
       (E4 5-e55)
       (F4 m6-e55)
       (G4 m7-e55)
       (A4 0-1)
       (B4 M2-e55-1)
       (C5 m3-e55-1)
       (D5 4-e55-1)
       (E5 5-e55-1)
       (F5 m6-e55-1)
       (G5 m7-e55-1)
       (A5 0-2)
       (B5 M2-e55-2)
       (C6 m3-e55-2)
       (D6 4-e55-2)
       (E6 5-e55-2)))) |}];
  ()
;;

let lower_c_sharp =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = C; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A1_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "c_sharp_major" =
  let scale = make_major_scale ~from:lower_c_sharp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV (
       (C#2 A1-e55)
       (D#2 A2-e55)
       (E#2 A3-e55)
       (F#2 A4-e55)))
     (III (
       (G#2 A1-e55)
       (A#2 A2-e55)
       (B#2 A3-e55)
       (C#3 A4-e55)))
     (II (
       (D#3 A1-e55)
       (E#3 A2-e55)
       (F#3 M3-e55)
       (G#3 A4-e55)))
     (I (
       (A#3 A1-e55)
       (B#3 A2-e55)
       (C#4 M3-e55)
       (D#4 A4-e55)
       (E#4 A5-e55)
       (F#4 M6-e55)
       (G#4 M7-e55)
       (A#4 A1-e55-1)
       (B#4 A2-e55-1)
       (C#5 M3-e55-1)
       (D#5 A4-e55-1)
       (E#5 A5-e55-1)
       (F#5 M6-e55-1)
       (G#5 M7-e55-1)
       (A#5 A1-e55-2)
       (B#5 A2-e55-2)
       (C#6 M3-e55-2)
       (D#6 A4-e55-2)
       (E#6 A5-e55-2)))) |}];
  ()
;;

let lower_d_flat =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = D; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "d_flat_major" =
  let scale = make_major_scale ~from:lower_d_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV (
       (Db2 m2-e55)
       (Eb2 m3-e55)
       (F2  4-e55)
       (Gb2 d5-e55)))
     (III (
       (Ab2 m2-e55)
       (Bb2 m3-e55)
       (C3  4-e55)
       (Db3 d5-e55)))
     (II (
       (Eb3 m2-e55)
       (F3  m3-e55)
       (Gb3 d4-e55)
       (Ab3 d5-e55)))
     (I (
       (Bb3 m2-e55)
       (C4  m3-e55)
       (Db4 d4-e55)
       (Eb4 d5-e55)
       (F4  m6-e55)
       (Gb4 d7-e55)
       (Ab4 d8-e55)
       (Bb4 m2-e55-1)
       (C5  m3-e55-1)
       (Db5 d4-e55-1)
       (Eb5 d5-e55-1)
       (F5  m6-e55-1)
       (Gb5 d7-e55-1)
       (Ab5 d8-e55-1)
       (Bb5 m2-e55-2)
       (C6  m3-e55-2)
       (Db6 d4-e55-2)
       (Eb6 d5-e55-2)
       (F6  m6-e55-2)))) |}];
  ()
;;

let lower_d =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = D; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "d_major" =
  let scale = make_major_scale ~from:lower_d in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 30 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV (
       (D2  M2-e55)
       (E2  M3-e55)
       (F#2 A4-e55)))
     (III (
       (G2  0)
       (A2  M2-e55)
       (B2  M3-e55)
       (C#3 A4-e55)))
     (II (
       (D3  0)
       (E3  M2-e55)
       (F#3 M3-e55)
       (G3  4-e55)))
     (I (
       (A3  0)
       (B3  M2-e55)
       (C#4 M3-e55)
       (D4  4-e55)
       (E4  5-e55)
       (F#4 M6-e55)
       (G4  m7-e55)
       (A4  0-1)
       (B4  M2-e55-1)
       (C#5 M3-e55-1)
       (D5  4-e55-1)
       (E5  5-e55-1)
       (F#5 M6-e55-1)
       (G5  m7-e55-1)
       (A5  0-2)
       (B5  M2-e55-2)
       (C#6 M3-e55-2)
       (D6  4-e55-2)
       (E6  5-e55-2)))) |}];
  ()
;;

let lower_e_flat =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "e_flat_major" =
  let scale = make_major_scale ~from:lower_e_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 30 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV (
       (Eb2 m3-e55)
       (F2  4-e55)))
     (III (
       (G2  0)
       (Ab2 m2-e55)
       (Bb2 m3-e55)
       (C3  4-e55)))
     (II (
       (D3  0)
       (Eb3 m2-e55)
       (F3  m3-e55)
       (G3  4-e55)
       (Ab3 d5-e55)))
     (I (
       (Bb3 m2-e55)
       (C4  m3-e55)
       (D4  4-e55)
       (Eb4 d5-e55)
       (F4  m6-e55)
       (G4  m7-e55)
       (Ab4 d8-e55)
       (Bb4 m2-e55-1)
       (C5  m3-e55-1)
       (D5  4-e55-1)
       (Eb5 d5-e55-1)
       (F5  m6-e55-1)
       (G5  m7-e55-1)
       (Ab5 d8-e55-1)
       (Bb5 m2-e55-2)
       (C6  m3-e55-2)
       (D6  4-e55-2)
       (Eb6 d5-e55-2)
       (F6  m6-e55-2)))) |}];
  ()
;;

let lower_e =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = E; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "e_major" =
  let scale = make_major_scale ~from:lower_e in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 29 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV (
       (E2  M3-e55)
       (F#2 A4-e55)))
     (III (
       (G#2 A1-e55)
       (A2  M2-e55)
       (B2  M3-e55)
       (C#3 A4-e55)))
     (II (
       (D#3 A1-e55)
       (E3  M2-e55)
       (F#3 M3-e55)
       (G#3 A4-e55)))
     (I (
       (A3  0)
       (B3  M2-e55)
       (C#4 M3-e55)
       (D#4 A4-e55)
       (E4  5-e55)
       (F#4 M6-e55)
       (G#4 M7-e55)
       (A4  0-1)
       (B4  M2-e55-1)
       (C#5 M3-e55-1)
       (D#5 A4-e55-1)
       (E5  5-e55-1)
       (F#5 M6-e55-1)
       (G#5 M7-e55-1)
       (A5  0-2)
       (B5  M2-e55-2)
       (C#6 M3-e55-2)
       (D#6 A4-e55-2)
       (E6  5-e55-2)))) |}];
  ()
;;

let lower_f =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "f_major" =
  let scale = make_major_scale ~from:lower_f in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((F2 4-e55)))
     (III (
       (G2  0)
       (A2  M2-e55)
       (Bb2 m3-e55)
       (C3  4-e55)))
     (II (
       (D3 0)
       (E3 M2-e55)
       (F3 m3-e55)
       (G3 4-e55)))
     (I (
       (A3  0)
       (Bb3 m2-e55)
       (C4  m3-e55)
       (D4  4-e55)
       (E4  5-e55)
       (F4  m6-e55)
       (G4  m7-e55)
       (A4  0-1)
       (Bb4 m2-e55-1)
       (C5  m3-e55-1)
       (D5  4-e55-1)
       (E5  5-e55-1)
       (F5  m6-e55-1)
       (G5  m7-e55-1)
       (A5  0-2)
       (Bb5 m2-e55-2)
       (C6  m3-e55-2)
       (D6  4-e55-2)
       (E6  5-e55-2)))) |}];
  ()
;;

let lower_f_sharp =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = F; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A4_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "f_sharp_major" =
  let scale = make_major_scale ~from:lower_f_sharp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((F#2 A4-e55)))
     (III (
       (G#2 A1-e55)
       (A#2 A2-e55)
       (B2  M3-e55)
       (C#3 A4-e55)))
     (II (
       (D#3 A1-e55)
       (E#3 A2-e55)
       (F#3 M3-e55)
       (G#3 A4-e55)))
     (I (
       (A#3 A1-e55)
       (B3  M2-e55)
       (C#4 M3-e55)
       (D#4 A4-e55)
       (E#4 A5-e55)
       (F#4 M6-e55)
       (G#4 M7-e55)
       (A#4 A1-e55-1)
       (B4  M2-e55-1)
       (C#5 M3-e55-1)
       (D#5 A4-e55-1)
       (E#5 A5-e55-1)
       (F#5 M6-e55-1)
       (G#5 M7-e55-1)
       (A#5 A1-e55-2)
       (B5  M2-e55-2)
       (C#6 M3-e55-2)
       (D#6 A4-e55-2)
       (E#6 A5-e55-2)))) |}];
  ()
;;

let lower_g_flat =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = G; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `d5_e55
      ; string_number = IV
      }
  }
;;

let%expect_test "g_flat_major" =
  let scale = make_major_scale ~from:lower_g_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Gb2 d5-e55)))
     (III (
       (Ab2 m2-e55)
       (Bb2 m3-e55)
       (Cb3 d4-e55)
       (Db3 d5-e55)))
     (II (
       (Eb3 m2-e55)
       (F3  m3-e55)
       (Gb3 d4-e55)
       (Ab3 d5-e55)))
     (I (
       (Bb3 m2-e55)
       (Cb4 d3-e55)
       (Db4 d4-e55)
       (Eb4 d5-e55)
       (F4  m6-e55)
       (Gb4 d7-e55)
       (Ab4 d8-e55)
       (Bb4 m2-e55-1)
       (Cb5 d3-e55-1)
       (Db5 d4-e55-1)
       (Eb5 d5-e55-1)
       (F5  m6-e55-1)
       (Gb5 d7-e55-1)
       (Ab5 d8-e55-1)
       (Bb5 m2-e55-2)
       (Cb6 d3-e55-2)
       (Db6 d4-e55-2)
       (Eb6 d5-e55-2)
       (F6  m6-e55-2)))) |}];
  ()
;;

let lower_g =
  let t = Lazy.force E55.t in
  System.open_string t III |> Option.value_exn ~here:[%here]
;;

let%expect_test "g_major" =
  let scale = make_major_scale ~from:lower_g in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III (
       (G2 0)
       (A2 M2-e55)
       (B2 M3-e55)
       (C3 4-e55)))
     (II (
       (D3  0)
       (E3  M2-e55)
       (F#3 M3-e55)
       (G3  4-e55)))
     (I (
       (A3  0)
       (B3  M2-e55)
       (C4  m3-e55)
       (D4  4-e55)
       (E4  5-e55)
       (F#4 M6-e55)
       (G4  m7-e55)
       (A4  0-1)
       (B4  M2-e55-1)
       (C5  m3-e55-1)
       (D5  4-e55-1)
       (E5  5-e55-1)
       (F#5 M6-e55-1)
       (G5  m7-e55-1)
       (A5  0-2)
       (B5  M2-e55-2)
       (C6  m3-e55-2)
       (D6  4-e55-2)
       (E6  5-e55-2)))) |}];
  ()
;;

let lower_a_flat =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = A; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2_e55
      ; string_number = III
      }
  }
;;

let%expect_test "a_flat_major" =
  let scale = make_major_scale ~from:lower_a_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III (
       (Ab2 m2-e55)
       (Bb2 m3-e55)
       (C3  4-e55)
       (Db3 d5-e55)))
     (II (
       (Eb3 m2-e55)
       (F3  m3-e55)
       (G3  4-e55)
       (Ab3 d5-e55)))
     (I (
       (Bb3 m2-e55)
       (C4  m3-e55)
       (Db4 d4-e55)
       (Eb4 d5-e55)
       (F4  m6-e55)
       (G4  m7-e55)
       (Ab4 d8-e55)
       (Bb4 m2-e55-1)
       (C5  m3-e55-1)
       (Db5 d4-e55-1)
       (Eb5 d5-e55-1)
       (F5  m6-e55-1)
       (G5  m7-e55-1)
       (Ab5 d8-e55-1)
       (Bb5 m2-e55-2)
       (C6  m3-e55-2)
       (Db6 d4-e55-2)
       (Eb6 d5-e55-2)
       (F6  m6-e55-2)))) |}];
  ()
;;

let lower_a =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = A; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2_e55
      ; string_number = III
      }
  }
;;

let%expect_test "a_major" =
  let scale = make_major_scale ~from:lower_a in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 26 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III (
       (A2  M2-e55)
       (B2  M3-e55)
       (C#3 A4-e55)))
     (II (
       (D3  0)
       (E3  M2-e55)
       (F#3 M3-e55)
       (G#3 A4-e55)))
     (I (
       (A3  0)
       (B3  M2-e55)
       (C#4 M3-e55)
       (D4  4-e55)
       (E4  5-e55)
       (F#4 M6-e55)
       (G#4 M7-e55)
       (A4  0-1)
       (B4  M2-e55-1)
       (C#5 M3-e55-1)
       (D5  4-e55-1)
       (E5  5-e55-1)
       (F#5 M6-e55-1)
       (G#5 M7-e55-1)
       (A5  0-2)
       (B5  M2-e55-2)
       (C#6 M3-e55-2)
       (D6  4-e55-2)
       (E6  5-e55-2)))) |}];
  ()
;;

let lower_b_flat =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = B; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3_e55
      ; string_number = III
      }
  }
;;

let%expect_test "b_flat_major" =
  let scale = make_major_scale ~from:lower_b_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 26 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III (
       (Bb2 m3-e55)
       (C3  4-e55)))
     (II (
       (D3  0)
       (Eb3 m2-e55)
       (F3  m3-e55)
       (G3  4-e55)))
     (I (
       (A3  0)
       (Bb3 m2-e55)
       (C4  m3-e55)
       (D4  4-e55)
       (Eb4 d5-e55)
       (F4  m6-e55)
       (G4  m7-e55)
       (A4  0-1)
       (Bb4 m2-e55-1)
       (C5  m3-e55-1)
       (D5  4-e55-1)
       (Eb5 d5-e55-1)
       (F5  m6-e55-1)
       (G5  m7-e55-1)
       (A5  0-2)
       (Bb5 m2-e55-2)
       (C6  m3-e55-2)
       (D6  4-e55-2)
       (Eb6 d5-e55-2)
       (F6  m6-e55-2)))) |}];
  ()
;;

let lower_b =
  let t = Lazy.force E55.t in
  { Located_note.note = { letter_name = B; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3_e55
      ; string_number = III
      }
  }
;;

let%expect_test "b_major" =
  let scale = make_major_scale ~from:lower_b in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 25 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III (
       (B2  M3-e55)
       (C#3 A4-e55)))
     (II (
       (D#3 A1-e55)
       (E3  M2-e55)
       (F#3 M3-e55)
       (G#3 A4-e55)))
     (I (
       (A#3 A1-e55)
       (B3  M2-e55)
       (C#4 M3-e55)
       (D#4 A4-e55)
       (E4  5-e55)
       (F#4 M6-e55)
       (G#4 M7-e55)
       (A#4 A1-e55-1)
       (B4  M2-e55-1)
       (C#5 M3-e55-1)
       (D#5 A4-e55-1)
       (E5  5-e55-1)
       (F#5 M6-e55-1)
       (G#5 M7-e55-1)
       (A#5 A1-e55-2)
       (B5  M2-e55-2)
       (C#6 M3-e55-2)
       (D#6 A4-e55-2)
       (E6  5-e55-2)))) |}];
  ()
;;
