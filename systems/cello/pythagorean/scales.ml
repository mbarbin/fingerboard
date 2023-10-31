open! Fingerboard

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  let t = force Pythagorean.t in
  make_scale t ~characterized_scale:Characterized_scale.major_pythagorean ~from
;;

let lower_c =
  let t = force Pythagorean.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let%expect_test "c_major" =
  let scale = make_major_scale ~from:lower_c in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((C2 0) (D2 M2p) (E2 M3p) (F2 4p)))
     (III ((G2 0) (A2 M2p) (B2 M3p) (C3 4p)))
     (II ((D3 0) (E3 M2p) (F3 m3p) (G3 4p)))
     (I
      ((A3 0) (B3 M2p) (C4 m3p) (D4 4p) (E4 5p) (F4 m6p) (G4 m7p) (A4 0-1)
       (B4 M2p-1) (C5 m3p-1) (D5 4p-1) (E5 5p-1) (F5 m6p-1) (G5 m7p-1) (A5 0-2)
       (B5 M2p-2) (C6 m3p-2) (D6 4p-2) (E6 5p-2)))) |}];
  ()
;;

let lower_c_sharp =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = C; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A1p
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
    ((IV ((C#2 A1p) (D#2 A2p) (E#2 A3p) (F#2 A4p)))
     (III ((G#2 A1p) (A#2 A2p) (B#2 A3p) (C#3 A4p)))
     (II ((D#3 A1p) (E#3 A2p) (F#3 M3p) (G#3 A4p)))
     (I
      ((A#3 A1p) (B#3 A2p) (C#4 M3p) (D#4 A4p) (E#4 A5p) (F#4 M6p) (G#4 M7p)
       (A#4 A1p-1) (B#4 A2p-1) (C#5 M3p-1) (D#5 A4p-1) (E#5 A5p-1) (F#5 M6p-1)
       (G#5 M7p-1) (A#5 A1p-2) (B#5 A2p-2) (C#6 M3p-2) (D#6 A4p-2) (E#6 A5p-2)))) |}];
  ()
;;

let lower_d_flat =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = D; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2p
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
    ((IV ((Db2 m2p) (Eb2 m3p) (F2 4p) (Gb2 d5p)))
     (III ((Ab2 m2p) (Bb2 m3p) (C3 4p) (Db3 d5p)))
     (II ((Eb3 m2p) (F3 m3p) (Gb3 d4p) (Ab3 d5p)))
     (I
      ((Bb3 m2p) (C4 m3p) (Db4 d4p) (Eb4 d5p) (F4 m6p) (Gb4 d7p) (Ab4 d8p)
       (Bb4 m2p-1) (C5 m3p-1) (Db5 d4p-1) (Eb5 d5p-1) (F5 m6p-1) (Gb5 d7p-1)
       (Ab5 d8p-1) (Bb5 m2p-2) (C6 m3p-2) (Db6 d4p-2) (Eb6 d5p-2) (F6 m6p-2)))) |}];
  ()
;;

let lower_d =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = D; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2p
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
    ((IV ((D2 M2p) (E2 M3p) (F#2 A4p)))
     (III ((G2 0) (A2 M2p) (B2 M3p) (C#3 A4p)))
     (II ((D3 0) (E3 M2p) (F#3 M3p) (G3 4p)))
     (I
      ((A3 0) (B3 M2p) (C#4 M3p) (D4 4p) (E4 5p) (F#4 M6p) (G4 m7p) (A4 0-1)
       (B4 M2p-1) (C#5 M3p-1) (D5 4p-1) (E5 5p-1) (F#5 M6p-1) (G5 m7p-1)
       (A5 0-2) (B5 M2p-2) (C#6 M3p-2) (D6 4p-2) (E6 5p-2)))) |}];
  ()
;;

let lower_e_flat =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3p
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
    ((IV ((Eb2 m3p) (F2 4p))) (III ((G2 0) (Ab2 m2p) (Bb2 m3p) (C3 4p)))
     (II ((D3 0) (Eb3 m2p) (F3 m3p) (G3 4p) (Ab3 d5p)))
     (I
      ((Bb3 m2p) (C4 m3p) (D4 4p) (Eb4 d5p) (F4 m6p) (G4 m7p) (Ab4 d8p)
       (Bb4 m2p-1) (C5 m3p-1) (D5 4p-1) (Eb5 d5p-1) (F5 m6p-1) (G5 m7p-1)
       (Ab5 d8p-1) (Bb5 m2p-2) (C6 m3p-2) (D6 4p-2) (Eb6 d5p-2) (F6 m6p-2)))) |}];
  ()
;;

let lower_e =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = E; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3p
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
    ((IV ((E2 M3p) (F#2 A4p))) (III ((G#2 A1p) (A2 M2p) (B2 M3p) (C#3 A4p)))
     (II ((D#3 A1p) (E3 M2p) (F#3 M3p) (G#3 A4p)))
     (I
      ((A3 0) (B3 M2p) (C#4 M3p) (D#4 A4p) (E4 5p) (F#4 M6p) (G#4 M7p) (A4 0-1)
       (B4 M2p-1) (C#5 M3p-1) (D#5 A4p-1) (E5 5p-1) (F#5 M6p-1) (G#5 M7p-1)
       (A5 0-2) (B5 M2p-2) (C#6 M3p-2) (D#6 A4p-2) (E6 5p-2)))) |}];
  ()
;;

let lower_f =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4p
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
    ((IV ((F2 4p))) (III ((G2 0) (A2 M2p) (Bb2 m3p) (C3 4p)))
     (II ((D3 0) (E3 M2p) (F3 m3p) (G3 4p)))
     (I
      ((A3 0) (Bb3 m2p) (C4 m3p) (D4 4p) (E4 5p) (F4 m6p) (G4 m7p) (A4 0-1)
       (Bb4 m2p-1) (C5 m3p-1) (D5 4p-1) (E5 5p-1) (F5 m6p-1) (G5 m7p-1) (A5 0-2)
       (Bb5 m2p-2) (C6 m3p-2) (D6 4p-2) (E6 5p-2)))) |}];
  ()
;;

let lower_f_sharp =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = F; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A4p
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
    ((IV ((F#2 A4p))) (III ((G#2 A1p) (A#2 A2p) (B2 M3p) (C#3 A4p)))
     (II ((D#3 A1p) (E#3 A2p) (F#3 M3p) (G#3 A4p)))
     (I
      ((A#3 A1p) (B3 M2p) (C#4 M3p) (D#4 A4p) (E#4 A5p) (F#4 M6p) (G#4 M7p)
       (A#4 A1p-1) (B4 M2p-1) (C#5 M3p-1) (D#5 A4p-1) (E#5 A5p-1) (F#5 M6p-1)
       (G#5 M7p-1) (A#5 A1p-2) (B5 M2p-2) (C#6 M3p-2) (D#6 A4p-2) (E#6 A5p-2)))) |}];
  ()
;;

let lower_g_flat =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = G; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `d5p
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
    ((IV ((Gb2 d5p))) (III ((Ab2 m2p) (Bb2 m3p) (Cb3 d4p) (Db3 d5p)))
     (II ((Eb3 m2p) (F3 m3p) (Gb3 d4p) (Ab3 d5p)))
     (I
      ((Bb3 m2p) (Cb4 d3p) (Db4 d4p) (Eb4 d5p) (F4 m6p) (Gb4 d7p) (Ab4 d8p)
       (Bb4 m2p-1) (Cb5 d3p-1) (Db5 d4p-1) (Eb5 d5p-1) (F5 m6p-1) (Gb5 d7p-1)
       (Ab5 d8p-1) (Bb5 m2p-2) (Cb6 d3p-2) (Db6 d4p-2) (Eb6 d5p-2) (F6 m6p-2)))) |}];
  ()
;;

let lower_g =
  let t = force Pythagorean.t in
  System.open_string t III |> Option.value_exn ~here:[%here]
;;

let%expect_test "g_major" =
  let scale = make_major_scale ~from:lower_g in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((G2 0) (A2 M2p) (B2 M3p) (C3 4p)))
     (II ((D3 0) (E3 M2p) (F#3 M3p) (G3 4p)))
     (I
      ((A3 0) (B3 M2p) (C4 m3p) (D4 4p) (E4 5p) (F#4 M6p) (G4 m7p) (A4 0-1)
       (B4 M2p-1) (C5 m3p-1) (D5 4p-1) (E5 5p-1) (F#5 M6p-1) (G5 m7p-1) (A5 0-2)
       (B5 M2p-2) (C6 m3p-2) (D6 4p-2) (E6 5p-2)))) |}];
  ()
;;

let lower_a_flat =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = A; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2p
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
    ((III ((Ab2 m2p) (Bb2 m3p) (C3 4p) (Db3 d5p)))
     (II ((Eb3 m2p) (F3 m3p) (G3 4p) (Ab3 d5p)))
     (I
      ((Bb3 m2p) (C4 m3p) (Db4 d4p) (Eb4 d5p) (F4 m6p) (G4 m7p) (Ab4 d8p)
       (Bb4 m2p-1) (C5 m3p-1) (Db5 d4p-1) (Eb5 d5p-1) (F5 m6p-1) (G5 m7p-1)
       (Ab5 d8p-1) (Bb5 m2p-2) (C6 m3p-2) (Db6 d4p-2) (Eb6 d5p-2) (F6 m6p-2)))) |}];
  ()
;;

let lower_a =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = A; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2p
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
    ((III ((A2 M2p) (B2 M3p) (C#3 A4p)))
     (II ((D3 0) (E3 M2p) (F#3 M3p) (G#3 A4p)))
     (I
      ((A3 0) (B3 M2p) (C#4 M3p) (D4 4p) (E4 5p) (F#4 M6p) (G#4 M7p) (A4 0-1)
       (B4 M2p-1) (C#5 M3p-1) (D5 4p-1) (E5 5p-1) (F#5 M6p-1) (G#5 M7p-1)
       (A5 0-2) (B5 M2p-2) (C#6 M3p-2) (D6 4p-2) (E6 5p-2)))) |}];
  ()
;;

let lower_b_flat =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = B; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3p
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
    ((III ((Bb2 m3p) (C3 4p))) (II ((D3 0) (Eb3 m2p) (F3 m3p) (G3 4p)))
     (I
      ((A3 0) (Bb3 m2p) (C4 m3p) (D4 4p) (Eb4 d5p) (F4 m6p) (G4 m7p) (A4 0-1)
       (Bb4 m2p-1) (C5 m3p-1) (D5 4p-1) (Eb5 d5p-1) (F5 m6p-1) (G5 m7p-1)
       (A5 0-2) (Bb5 m2p-2) (C6 m3p-2) (D6 4p-2) (Eb6 d5p-2) (F6 m6p-2)))) |}];
  ()
;;

let lower_b =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = B; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3p
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
    ((III ((B2 M3p) (C#3 A4p))) (II ((D#3 A1p) (E3 M2p) (F#3 M3p) (G#3 A4p)))
     (I
      ((A#3 A1p) (B3 M2p) (C#4 M3p) (D#4 A4p) (E4 5p) (F#4 M6p) (G#4 M7p)
       (A#4 A1p-1) (B4 M2p-1) (C#5 M3p-1) (D#5 A4p-1) (E5 5p-1) (F#5 M6p-1)
       (G#5 M7p-1) (A#5 A1p-2) (B5 M2p-2) (C#6 M3p-2) (D#6 A4p-2) (E6 5p-2)))) |}];
  ()
;;
