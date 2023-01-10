open! Core
open! Fingerboard

let%expect_test "edo53 and octaves" =
  let t = force E53.t in
  let az = Cello.find_fingerboard_position_exn t `P8z_e53 in
  let bz =
    Cello.find_fingerboard_position_exn t `M2z_e53
    |> Fingerboard_position.at_octave ~octave:1
  in
  let i =
    System.acoustic_interval
      t
      ~from:{ fingerboard_position = az; string_number = I }
      ~to_:{ fingerboard_position = bz; string_number = I }
    |> Option.value_exn ~here:[%here]
  in
  print_s [%sexp (i : Acoustic_interval.t)];
  [%expect {| (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)) |}];
  let cents_i = Acoustic_interval.to_cents i in
  let major_ton =
    Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:9
  in
  print_s [%sexp (major_ton : Acoustic_interval.t)];
  [%expect {| (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)) |}];
  let cents_major_ton = Acoustic_interval.to_cents major_ton in
  print_s [%sexp (cents_major_ton : float)];
  [%expect {| 203.77358490566036 |}];
  print_s [%sexp (Float.equal cents_i cents_major_ton : bool)];
  [%expect {| true |}];
  ()
;;

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let make_major_pythagorean_scale ~from =
  let t = force E53.t in
  make_scale t ~characterized_scale:Characterized_scale.major_pythagorean_e53 ~from
;;

let make_major_just_scale ~from =
  let t = force E53.t in
  make_scale t ~characterized_scale:Characterized_scale.major_just_e53 ~from
;;

let lower_c =
  let t = force E53.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let%expect_test "c_major_just" =
  let scale = make_major_just_scale ~from:lower_c in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((C2 0) (D2 M2p-e53) (E2 M3z-e53) (F2 4p-e53)))
     (III ((G2 0) (A2 M2z-e53) (B2 M3z-e53) (C3 4p-e53)))
     (II ((D3 0) (E3 M2z-e53) (F3 m3p-e53) (G3 4p-e53) (A3 5z-e53)))
     (I
      ((B3 M2z-e53) (C4 m3p-e53) (D4 4p-e53) (E4 5z-e53) (F4 m6p-e53)
       (G4 m7p-e53) (A4 8z-e53) (B4 M2z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (E5 5z-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (A5 8z-e53-1) (B5 M2z-e53-2)
       (C6 m3p-e53-2) (D6 4p-e53-2) (E6 5z-e53-2)))) |}];
  ()
;;

let%expect_test "c_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_c in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((C2 0) (D2 M2p-e53) (E2 M3p-e53) (F2 4p-e53)))
     (III ((G2 0) (A2 M2p-e53) (B2 M3p-e53) (C3 4p-e53)))
     (II ((D3 0) (E3 M2p-e53) (F3 m3p-e53) (G3 4p-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C4 m3p-e53) (D4 4p-e53) (E4 5p-e53) (F4 m6p-e53)
       (G4 m7p-e53) (A4 0-1) (B4 M2p-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C6 m3p-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let lower_cp_sharp =
  let t = force E53.t in
  { Located_note.note = { letter_name = C; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "c_sharp_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_cp_sharp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((C#2 m2z-e53) (D#2 m3z-e53) (E#2 4z-e53) (F#2 d5z-e53)))
     (III ((G#2 m2z-e53) (A#2 m3z-e53) (B#2 4z-e53) (C#3 d5z-e53)))
     (II ((D#3 m2z-e53) (E#3 m3z-e53) (F#3 M3p-e53) (G#3 d5z-e53)))
     (I
      ((A#3 m2z-e53) (B#3 m3z-e53) (C#4 M3p-e53) (D#4 d5z-e53) (E#4 m6z-e53)
       (F#4 M6p-e53) (G#4 M7p-e53) (A#4 m2z-e53-1) (B#4 m3z-e53-1)
       (C#5 M3p-e53-1) (D#5 d5z-e53-1) (E#5 m6z-e53-1) (F#5 M6p-e53-1)
       (G#5 M7p-e53-1) (A#5 m2z-e53-2) (B#5 m3z-e53-2) (C#6 M3p-e53-2)
       (D#6 d5z-e53-2) (E#6 m6z-e53-2)))) |}];
  ()
;;

let lower_dp_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = D; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A1z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "d_flat_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_dp_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Db2 A1z-e53) (Eb2 m3p-e53) (F2 4p-e53) (Gb2 A4z-e53)))
     (III ((Ab2 A1z-e53) (Bb2 m3p-e53) (C3 4p-e53) (Db3 A4z-e53)))
     (II ((Eb3 A1z-e53) (F3 m3p-e53) (Gb3 M3z-e53) (Ab3 A4z-e53)))
     (I
      ((Bb3 A1z-e53) (C4 m3p-e53) (Db4 M3z-e53) (Eb4 A4z-e53) (F4 m6p-e53)
       (Gb4 M6z-e53) (Ab4 M7z-e53) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (Db5 M3z-e53-1)
       (Eb5 A4z-e53-1) (F5 m6p-e53-1) (Gb5 M6z-e53-1) (Ab5 M7z-e53-1)
       (Bb5 A1z-e53-2) (C6 m3p-e53-2) (Db6 M3z-e53-2) (Eb6 A4z-e53-2)
       (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_dz_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = D; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "d_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_dz_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 31 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Db2 m2z-e53) (Eb2 m3z-e53) (F2 4p-e53) (Gb2 d5z-e53)))
     (III ((Ab2 m2z-e53) (Bb2 m3p-e53) (C3 4p-e53) (Db3 d5z-e53)))
     (II ((Eb3 m2z-e53) (F3 m3p-e53) (Gb3 M3p-e53) (Ab3 d5z-e53)))
     (I
      ((Bb3 A1z-e53) (C4 m3p-e53) (Db4 M3p-e53) (Eb4 d5z-e53) (F4 m6p-e53)
       (Gb4 M6p-e53) (Ab4 M7p-e53) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (Db5 M3p-e53-1)
       (Eb5 d5z-e53-1) (F5 m6p-e53-1) (Gb5 M6p-e53-1) (Ab5 M7p-e53-1)
       (Bb5 A1z-e53-2) (C6 m3p-e53-2) (Db6 M3p-e53-2) (Eb6 d5z-e53-2)
       (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_d =
  let t = force E53.t in
  { Located_note.note = { letter_name = D; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2p_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "d_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_d in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 30 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((D2 M2p-e53) (E2 M3p-e53) (F#2 d5z-e53)))
     (III ((G2 0) (A2 M2p-e53) (B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D3 0) (E3 M2p-e53) (F#3 M3p-e53) (G3 4p-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C#4 M3p-e53) (D4 4p-e53) (E4 5p-e53) (F#4 M6p-e53)
       (G4 m7p-e53) (A4 0-1) (B4 M2p-e53-1) (C#5 M3p-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F#5 M6p-e53-1) (G5 m7p-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C#6 M3p-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let%expect_test "d_major_just" =
  let scale = make_major_just_scale ~from:lower_d in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 30 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((D2 M2p-e53) (E2 M3p-e53) (F#2 A4z-e53)))
     (III ((G2 0) (A2 M2p-e53) (B2 M3z-e53) (C#3 A4z-e53)))
     (II ((D3 0) (E3 M2p-e53) (F#3 M3z-e53) (G3 4p-e53)))
     (I
      ((A3 0) (B3 M2z-e53) (C#4 M3z-e53) (D4 4p-e53) (E4 5p-e53) (F#4 M6z-e53)
       (G4 m7p-e53) (A4 0-1) (B4 M2z-e53-1) (C#5 M3z-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F#5 M6z-e53-1) (G5 m7p-e53-1) (A5 0-2) (B5 M2z-e53-2)
       (C#6 M3z-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let lower_ez_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "e_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_ez_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 30 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Eb2 m3z-e53) (F2 4z-e53)))
     (III ((G2 0) (Ab2 m2z-e53) (Bb2 m3z-e53) (C3 4p-e53)))
     (II ((D3 0) (Eb3 m2z-e53) (F3 m3z-e53) (G3 4p-e53) (Ab3 d5z-e53)))
     (I
      ((Bb3 m2z-e53) (C4 m3p-e53) (D4 4p-e53) (Eb4 d5z-e53) (F4 m6z-e53)
       (G4 m7p-e53) (Ab4 M7p-e53) (Bb4 m2z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (Eb5 d5z-e53-1) (F5 m6z-e53-1) (G5 m7p-e53-1) (Ab5 M7p-e53-1)
       (Bb5 m2z-e53-2) (C6 m3p-e53-2) (D6 4p-e53-2) (Eb6 d5z-e53-2)
       (F6 m6z-e53-2)))) |}];
  ()
;;

(* When building pythagorean major scale, one has to start from the
   right tonic, otherwise we are falling short of positions pretty
   quickly. Compare below the pythagorean major scale starting from
   two different e flat. *)

let%expect_test "ez_flat_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_ez_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 2 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect {|
    ((IV ((Eb2 m3z-e53) (F2 4z-e53)))) |}];
  ()
;;

let lower_ep_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3p_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "e_flat_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_ep_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 30 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Eb2 m3p-e53) (F2 4p-e53)))
     (III ((G2 0) (Ab2 A1z-e53) (Bb2 m3p-e53) (C3 4p-e53)))
     (II ((D3 0) (Eb3 A1z-e53) (F3 m3p-e53) (G3 4p-e53) (Ab3 A4z-e53)))
     (I
      ((Bb3 A1z-e53) (C4 m3p-e53) (D4 4p-e53) (Eb4 A4z-e53) (F4 m6p-e53)
       (G4 m7p-e53) (Ab4 M7z-e53) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (Eb5 A4z-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (Ab5 M7z-e53-1)
       (Bb5 A1z-e53-2) (C6 m3p-e53-2) (D6 4p-e53-2) (Eb6 A4z-e53-2)
       (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_e =
  let t = force E53.t in
  { Located_note.note = { letter_name = E; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3p_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "e_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_e in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 29 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((E2 M3p-e53) (F#2 d5z-e53)))
     (III ((G#2 m2z-e53) (A2 M2p-e53) (B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D#3 m2z-e53) (E3 M2p-e53) (F#3 M3p-e53) (G#3 d5z-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C#4 M3p-e53) (D#4 d5z-e53) (E4 5p-e53) (F#4 M6p-e53)
       (G#4 M7p-e53) (A4 0-1) (B4 M2p-e53-1) (C#5 M3p-e53-1) (D#5 d5z-e53-1)
       (E5 5p-e53-1) (F#5 M6p-e53-1) (G#5 M7p-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C#6 M3p-e53-2) (D#6 d5z-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let%expect_test "e_major_just" =
  let scale = make_major_just_scale ~from:lower_e in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 29 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((E2 M3p-e53) (F#2 d5z-e53)))
     (III ((G#2 A1z-e53) (A2 M2p-e53) (B2 M3p-e53) (C#3 A4z-e53)))
     (II ((D#3 A1z-e53) (E3 M2p-e53) (F#3 M3p-e53) (G#3 A4z-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C#4 M3z-e53) (D#4 A4z-e53) (E4 5p-e53) (F#4 M6p-e53)
       (G#4 M7z-e53) (A4 0-1) (B4 M2p-e53-1) (C#5 M3z-e53-1) (D#5 A4z-e53-1)
       (E5 5p-e53-1) (F#5 M6p-e53-1) (G#5 M7z-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C#6 M3z-e53-2) (D#6 A4z-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let lower_fp =
  let t = force E53.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4p_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "fp_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_fp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((F2 4p-e53))) (III ((G2 0) (A2 M2p-e53) (Bb2 m3p-e53) (C3 4p-e53)))
     (II ((D3 0) (E3 M2p-e53) (F3 m3p-e53) (G3 4p-e53)))
     (I
      ((A3 0) (Bb3 A1z-e53) (C4 m3p-e53) (D4 4p-e53) (E4 5p-e53) (F4 m6p-e53)
       (G4 m7p-e53) (A4 0-1) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (A5 0-2) (Bb5 A1z-e53-2)
       (C6 m3p-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let%expect_test "fp_major_just" =
  let scale = make_major_just_scale ~from:lower_fp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 12 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((F2 4p-e53)))
     (III ((G2 0) (A2 M2z-e53) (Bb2 m3p-e53) (C3 4p-e53) (D3 5z-e53)))
     (II ((E3 M2z-e53) (F3 m3p-e53) (G3 4p-e53) (A3 5z-e53)))
     (I ((Bb3 A1z-e53) (C4 m3p-e53)))) |}];
  ()
;;

let lower_fz =
  let t = force E53.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "fz_major_just" =
  let scale = make_major_just_scale ~from:lower_fz in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 1 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect {|
    ((IV ((F2 4z-e53)))) |}];
  ()
;;

let lower_fp_sharp =
  let t = force E53.t in
  { Located_note.note = { letter_name = F; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `d5z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "f_sharp_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_fp_sharp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((F#2 d5z-e53)))
     (III ((G#2 m2z-e53) (A#2 m3z-e53) (B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D#3 m2z-e53) (E#3 m3z-e53) (F#3 M3p-e53) (G#3 d5z-e53)))
     (I
      ((A#3 m2z-e53) (B3 M2p-e53) (C#4 M3p-e53) (D#4 d5z-e53) (E#4 m6z-e53)
       (F#4 M6p-e53) (G#4 M7p-e53) (A#4 m2z-e53-1) (B4 M2p-e53-1) (C#5 M3p-e53-1)
       (D#5 d5z-e53-1) (E#5 m6z-e53-1) (F#5 M6p-e53-1) (G#5 M7p-e53-1)
       (A#5 m2z-e53-2) (B5 M2p-e53-2) (C#6 M3p-e53-2) (D#6 d5z-e53-2)
       (E#6 m6z-e53-2)))) |}];
  ()
;;

let%expect_test "f_sharp_major_just" =
  let scale = make_major_just_scale ~from:lower_fp_sharp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((F#2 d5z-e53)))
     (III ((G#2 m2z-e53) (A#2 m3p-e53) (B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D#3 A1z-e53) (E#3 m3p-e53) (F#3 M3p-e53) (G#3 d5z-e53)))
     (I
      ((A#3 A1z-e53) (B3 M2p-e53) (C#4 M3p-e53) (D#4 A4z-e53) (E#4 m6p-e53)
       (F#4 M6p-e53) (G#4 M7p-e53) (A#4 A1z-e53-1) (B4 M2p-e53-1) (C#5 M3p-e53-1)
       (D#5 A4z-e53-1) (E#5 m6p-e53-1) (F#5 M6p-e53-1) (G#5 M7p-e53-1)
       (A#5 A1z-e53-2) (B5 M2p-e53-2) (C#6 M3p-e53-2) (D#6 A4z-e53-2)
       (E#6 m6p-e53-2)))) |}];
  ()
;;

let lower_gp_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = G; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A4z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "g_flat_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_gp_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Gb2 A4z-e53)))
     (III ((Ab2 A1z-e53) (Bb2 m3p-e53) (Cb3 M3z-e53) (Db3 A4z-e53)))
     (II ((Eb3 A1z-e53) (F3 m3p-e53) (Gb3 M3z-e53) (Ab3 A4z-e53)))
     (I
      ((Bb3 A1z-e53) (Cb4 M2z-e53) (Db4 M3z-e53) (Eb4 A4z-e53) (F4 m6p-e53)
       (Gb4 M6z-e53) (Ab4 M7z-e53) (Bb4 A1z-e53-1) (Cb5 M2z-e53-1)
       (Db5 M3z-e53-1) (Eb5 A4z-e53-1) (F5 m6p-e53-1) (Gb5 M6z-e53-1)
       (Ab5 M7z-e53-1) (Bb5 A1z-e53-2) (Cb6 M2z-e53-2) (Db6 M3z-e53-2)
       (Eb6 A4z-e53-2) (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_gz_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = G; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `d5z_e53
      ; string_number = IV
      }
  }
;;

let%expect_test "g_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_gz_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 28 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((IV ((Gb2 d5z-e53)))
     (III ((Ab2 m2z-e53) (Bb2 m3p-e53) (Cb3 M3p-e53) (Db3 d5z-e53)))
     (II ((Eb3 A1z-e53) (F3 m3p-e53) (Gb3 M3p-e53) (Ab3 d5z-e53)))
     (I
      ((Bb3 A1z-e53) (Cb4 M2p-e53) (Db4 M3p-e53) (Eb4 A4z-e53) (F4 m6p-e53)
       (Gb4 M6p-e53) (Ab4 M7p-e53) (Bb4 A1z-e53-1) (Cb5 M2p-e53-1)
       (Db5 M3p-e53-1) (Eb5 A4z-e53-1) (F5 m6p-e53-1) (Gb5 M6p-e53-1)
       (Ab5 M7p-e53-1) (Bb5 A1z-e53-2) (Cb6 M2p-e53-2) (Db6 M3p-e53-2)
       (Eb6 A4z-e53-2) (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_g =
  let t = force E53.t in
  System.open_string t III |> Option.value_exn ~here:[%here]
;;

let%expect_test "g_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_g in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((G2 0) (A2 M2p-e53) (B2 M3p-e53) (C3 4p-e53)))
     (II ((D3 0) (E3 M2p-e53) (F#3 M3p-e53) (G3 4p-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C4 m3p-e53) (D4 4p-e53) (E4 5p-e53) (F#4 M6p-e53)
       (G4 m7p-e53) (A4 0-1) (B4 M2p-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F#5 M6p-e53-1) (G5 m7p-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C6 m3p-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let%expect_test "g_major_just" =
  let scale = make_major_just_scale ~from:lower_g in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((G2 0) (A2 M2p-e53) (B2 M3z-e53) (C3 4p-e53)))
     (II ((D3 0) (E3 M2z-e53) (F#3 M3z-e53) (G3 4p-e53)))
     (I
      ((A3 0) (B3 M2z-e53) (C4 m3p-e53) (D4 4p-e53) (E4 5z-e53) (F#4 M6z-e53)
       (G4 m7p-e53) (A4 0-1) (B4 M2z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (E5 5z-e53-1) (F#5 M6z-e53-1) (G5 m7p-e53-1) (A5 0-2) (B5 M2z-e53-2)
       (C6 m3p-e53-2) (D6 4p-e53-2) (E6 5z-e53-2)))) |}];
  ()
;;

let lower_ap_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = A; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A1z_e53
      ; string_number = III
      }
  }
;;

let%expect_test "a_flat_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_ap_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((Ab2 A1z-e53) (Bb2 m3p-e53) (C3 4p-e53) (Db3 A4z-e53)))
     (II ((Eb3 A1z-e53) (F3 m3p-e53) (G3 4p-e53) (Ab3 A4z-e53)))
     (I
      ((Bb3 A1z-e53) (C4 m3p-e53) (Db4 M3z-e53) (Eb4 A4z-e53) (F4 m6p-e53)
       (G4 m7p-e53) (Ab4 M7z-e53) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (Db5 M3z-e53-1)
       (Eb5 A4z-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (Ab5 M7z-e53-1)
       (Bb5 A1z-e53-2) (C6 m3p-e53-2) (Db6 M3z-e53-2) (Eb6 A4z-e53-2)
       (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_az_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = A; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2z_e53
      ; string_number = III
      }
  }
;;

let%expect_test "a_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_az_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 27 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((Ab2 m2z-e53) (Bb2 m3z-e53) (C3 4p-e53) (Db3 d5z-e53)))
     (II ((Eb3 m2z-e53) (F3 m3p-e53) (G3 4p-e53) (Ab3 d5z-e53)))
     (I
      ((Bb3 m2z-e53) (C4 m3p-e53) (Db4 M3p-e53) (Eb4 d5z-e53) (F4 m6p-e53)
       (G4 m7p-e53) (Ab4 M7p-e53) (Bb4 m2z-e53-1) (C5 m3p-e53-1) (Db5 M3p-e53-1)
       (Eb5 d5z-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (Ab5 M7p-e53-1)
       (Bb5 m2z-e53-2) (C6 m3p-e53-2) (Db6 M3p-e53-2) (Eb6 d5z-e53-2)
       (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_a =
  let t = force E53.t in
  { Located_note.note = { letter_name = A; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2p_e53
      ; string_number = III
      }
  }
;;

let%expect_test "a_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_a in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 26 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((A2 M2p-e53) (B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D3 0) (E3 M2p-e53) (F#3 M3p-e53) (G#3 d5z-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C#4 M3p-e53) (D4 4p-e53) (E4 5p-e53) (F#4 M6p-e53)
       (G#4 M7p-e53) (A4 0-1) (B4 M2p-e53-1) (C#5 M3p-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F#5 M6p-e53-1) (G#5 M7p-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C#6 M3p-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let%expect_test "a_major_just" =
  let scale = make_major_just_scale ~from:lower_a in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 26 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((A2 M2p-e53) (B2 M3p-e53) (C#3 A4z-e53)))
     (II ((D3 0) (E3 M2p-e53) (F#3 M3z-e53) (G#3 A4z-e53)))
     (I
      ((A3 0) (B3 M2p-e53) (C#4 M3z-e53) (D4 4p-e53) (E4 5p-e53) (F#4 M6z-e53)
       (G#4 M7z-e53) (A4 0-1) (B4 M2p-e53-1) (C#5 M3z-e53-1) (D5 4p-e53-1)
       (E5 5p-e53-1) (F#5 M6z-e53-1) (G#5 M7z-e53-1) (A5 0-2) (B5 M2p-e53-2)
       (C#6 M3z-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
  ()
;;

let lower_bp_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = B; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3p_e53
      ; string_number = III
      }
  }
;;

let%expect_test "b_flat_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_bp_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 26 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((Bb2 m3p-e53) (C3 4p-e53)))
     (II ((D3 0) (Eb3 A1z-e53) (F3 m3p-e53) (G3 4p-e53)))
     (I
      ((A3 0) (Bb3 A1z-e53) (C4 m3p-e53) (D4 4p-e53) (Eb4 A4z-e53) (F4 m6p-e53)
       (G4 m7p-e53) (A4 0-1) (Bb4 A1z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
       (Eb5 A4z-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (A5 0-2) (Bb5 A1z-e53-2)
       (C6 m3p-e53-2) (D6 4p-e53-2) (Eb6 A4z-e53-2) (F6 m6p-e53-2)))) |}];
  ()
;;

let lower_bz_flat =
  let t = force E53.t in
  { Located_note.note = { letter_name = B; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3z_e53
      ; string_number = III
      }
  }
;;

let%expect_test "b_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_bz_flat in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 26 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((Bb2 m3z-e53) (C3 4z-e53)))
     (II ((D3 0) (Eb3 m2z-e53) (F3 m3z-e53) (G3 4p-e53)))
     (I
      ((A3 0) (Bb3 m2z-e53) (C4 m3z-e53) (D4 4p-e53) (Eb4 d5z-e53) (F4 m6z-e53)
       (G4 m7p-e53) (A4 0-1) (Bb4 m2z-e53-1) (C5 m3z-e53-1) (D5 4p-e53-1)
       (Eb5 d5z-e53-1) (F5 m6z-e53-1) (G5 m7p-e53-1) (A5 0-2) (Bb5 m2z-e53-2)
       (C6 m3z-e53-2) (D6 4p-e53-2) (Eb6 d5z-e53-2) (F6 m6z-e53-2)))) |}];
  ()
;;

let lower_bp =
  let t = force E53.t in
  { Located_note.note = { letter_name = B; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3p_e53
      ; string_number = III
      }
  }
;;

let%expect_test "b_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_bp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 25 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D#3 m2z-e53) (E3 M2p-e53) (F#3 M3p-e53) (G#3 d5z-e53)))
     (I
      ((A#3 m2z-e53) (B3 M2p-e53) (C#4 M3p-e53) (D#4 d5z-e53) (E4 5p-e53)
       (F#4 M6p-e53) (G#4 M7p-e53) (A#4 m2z-e53-1) (B4 M2p-e53-1) (C#5 M3p-e53-1)
       (D#5 d5z-e53-1) (E5 5p-e53-1) (F#5 M6p-e53-1) (G#5 M7p-e53-1)
       (A#5 m2z-e53-2) (B5 M2p-e53-2) (C#6 M3p-e53-2) (D#6 d5z-e53-2)
       (E6 5p-e53-2)))) |}];
  ()
;;

let%expect_test "b_major_just" =
  let scale = make_major_just_scale ~from:lower_bp in
  print_s [%sexp (List.length scale : int)];
  [%expect {| 25 |}];
  print_s [%sexp (scale |> Located_note.to_scale_abbrev : Located_note.Scale_abbrev.t)];
  [%expect
    {|
    ((III ((B2 M3p-e53) (C#3 d5z-e53)))
     (II ((D#3 A1z-e53) (E3 M2p-e53) (F#3 M3p-e53) (G#3 A4z-e53)))
     (I
      ((A#3 A1z-e53) (B3 M2p-e53) (C#4 M3p-e53) (D#4 A4z-e53) (E4 5p-e53)
       (F#4 M6p-e53) (G#4 M7z-e53) (A#4 A1z-e53-1) (B4 M2p-e53-1) (C#5 M3p-e53-1)
       (D#5 A4z-e53-1) (E5 5p-e53-1) (F#5 M6p-e53-1) (G#5 M7z-e53-1)
       (A#5 A1z-e53-2) (B5 M2p-e53-2) (C#6 M3p-e53-2) (D#6 A4z-e53-2)
       (E6 5p-e53-2)))) |}];
  ()
;;
