open! Core
open! Cemper

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

let make_major_just_scale ~from =
  let t = force E53.t in
  make_scale t ~characterized_scale:Characterized_scale.major_just_e53 ~from
;;

let make_major_pythagorean_scale ~from =
  let t = force E53.t in
  make_scale t ~characterized_scale:Characterized_scale.major_pythagorean_e53 ~from
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
    ((IV ((C2 0-0) (D2 M2p-e53-0) (E2 M3z-e53-0) (F2 4p-e53-0)))
     (III ((G2 0-0) (A2 M2z-e53-0) (B2 M3z-e53-0) (C3 4p-e53-0)))
     (II ((D3 0-0) (E3 M2z-e53-0) (F3 m3p-e53-0) (G3 4p-e53-0) (A3 5z-e53-0)))
     (I
      ((B3 M2z-e53-0) (C4 m3p-e53-0) (D4 4p-e53-0) (E4 5z-e53-0) (F4 m6p-e53-0)
       (G4 m7p-e53-0) (A4 8z-e53-0) (B4 M2z-e53-1) (C5 m3p-e53-1) (D5 4p-e53-1)
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
    ((IV ((C2 0-0) (D2 M2p-e53-0) (E2 M3p-e53-0) (F2 4p-e53-0)))
     (III ((G2 0-0) (A2 M2p-e53-0) (B2 M3p-e53-0) (C3 4p-e53-0)))
     (II ((D3 0-0) (E3 M2p-e53-0) (F3 m3p-e53-0) (G3 4p-e53-0)))
     (I
      ((A3 0-0) (B3 M2p-e53-0) (C4 m3p-e53-0) (D4 4p-e53-0) (E4 5p-e53-0)
       (F4 m6p-e53-0) (G4 m7p-e53-0) (A4 0-1) (B4 M2p-e53-1) (C5 m3p-e53-1)
       (D5 4p-e53-1) (E5 5p-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1) (A5 0-2)
       (B5 M2p-e53-2) (C6 m3p-e53-2) (D6 4p-e53-2) (E6 5p-e53-2)))) |}];
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
    ((IV ((Eb2 m3z-e53-0) (F2 4z-e53-0)))
     (III ((G2 0-0) (Ab2 m2z-e53-0) (Bb2 m3z-e53-0) (C3 4p-e53-0)))
     (II ((D3 0-0) (Eb3 m2z-e53-0) (F3 m3z-e53-0) (G3 4p-e53-0) (Ab3 d5z-e53-0)))
     (I
      ((Bb3 m2z-e53-0) (C4 m3p-e53-0) (D4 4p-e53-0) (Eb4 d5z-e53-0)
       (F4 m6z-e53-0) (G4 m7p-e53-0) (Ab4 M7p-e53-0) (Bb4 m2z-e53-1)
       (C5 m3p-e53-1) (D5 4p-e53-1) (Eb5 d5z-e53-1) (F5 m6z-e53-1) (G5 m7p-e53-1)
       (Ab5 M7p-e53-1) (Bb5 m2z-e53-2) (C6 m3p-e53-2) (D6 4p-e53-2)
       (Eb6 d5z-e53-2) (F6 m6z-e53-2)))) |}];
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
    ((IV ((Eb2 m3z-e53-0) (F2 4z-e53-0)))) |}];
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
    ((IV ((Eb2 m3p-e53-0) (F2 4p-e53-0)))
     (III ((G2 0-0) (Ab2 A1z-e53-0) (Bb2 m3p-e53-0) (C3 4p-e53-0)))
     (II ((D3 0-0) (Eb3 A1z-e53-0) (F3 m3p-e53-0) (G3 4p-e53-0) (Ab3 A4z-e53-0)))
     (I
      ((Bb3 A1z-e53-0) (C4 m3p-e53-0) (D4 4p-e53-0) (Eb4 A4z-e53-0)
       (F4 m6p-e53-0) (G4 m7p-e53-0) (Ab4 M7z-e53-0) (Bb4 A1z-e53-1)
       (C5 m3p-e53-1) (D5 4p-e53-1) (Eb5 A4z-e53-1) (F5 m6p-e53-1) (G5 m7p-e53-1)
       (Ab5 M7z-e53-1) (Bb5 A1z-e53-2) (C6 m3p-e53-2) (D6 4p-e53-2)
       (Eb6 A4z-e53-2) (F6 m6p-e53-2)))) |}];
  ()
;;
