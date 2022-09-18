open! Core
open! Cemper

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
