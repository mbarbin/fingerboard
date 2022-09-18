open! Core
open! Cemper

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let lower_c =
  let t = force Just.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let c_just_scale =
  let t = force Just.t in
  make_scale t ~characterized_scale:Characterized_scale.major_just ~from:lower_c
;;

let%expect_test "c_just_scale" =
  print_s [%sexp (c_just_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name C) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number IV))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number IV))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -2)) ((prime 5) (exponent 1)))))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number III))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
             ((prime 5) (exponent 1)))))))
        (string_number III))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -2)) ((prime 5) (exponent 1)))))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number II))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
             ((prime 5) (exponent 1)))))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
             ((prime 5) (exponent 1)))))))
        (string_number II))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 8z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -4))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 8z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -4))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
             ((prime 5) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
             ((prime 5) (exponent 1)))))))
        (string_number I))))) |}];
  ()
;;

let%expect_test "c_just_scale" =
  print_s
    [%sexp
      (c_just_scale |> List.map ~f:Located_note.to_abbrev : Located_note.Abbrev.t list)];
  [%expect
    {|
    ((C2 0-0 IV) (D2 M2p-0 IV) (E2 M3z-0 IV) (F2 4p-0 IV) (G2 0-0 III)
     (A2 M2z-0 III) (B2 M3z-0 III) (C3 4p-0 III) (D3 0-0 II) (E3 M2z-0 II)
     (F3 m3p-0 II) (G3 4p-0 II) (A3 5z-0 II) (B3 M2z-0 I) (C4 m3p-0 I)
     (D4 4p-0 I) (E4 5z-0 I) (F4 m6p-0 I) (G4 m7p-0 I) (A4 8z-0 I) (B4 M2z-1 I)
     (C5 m3p-1 I) (D5 4p-1 I) (E5 5z-1 I) (F5 m6p-1 I) (G5 m7p-1 I) (A5 8z-1 I)
     (B5 M2z-2 I) (C6 m3p-2 I) (D6 4p-2 I) (E6 5z-2 I)) |}];
  print_s
    [%sexp (c_just_scale |> Located_note.to_scales_abbrev : Located_note.Scales_abbrev.t)];
  [%expect
    {|
    ((IV ((C2 0-0) (D2 M2p-0) (E2 M3z-0) (F2 4p-0)))
     (III ((G2 0-0) (A2 M2z-0) (B2 M3z-0) (C3 4p-0)))
     (II ((D3 0-0) (E3 M2z-0) (F3 m3p-0) (G3 4p-0) (A3 5z-0)))
     (I
      ((B3 M2z-0) (C4 m3p-0) (D4 4p-0) (E4 5z-0) (F4 m6p-0) (G4 m7p-0) (A4 8z-0)
       (B4 M2z-1) (C5 m3p-1) (D5 4p-1) (E5 5z-1) (F5 m6p-1) (G5 m7p-1) (A5 8z-1)
       (B5 M2z-2) (C6 m3p-2) (D6 4p-2) (E6 5z-2)))) |}];
  ()
;;

let c_pythagorean_scale =
  let t = force Just.t in
  make_scale t ~characterized_scale:Characterized_scale.major_pythagorean ~from:lower_c
;;

let%expect_test "c_pythagorean_scale" =
  print_s [%sexp (c_pythagorean_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name C) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number IV))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number IV))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -6)) ((prime 3) (exponent 4)))))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number III))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number III))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -6)) ((prime 3) (exponent 4)))))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number II))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -3)) ((prime 3) (exponent 2)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))
        (string_number I))))) |}];
  ()
;;

let lower_ez_flat =
  let t = force Just.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3z
      ; string_number = IV
      }
  }
;;

let e_flat_just_major_scale =
  let t = force Just.t in
  make_scale t ~characterized_scale:Characterized_scale.major_just ~from:lower_ez_flat
;;

let%expect_test "e_flat_just_major_scale" =
  print_s [%sexp (e_flat_just_major_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name E) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
             ((prime 5) (exponent -1)))))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -2)) ((prime 3) (exponent 3))
             ((prime 5) (exponent -1)))))))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number III))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
             ((prime 5) (exponent -1)))))))
        (string_number III))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
             ((prime 5) (exponent -1)))))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number II))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
             ((prime 5) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
             ((prime 5) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
             ((prime 5) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name d8z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -3))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name d8z) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -3))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2z) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5z) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
             ((prime 5) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6z) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 3)) ((prime 5) (exponent -1)))))))
        (string_number I))))) |}];
  ()
;;

let ez_flat_pythagorean_major_scale =
  let t = force Just.t in
  make_scale
    t
    ~characterized_scale:Characterized_scale.major_pythagorean
    ~from:lower_ez_flat
;;

let%expect_test "ez_flat_pythagorean_major_scale" =
  print_s [%sexp (ez_flat_pythagorean_major_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name E) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
             ((prime 5) (exponent -1)))))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4z) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent -2)) ((prime 3) (exponent 3))
             ((prime 5) (exponent -1)))))))
        (string_number IV))))) |}];
  ()
;;

let lower_ep_flat =
  let t = force Just.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3p
      ; string_number = IV
      }
  }
;;

let ep_flat_pythagorean_major_scale =
  let t = force Just.t in
  make_scale
    t
    ~characterized_scale:Characterized_scale.major_pythagorean
    ~from:lower_ep_flat
;;

let%expect_test "ep_flat_pythagorean_major_scale" =
  print_s [%sexp (ep_flat_pythagorean_major_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name E) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number III))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -5)))))))
        (string_number III))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number II))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -5)))))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number II))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 10)) ((prime 3) (exponent -6)))))))
        (string_number II))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -5)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 10)) ((prime 3) (exponent -6)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name d8p) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 12)) ((prime 3) (exponent -7)))))))
        (string_number I))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -5)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 10)) ((prime 3) (exponent -6)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 4)) ((prime 3) (exponent -2)))))))
        (string_number I))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name d8p) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 12)) ((prime 3) (exponent -7)))))))
        (string_number I))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 8)) ((prime 3) (exponent -5)))))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 5)) ((prime 3) (exponent -3)))))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name d5p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 10)) ((prime 3) (exponent -6)))))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Reduced_natural_ratio
            (((prime 2) (exponent 7)) ((prime 3) (exponent -4)))))))
        (string_number I))))) |}];
  ()
;;
