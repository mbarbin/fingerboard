open! Core
open! Cemper

let characterized_major_just_scale =
  let second quality acoustic_interval =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn ~interval ~acoustic_interval
  in
  let minor_ton = second Major Acoustic_interval.just_minor_ton in
  let major_ton = second Major Acoustic_interval.just_major_ton in
  let semiton = second Minor Acoustic_interval.just_diatonic_semiton in
  [ major_ton; minor_ton; semiton; major_ton; minor_ton; major_ton; semiton ]
;;

let characterized_major_pythagorean_scale =
  let second quality =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.pythagorean interval)
  in
  let ton = second Major in
  let semiton = second Minor in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let lower_c =
  let t = force Just.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let c_just_scale =
  let t = force Just.t in
  make_scale t ~characterized_scale:characterized_major_just_scale ~from:lower_c
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

let c_pythagorean_scale =
  let t = force Just.t in
  make_scale t ~characterized_scale:characterized_major_pythagorean_scale ~from:lower_c
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
  make_scale t ~characterized_scale:characterized_major_just_scale ~from:lower_ez_flat
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
    ~characterized_scale:characterized_major_pythagorean_scale
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
    ~characterized_scale:characterized_major_pythagorean_scale
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
