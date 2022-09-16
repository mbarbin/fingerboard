open! Core
open! Cemper

let characterized_major_scale =
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
  let t = force Pythagorean.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let c_scale =
  let t = force Pythagorean.t in
  make_scale t ~characterized_scale:characterized_major_scale ~from:lower_c
;;

let%expect_test "c_scale" =
  print_s [%sexp (c_scale : Located_note.t list)];
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

let lower_e_flat =
  let t = force Pythagorean.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3p
      ; string_number = IV
      }
  }
;;

let e_flat_scale =
  let t = force Pythagorean.t in
  make_scale t ~characterized_scale:characterized_major_scale ~from:lower_e_flat
;;

let%expect_test "e_flat_scale" =
  print_s [%sexp (e_flat_scale : Located_note.t list)];
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
