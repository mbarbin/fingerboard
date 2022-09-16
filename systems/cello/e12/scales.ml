open! Core
open! Cemper

let characterized_major_scale =
  let second quality =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.equal_tempered_12 interval)
  in
  let ton = second Major in
  let semiton = second Minor in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let lower_c =
  let t = force E12.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let c_scale =
  let t = force E12.t in
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
         ((name M2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (string_number IV))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
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
         ((name M2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (string_number III))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
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
         ((name M2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
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
         ((name M2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
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
         ((name M2e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
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
         ((name M2e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
        (string_number I))))) |}];
  ()
;;

let lower_e_flat =
  let t = force E12.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3e
      ; string_number = IV
      }
  }
;;

let e_flat_scale =
  let t = force E12.t in
  make_scale t ~characterized_scale:characterized_major_scale ~from:lower_e_flat
;;

let%expect_test "e_flat_scale" =
  print_s [%sexp (e_flat_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name E) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
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
         ((name m2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (string_number III))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
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
         ((name m2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number II))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name A4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (string_number II))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name A4e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
        (string_number I))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name M7e) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11)))))
        (string_number I))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name A4e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
        (string_number I))))
     ((note ((letter_name A) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name M7e) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11)))))
        (string_number I))))
     ((note ((letter_name B) (symbol Flat) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m2e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Flat) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name A4e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6e) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (string_number I))))) |}];
  ()
;;
