open! Core
open! Cemper

let lower_c =
  let t = force E12.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

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

let c_scale =
  let t = force E12.t in
  let rec aux acc scale (located_note : Located_note.t) =
    if Option.is_some
         (Interval.compute ~from:Cello.fingerboard_highest_note ~to_:located_note.note ())
    then acc
    else (
      match scale with
      | [] -> aux acc characterized_major_scale located_note
      | hd :: tl ->
        (match System.find_next_located_note t located_note hd with
         | None -> acc
         | Some next_located_note -> aux (next_located_note :: acc) tl next_located_note))
  in
  aux [ lower_c ] [] lower_c |> List.rev
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
