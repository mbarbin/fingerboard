open! Core
open! Cemper

let fingerboard_high_bound =
  (* The selection of this particular note is somewhat arbitrary,
     anything around that note is realistic here. *)
  { Note.letter_name = E; symbol = Natural; octave_designation = 6 }
;;

let lower_c =
  let t = force E12.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let characterized_major_scale =
  let ton =
    let interval =
      { Interval.number = Second; quality = Major; additional_octaves = 0 }
    in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.equal_tempered_12 interval)
  in
  let semiton =
    let interval =
      { Interval.number = Second; quality = Minor; additional_octaves = 0 }
    in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.equal_tempered_12 interval)
  in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let c_scale =
  let t = force E12.t in
  let rec aux acc scale (located_note : Located_note.t) =
    if Option.is_some
         (Interval.compute ~from:fingerboard_high_bound ~to_:located_note.note ())
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
         ((name 0)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
          (at_octave 0)))
        (string_number IV))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 2M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
          (at_octave 0)))
        (string_number IV))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 3M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
          (at_octave 0)))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name P4)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
          (at_octave 0)))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
          (at_octave 0)))
        (string_number III))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 2M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
          (at_octave 0)))
        (string_number III))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 3M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
          (at_octave 0)))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name P4)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
          (at_octave 0)))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
          (at_octave 0)))
        (string_number II))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 2M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
          (at_octave 0)))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 3m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
          (at_octave 0)))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name P4)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
          (at_octave 0)))
        (string_number II))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 2M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 3m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name P4)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name P5)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 6m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 7m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
          (at_octave 0)))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 2M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 3m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name P4)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name P5)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 6m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 7m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
          (at_octave 1)))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
          (at_octave 2)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 2M)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
          (at_octave 2)))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 3m)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
          (at_octave 2)))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name P4)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
          (at_octave 2)))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name P5)
          (basis_vibrating_string_portion
           ((acoustic_interval_to_the_open_string
             (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
          (at_octave 2)))
        (string_number I))))) |}];
  ()
;;
