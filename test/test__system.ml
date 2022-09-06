open! Core
open! Cemper

let%expect_test "4-strings cello" =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create ~len:3 (fifth, Acoustic_interval.of_symbolic (Pythagorean fifth))
  in
  let system = System.create ~high_vibrating_string:a ~pitch ~intervals_going_down in
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
        (pitch 220) (roman_numeral I))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral II))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.777777777777771) (roman_numeral III))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.185185185185176) (roman_numeral IV))))
     (intervals_going_down
      ((((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))) |}];
  ()
;;

let%expect_test "picollo cello" =
  let e = { Note.letter_name = E; symbol = Natural; octave_designation = 4 } in
  let pitch =
    Frequency.a4_440
    |> Acoustic_interval.shift_down
         (Acoustic_interval.of_symbolic
            (Pythagorean { number = Fourth; quality = Perfect; additional_octaves = 0 }))
  in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create ~len:4 (fifth, Acoustic_interval.of_symbolic (Pythagorean fifth))
  in
  let system = System.create ~high_vibrating_string:e ~pitch ~intervals_going_down in
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name E) (symbol Natural) (octave_designation 4)))
        (pitch 330) (roman_numeral I))
       ((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
        (pitch 220) (roman_numeral II))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral III))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.777777777777771) (roman_numeral IV))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.185185185185176) (roman_numeral V))))
     (intervals_going_down
      ((((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))) |}];
  ()
;;

let%expect_test "5th Bach's suite for cello" =
  let g = { Note.letter_name = G; symbol = Natural; octave_designation = 3 } in
  let pitch =
    Frequency.a4_440
    |> Acoustic_interval.shift_down
         (Acoustic_interval.of_symbolic
            (Pythagorean { number = Second; quality = Major; additional_octaves = 1 }))
  in
  let intervals_going_down =
    let fourth =
      { Interval.number = Fourth; quality = Perfect; additional_octaves = 0 }
    in
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.concat
      [ [| fourth, Acoustic_interval.of_symbolic (Pythagorean fourth) |]
      ; Array.create ~len:2 (fifth, Acoustic_interval.of_symbolic (Pythagorean fifth))
      ]
  in
  let system = System.create ~high_vibrating_string:g ~pitch ~intervals_going_down in
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name G) (symbol Natural) (octave_designation 3)))
        (pitch 195.55555555555554) (roman_numeral I))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral II))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.777777777777771) (roman_numeral III))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.185185185185176) (roman_numeral IV))))
     (intervals_going_down
      ((((number Fourth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent 2)) ((prime 3) (exponent -1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))) |}];
  ()
;;

let%expect_test "Kodaly sonata for cello solo" =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    [| fifth, Acoustic_interval.of_symbolic (Pythagorean fifth)
     ; ( { Interval.number = Sixth; quality = Minor; additional_octaves = 0 }
       , Acoustic_interval.of_symbolic Just_minor_sixth )
     ; fifth, Acoustic_interval.of_symbolic (Pythagorean fifth)
    |]
  in
  let system = System.create ~high_vibrating_string:a ~pitch ~intervals_going_down in
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
        (pitch 220) (roman_numeral I))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral II))
       ((open_string ((letter_name F) (symbol Sharp) (octave_designation 2)))
        (pitch 91.666666666666657) (roman_numeral III))
       ((open_string ((letter_name B) (symbol Natural) (octave_designation 1)))
        (pitch 61.111111111111107) (roman_numeral IV))))
     (intervals_going_down
      ((((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))
       (((number Sixth) (quality Minor) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent 3)) ((prime 5) (exponent -1)))))
       (((number Fifth) (quality Perfect) (additional_octaves 0))
        (Reduced_natural_ratio
         (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))) |}];
  ()
;;
