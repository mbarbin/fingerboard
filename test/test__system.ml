open! Core
open! Cemper

let%expect_test "4-strings cello" =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Acoustic_interval.shift_down Frequency.a4_440 Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create ~len:3 (fifth, Acoustic_interval.of_symbolic (Pythagorean fifth))
  in
  let system = System.create ~high_vibrating_string:a ~pitch ~intervals_going_down in
  print_s [%sexp (system : System.t)];
  [%expect {|
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
