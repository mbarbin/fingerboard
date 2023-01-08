open! Core
open! Fingerboard

let create () = Cello.fifth_system ()

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (List.concat
       [ (Cello.Fingerboard_position_name.Pythagorean.all
           :> Cello.Fingerboard_position_name.t list)
       ; (Cello.Fingerboard_position_name.Just.all
           :> Cello.Fingerboard_position_name.t list)
       ])
;;

let t =
  lazy
    (let t = create () in
     add_positions t;
     t)
;;

let%expect_test "sexp_of_t" =
  let t = force t in
  print_s [%sexp (t : System.t)];
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
      (((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))
     (fingerboard_positions
      (((name 0) (at_octave 0) (basis_acoustic_interval_to_the_open_string Zero))
       ((name m2p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 8)) ((prime 3) (exponent -5))))))
       ((name A1z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -7)) ((prime 3) (exponent 3))
           ((prime 5) (exponent 1))))))
       ((name m2z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
           ((prime 5) (exponent -1))))))
       ((name A1p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -11)) ((prime 3) (exponent 7))))))
       ((name d3p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 16)) ((prime 3) (exponent -10))))))
       ((name M2z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name d3z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 12)) ((prime 3) (exponent -6))
           ((prime 5) (exponent -1))))))
       ((name M2p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -3)) ((prime 3) (exponent 2))))))
       ((name m3p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 5)) ((prime 3) (exponent -3))))))
       ((name m3z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
           ((prime 5) (exponent -1))))))
       ((name A2p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -14)) ((prime 3) (exponent 9))))))
       ((name d4p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 13)) ((prime 3) (exponent -8))))))
       ((name M3z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -2)) ((prime 5) (exponent 1))))))
       ((name d4z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 9)) ((prime 3) (exponent -4))
           ((prime 5) (exponent -1))))))
       ((name M3p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -6)) ((prime 3) (exponent 4))))))
       ((name 4p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 2)) ((prime 3) (exponent -1))))))
       ((name 4z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -2)) ((prime 3) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name A3p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -17)) ((prime 3) (exponent 11))))))
       ((name d5p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 10)) ((prime 3) (exponent -6))))))
       ((name A4z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -5)) ((prime 3) (exponent 2))
           ((prime 5) (exponent 1))))))
       ((name d5z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
           ((prime 5) (exponent -1))))))
       ((name A4p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -9)) ((prime 3) (exponent 6))))))
       ((name d6p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 18)) ((prime 3) (exponent -11))))))
       ((name 5z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
           ((prime 5) (exponent 1))))))
       ((name 5p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((name m6p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 7)) ((prime 3) (exponent -4))))))
       ((name m6z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 5) (exponent -1))))))
       ((name A5p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -12)) ((prime 3) (exponent 8))))))
       ((name d7p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 15)) ((prime 3) (exponent -9))))))
       ((name M6z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 3) (exponent -1)) ((prime 5) (exponent 1))))))
       ((name d7z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 11)) ((prime 3) (exponent -5))
           ((prime 5) (exponent -1))))))
       ((name M6p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -4)) ((prime 3) (exponent 3))))))
       ((name m7p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 4)) ((prime 3) (exponent -2))))))
       ((name A6p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -15)) ((prime 3) (exponent 10))))))
       ((name d8p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 12)) ((prime 3) (exponent -7))))))
       ((name M7z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -3)) ((prime 3) (exponent 1))
           ((prime 5) (exponent 1))))))
       ((name d8z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 8)) ((prime 3) (exponent -3))
           ((prime 5) (exponent -1))))))
       ((name M7p) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -7)) ((prime 3) (exponent 5))))))
       ((name 8z) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 5)) ((prime 3) (exponent -4))
           ((prime 5) (exponent 1))))))
       ((name 0) (at_octave 1) (basis_acoustic_interval_to_the_open_string Zero))
       ((name m2p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 8)) ((prime 3) (exponent -5))))))
       ((name A1z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -7)) ((prime 3) (exponent 3))
           ((prime 5) (exponent 1))))))
       ((name m2z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
           ((prime 5) (exponent -1))))))
       ((name A1p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -11)) ((prime 3) (exponent 7))))))
       ((name d3p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 16)) ((prime 3) (exponent -10))))))
       ((name M2z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name d3z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 12)) ((prime 3) (exponent -6))
           ((prime 5) (exponent -1))))))
       ((name M2p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -3)) ((prime 3) (exponent 2))))))
       ((name m3p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 5)) ((prime 3) (exponent -3))))))
       ((name m3z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
           ((prime 5) (exponent -1))))))
       ((name A2p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -14)) ((prime 3) (exponent 9))))))
       ((name d4p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 13)) ((prime 3) (exponent -8))))))
       ((name M3z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -2)) ((prime 5) (exponent 1))))))
       ((name d4z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 9)) ((prime 3) (exponent -4))
           ((prime 5) (exponent -1))))))
       ((name M3p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -6)) ((prime 3) (exponent 4))))))
       ((name 4p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 2)) ((prime 3) (exponent -1))))))
       ((name 4z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -2)) ((prime 3) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name A3p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -17)) ((prime 3) (exponent 11))))))
       ((name d5p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 10)) ((prime 3) (exponent -6))))))
       ((name A4z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -5)) ((prime 3) (exponent 2))
           ((prime 5) (exponent 1))))))
       ((name d5z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
           ((prime 5) (exponent -1))))))
       ((name A4p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -9)) ((prime 3) (exponent 6))))))
       ((name d6p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 18)) ((prime 3) (exponent -11))))))
       ((name 5z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
           ((prime 5) (exponent 1))))))
       ((name 5p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((name m6p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 7)) ((prime 3) (exponent -4))))))
       ((name m6z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 5) (exponent -1))))))
       ((name A5p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -12)) ((prime 3) (exponent 8))))))
       ((name d7p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 15)) ((prime 3) (exponent -9))))))
       ((name M6z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 3) (exponent -1)) ((prime 5) (exponent 1))))))
       ((name d7z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 11)) ((prime 3) (exponent -5))
           ((prime 5) (exponent -1))))))
       ((name M6p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -4)) ((prime 3) (exponent 3))))))
       ((name m7p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 4)) ((prime 3) (exponent -2))))))
       ((name A6p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -15)) ((prime 3) (exponent 10))))))
       ((name d8p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 12)) ((prime 3) (exponent -7))))))
       ((name M7z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -3)) ((prime 3) (exponent 1))
           ((prime 5) (exponent 1))))))
       ((name d8z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 8)) ((prime 3) (exponent -3))
           ((prime 5) (exponent -1))))))
       ((name M7p) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -7)) ((prime 3) (exponent 5))))))
       ((name 8z) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 5)) ((prime 3) (exponent -4))
           ((prime 5) (exponent 1))))))
       ((name 0) (at_octave 2) (basis_acoustic_interval_to_the_open_string Zero))
       ((name m2p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 8)) ((prime 3) (exponent -5))))))
       ((name A1z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -7)) ((prime 3) (exponent 3))
           ((prime 5) (exponent 1))))))
       ((name m2z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 4)) ((prime 3) (exponent -1))
           ((prime 5) (exponent -1))))))
       ((name A1p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -11)) ((prime 3) (exponent 7))))))
       ((name d3p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 16)) ((prime 3) (exponent -10))))))
       ((name M2z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 1)) ((prime 3) (exponent -2))
           ((prime 5) (exponent 1))))))
       ((name d3z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 12)) ((prime 3) (exponent -6))
           ((prime 5) (exponent -1))))))
       ((name M2p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -3)) ((prime 3) (exponent 2))))))
       ((name m3p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 5)) ((prime 3) (exponent -3))))))
       ((name m3z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 1)) ((prime 3) (exponent 1))
           ((prime 5) (exponent -1))))))
       ((name A2p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -14)) ((prime 3) (exponent 9))))))
       ((name d4p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 13)) ((prime 3) (exponent -8))))))
       ((name M3z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -2)) ((prime 5) (exponent 1))))))
       ((name d4z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 9)) ((prime 3) (exponent -4))
           ((prime 5) (exponent -1))))))
       ((name M3p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -6)) ((prime 3) (exponent 4))))))
       ((name 4p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 2)) ((prime 3) (exponent -1))))))
       ((name 4z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -2)) ((prime 3) (exponent 3))
           ((prime 5) (exponent -1))))))
       ((name A3p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -17)) ((prime 3) (exponent 11))))))
       ((name d5p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 10)) ((prime 3) (exponent -6))))))
       ((name A4z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -5)) ((prime 3) (exponent 2))
           ((prime 5) (exponent 1))))))
       ((name d5z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 6)) ((prime 3) (exponent -2))
           ((prime 5) (exponent -1))))))
       ((name A4p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -9)) ((prime 3) (exponent 6))))))
       ((name d6p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 18)) ((prime 3) (exponent -11))))))
       ((name 5z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 3) (exponent -3))
           ((prime 5) (exponent 1))))))
       ((name 5p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((name m6p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 7)) ((prime 3) (exponent -4))))))
       ((name m6z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 5) (exponent -1))))))
       ((name A5p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -12)) ((prime 3) (exponent 8))))))
       ((name d7p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 15)) ((prime 3) (exponent -9))))))
       ((name M6z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 3) (exponent -1)) ((prime 5) (exponent 1))))))
       ((name d7z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 11)) ((prime 3) (exponent -5))
           ((prime 5) (exponent -1))))))
       ((name M6p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -4)) ((prime 3) (exponent 3))))))
       ((name m7p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 4)) ((prime 3) (exponent -2))))))
       ((name A6p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -15)) ((prime 3) (exponent 10))))))
       ((name d8p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 12)) ((prime 3) (exponent -7))))))
       ((name M7z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -3)) ((prime 3) (exponent 1))
           ((prime 5) (exponent 1))))))
       ((name d8z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 8)) ((prime 3) (exponent -3))
           ((prime 5) (exponent -1))))))
       ((name M7p) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent -7)) ((prime 3) (exponent 5))))))
       ((name 8z) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Reduced_natural_ratio
          (((prime 2) (exponent 5)) ((prime 3) (exponent -4))
           ((prime 5) (exponent 1))))))))) |}];
  ()
;;

let%expect_test "tables" =
  let t = force t in
  print_endline (System.to_ascii_tables t);
  [%expect
    {|
    ┌────────┬──────┬────────┬────────────┬───────┐
    │ String │ Note │  Pitch │ Interval   │ Cents │
    ├────────┼──────┼────────┼────────────┼───────┤
    │      I │ A3   │ 220.00 │ P5 - 3 / 2 │ 702   │
    │     II │ D3   │ 146.67 │ P5 - 3 / 2 │ 702   │
    │    III │ G2   │  97.78 │ P5 - 3 / 2 │ 702   │
    │     IV │ C2   │  65.19 │            │       │
    └────────┴──────┴────────┴────────────┴───────┘

    ┌───────┬───────┬──────────────────────┐
    │   Pos │ Cents │             Interval │
    ├───────┼───────┼──────────────────────┤
    │     0 │     0 │               unison │
    │   m2p │    90 │        2 ^ 8 / 3 ^ 5 │
    │   A1z │    92 │  (3 ^ 3 * 5) / 2 ^ 7 │
    │   m2z │   112 │      2 ^ 4 / (3 * 5) │
    │   A1p │   114 │       3 ^ 7 / 2 ^ 11 │
    │   d3p │   180 │      2 ^ 16 / 3 ^ 10 │
    │   M2z │   182 │      (2 * 5) / 3 ^ 2 │
    │   d3z │   202 │ 2 ^ 12 / (3 ^ 6 * 5) │
    │   M2p │   204 │        3 ^ 2 / 2 ^ 3 │
    │   m3p │   294 │        2 ^ 5 / 3 ^ 3 │
    │   m3z │   316 │          (2 * 3) / 5 │
    │   A2p │   318 │       3 ^ 9 / 2 ^ 14 │
    │   d4p │   384 │       2 ^ 13 / 3 ^ 8 │
    │   M3z │   386 │            5 / 2 ^ 2 │
    │   d4z │   406 │  2 ^ 9 / (3 ^ 4 * 5) │
    │   M3p │   408 │        3 ^ 4 / 2 ^ 6 │
    │    4p │   498 │            2 ^ 2 / 3 │
    │    4z │   520 │  3 ^ 3 / (2 ^ 2 * 5) │
    │   A3p │   522 │      3 ^ 11 / 2 ^ 17 │
    │   d5p │   588 │       2 ^ 10 / 3 ^ 6 │
    │   A4z │   590 │  (3 ^ 2 * 5) / 2 ^ 5 │
    │   d5z │   610 │  2 ^ 6 / (3 ^ 2 * 5) │
    │   A4p │   612 │        3 ^ 6 / 2 ^ 9 │
    │   d6p │   678 │      2 ^ 18 / 3 ^ 11 │
    │    5z │   680 │  (2 ^ 3 * 5) / 3 ^ 3 │
    │    5p │   702 │                3 / 2 │
    │   m6p │   792 │        2 ^ 7 / 3 ^ 4 │
    │   m6z │   814 │            2 ^ 3 / 5 │
    │   A5p │   816 │       3 ^ 8 / 2 ^ 12 │
    │   d7p │   882 │       2 ^ 15 / 3 ^ 9 │
    │   M6z │   884 │                5 / 3 │
    │   d7z │   904 │ 2 ^ 11 / (3 ^ 5 * 5) │
    │   M6p │   906 │        3 ^ 3 / 2 ^ 4 │
    │   m7p │   996 │        2 ^ 4 / 3 ^ 2 │
    │   A6p │  1020 │      3 ^ 10 / 2 ^ 15 │
    │   d8p │  1086 │       2 ^ 12 / 3 ^ 7 │
    │   M7z │  1088 │      (3 * 5) / 2 ^ 3 │
    │   d8z │  1108 │  2 ^ 8 / (3 ^ 3 * 5) │
    │   M7p │  1110 │        3 ^ 5 / 2 ^ 7 │
    │    8z │  1178 │  (2 ^ 5 * 5) / 3 ^ 4 │
    │   0-1 │  1200 │             1 octave │
    │ m2p-1 │  1290 │        2 ^ 9 / 3 ^ 5 │
    │ A1z-1 │  1292 │  (3 ^ 3 * 5) / 2 ^ 6 │
    │ m2z-1 │  1312 │      2 ^ 5 / (3 * 5) │
    │ A1p-1 │  1314 │       3 ^ 7 / 2 ^ 10 │
    │ d3p-1 │  1380 │      2 ^ 17 / 3 ^ 10 │
    │ M2z-1 │  1382 │  (2 ^ 2 * 5) / 3 ^ 2 │
    │ d3z-1 │  1402 │ 2 ^ 13 / (3 ^ 6 * 5) │
    │ M2p-1 │  1404 │        3 ^ 2 / 2 ^ 2 │
    │ m3p-1 │  1494 │        2 ^ 6 / 3 ^ 3 │
    │ m3z-1 │  1516 │      (2 ^ 2 * 3) / 5 │
    │ A2p-1 │  1518 │       3 ^ 9 / 2 ^ 13 │
    │ d4p-1 │  1584 │       2 ^ 14 / 3 ^ 8 │
    │ M3z-1 │  1586 │                5 / 2 │
    │ d4z-1 │  1606 │ 2 ^ 10 / (3 ^ 4 * 5) │
    │ M3p-1 │  1608 │        3 ^ 4 / 2 ^ 5 │
    │  4p-1 │  1698 │            2 ^ 3 / 3 │
    │  4z-1 │  1720 │      3 ^ 3 / (2 * 5) │
    │ A3p-1 │  1722 │      3 ^ 11 / 2 ^ 16 │
    │ d5p-1 │  1788 │       2 ^ 11 / 3 ^ 6 │
    │ A4z-1 │  1790 │  (3 ^ 2 * 5) / 2 ^ 4 │
    │ d5z-1 │  1810 │  2 ^ 7 / (3 ^ 2 * 5) │
    │ A4p-1 │  1812 │        3 ^ 6 / 2 ^ 8 │
    │ d6p-1 │  1878 │      2 ^ 19 / 3 ^ 11 │
    │  5z-1 │  1880 │  (2 ^ 4 * 5) / 3 ^ 3 │
    │  5p-1 │  1902 │                    3 │
    │ m6p-1 │  1992 │        2 ^ 8 / 3 ^ 4 │
    │ m6z-1 │  2014 │            2 ^ 4 / 5 │
    │ A5p-1 │  2016 │       3 ^ 8 / 2 ^ 11 │
    │ d7p-1 │  2082 │       2 ^ 16 / 3 ^ 9 │
    │ M6z-1 │  2084 │          (2 * 5) / 3 │
    │ d7z-1 │  2104 │ 2 ^ 12 / (3 ^ 5 * 5) │
    │ M6p-1 │  2106 │        3 ^ 3 / 2 ^ 3 │
    │ m7p-1 │  2196 │        2 ^ 5 / 3 ^ 2 │
    │ A6p-1 │  2220 │      3 ^ 10 / 2 ^ 14 │
    │ d8p-1 │  2286 │       2 ^ 13 / 3 ^ 7 │
    │ M7z-1 │  2288 │      (3 * 5) / 2 ^ 2 │
    │ d8z-1 │  2308 │  2 ^ 9 / (3 ^ 3 * 5) │
    │ M7p-1 │  2310 │        3 ^ 5 / 2 ^ 6 │
    │  8z-1 │  2378 │  (2 ^ 6 * 5) / 3 ^ 4 │
    │   0-2 │  2400 │            2 octaves │
    │ m2p-2 │  2490 │       2 ^ 10 / 3 ^ 5 │
    │ A1z-2 │  2492 │  (3 ^ 3 * 5) / 2 ^ 5 │
    │ m2z-2 │  2512 │      2 ^ 6 / (3 * 5) │
    │ A1p-2 │  2514 │        3 ^ 7 / 2 ^ 9 │
    │ d3p-2 │  2580 │      2 ^ 18 / 3 ^ 10 │
    │ M2z-2 │  2582 │  (2 ^ 3 * 5) / 3 ^ 2 │
    │ d3z-2 │  2602 │ 2 ^ 14 / (3 ^ 6 * 5) │
    │ M2p-2 │  2604 │            3 ^ 2 / 2 │
    │ m3p-2 │  2694 │        2 ^ 7 / 3 ^ 3 │
    │ m3z-2 │  2716 │      (2 ^ 3 * 3) / 5 │
    │ A2p-2 │  2718 │       3 ^ 9 / 2 ^ 12 │
    │ d4p-2 │  2784 │       2 ^ 15 / 3 ^ 8 │
    │ M3z-2 │  2786 │                    5 │
    │ d4z-2 │  2806 │ 2 ^ 11 / (3 ^ 4 * 5) │
    │ M3p-2 │  2808 │        3 ^ 4 / 2 ^ 4 │
    │  4p-2 │  2898 │            2 ^ 4 / 3 │
    │  4z-2 │  2920 │            3 ^ 3 / 5 │
    │ A3p-2 │  2922 │      3 ^ 11 / 2 ^ 15 │
    │ d5p-2 │  2988 │       2 ^ 12 / 3 ^ 6 │
    │ A4z-2 │  2990 │  (3 ^ 2 * 5) / 2 ^ 3 │
    │ d5z-2 │  3010 │  2 ^ 8 / (3 ^ 2 * 5) │
    │ A4p-2 │  3012 │        3 ^ 6 / 2 ^ 7 │
    │ d6p-2 │  3078 │      2 ^ 20 / 3 ^ 11 │
    │  5z-2 │  3080 │  (2 ^ 5 * 5) / 3 ^ 3 │
    │  5p-2 │  3102 │              (2 * 3) │
    │ m6p-2 │  3192 │        2 ^ 9 / 3 ^ 4 │
    │ m6z-2 │  3214 │            2 ^ 5 / 5 │
    │ A5p-2 │  3216 │       3 ^ 8 / 2 ^ 10 │
    │ d7p-2 │  3282 │       2 ^ 17 / 3 ^ 9 │
    │ M6z-2 │  3284 │      (2 ^ 2 * 5) / 3 │
    │ d7z-2 │  3304 │ 2 ^ 13 / (3 ^ 5 * 5) │
    │ M6p-2 │  3306 │        3 ^ 3 / 2 ^ 2 │
    │ m7p-2 │  3396 │        2 ^ 6 / 3 ^ 2 │
    │ A6p-2 │  3420 │      3 ^ 10 / 2 ^ 13 │
    │ d8p-2 │  3486 │       2 ^ 14 / 3 ^ 7 │
    │ M7z-2 │  3488 │          (3 * 5) / 2 │
    │ d8z-2 │  3508 │ 2 ^ 10 / (3 ^ 3 * 5) │
    │ M7p-2 │  3510 │        3 ^ 5 / 2 ^ 5 │
    │  8z-2 │  3578 │  (2 ^ 7 * 5) / 3 ^ 4 │
    └───────┴───────┴──────────────────────┘ |}]
;;
