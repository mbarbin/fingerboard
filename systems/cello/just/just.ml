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
    ┌────────┬──────┬────────┬─────────────────────┐
    │ String │ Note │  Pitch │ Interval going down │
    ├────────┼──────┼────────┼─────────────────────┤
    │ I      │ A3   │ 220.00 │ P5 - 3 / 2          │
    │ II     │ D3   │ 146.67 │ P5 - 3 / 2          │
    │ III    │ G2   │  97.78 │ P5 - 3 / 2          │
    │ IV     │ C2   │  65.19 │                     │
    └────────┴──────┴────────┴─────────────────────┘

    ┌───────┬──────────────────────────────────────┐
    │ Name  │          Interval to the open string │
    ├───────┼──────────────────────────────────────┤
    │ 0     │                  unison (0.00 cents) │
    │ m2p   │          2 ^ 8 / 3 ^ 5 (90.22 cents) │
    │ A1z   │    (3 ^ 3 * 5) / 2 ^ 7 (92.18 cents) │
    │ m2z   │       2 ^ 4 / (3 * 5) (111.73 cents) │
    │ A1p   │        3 ^ 7 / 2 ^ 11 (113.69 cents) │
    │ d3p   │       2 ^ 16 / 3 ^ 10 (180.45 cents) │
    │ M2z   │       (2 * 5) / 3 ^ 2 (182.40 cents) │
    │ d3z   │  2 ^ 12 / (3 ^ 6 * 5) (201.96 cents) │
    │ M2p   │         3 ^ 2 / 2 ^ 3 (203.91 cents) │
    │ m3p   │         2 ^ 5 / 3 ^ 3 (294.13 cents) │
    │ m3z   │           (2 * 3) / 5 (315.64 cents) │
    │ A2p   │        3 ^ 9 / 2 ^ 14 (317.60 cents) │
    │ d4p   │        2 ^ 13 / 3 ^ 8 (384.36 cents) │
    │ M3z   │             5 / 2 ^ 2 (386.31 cents) │
    │ d4z   │   2 ^ 9 / (3 ^ 4 * 5) (405.87 cents) │
    │ M3p   │         3 ^ 4 / 2 ^ 6 (407.82 cents) │
    │ 4p    │             2 ^ 2 / 3 (498.04 cents) │
    │ 4z    │   3 ^ 3 / (2 ^ 2 * 5) (519.55 cents) │
    │ A3p   │       3 ^ 11 / 2 ^ 17 (521.51 cents) │
    │ d5p   │        2 ^ 10 / 3 ^ 6 (588.27 cents) │
    │ A4z   │   (3 ^ 2 * 5) / 2 ^ 5 (590.22 cents) │
    │ d5z   │   2 ^ 6 / (3 ^ 2 * 5) (609.78 cents) │
    │ A4p   │         3 ^ 6 / 2 ^ 9 (611.73 cents) │
    │ d6p   │       2 ^ 18 / 3 ^ 11 (678.49 cents) │
    │ 5z    │   (2 ^ 3 * 5) / 3 ^ 3 (680.45 cents) │
    │ 5p    │                 3 / 2 (701.96 cents) │
    │ m6p   │         2 ^ 7 / 3 ^ 4 (792.18 cents) │
    │ m6z   │             2 ^ 3 / 5 (813.69 cents) │
    │ A5p   │        3 ^ 8 / 2 ^ 12 (815.64 cents) │
    │ d7p   │        2 ^ 15 / 3 ^ 9 (882.40 cents) │
    │ M6z   │                 5 / 3 (884.36 cents) │
    │ d7z   │  2 ^ 11 / (3 ^ 5 * 5) (903.91 cents) │
    │ M6p   │         3 ^ 3 / 2 ^ 4 (905.87 cents) │
    │ m7p   │         2 ^ 4 / 3 ^ 2 (996.09 cents) │
    │ A6p   │      3 ^ 10 / 2 ^ 15 (1019.55 cents) │
    │ d8p   │       2 ^ 12 / 3 ^ 7 (1086.31 cents) │
    │ M7z   │      (3 * 5) / 2 ^ 3 (1088.27 cents) │
    │ d8z   │  2 ^ 8 / (3 ^ 3 * 5) (1107.82 cents) │
    │ M7p   │        3 ^ 5 / 2 ^ 7 (1109.78 cents) │
    │ 8z    │  (2 ^ 5 * 5) / 3 ^ 4 (1178.49 cents) │
    │ 0-1   │             1 octave (1200.00 cents) │
    │ m2p-1 │        2 ^ 9 / 3 ^ 5 (1290.22 cents) │
    │ A1z-1 │  (3 ^ 3 * 5) / 2 ^ 6 (1292.18 cents) │
    │ m2z-1 │      2 ^ 5 / (3 * 5) (1311.73 cents) │
    │ A1p-1 │       3 ^ 7 / 2 ^ 10 (1313.69 cents) │
    │ d3p-1 │      2 ^ 17 / 3 ^ 10 (1380.45 cents) │
    │ M2z-1 │  (2 ^ 2 * 5) / 3 ^ 2 (1382.40 cents) │
    │ d3z-1 │ 2 ^ 13 / (3 ^ 6 * 5) (1401.96 cents) │
    │ M2p-1 │        3 ^ 2 / 2 ^ 2 (1403.91 cents) │
    │ m3p-1 │        2 ^ 6 / 3 ^ 3 (1494.13 cents) │
    │ m3z-1 │      (2 ^ 2 * 3) / 5 (1515.64 cents) │
    │ A2p-1 │       3 ^ 9 / 2 ^ 13 (1517.60 cents) │
    │ d4p-1 │       2 ^ 14 / 3 ^ 8 (1584.36 cents) │
    │ M3z-1 │                5 / 2 (1586.31 cents) │
    │ d4z-1 │ 2 ^ 10 / (3 ^ 4 * 5) (1605.87 cents) │
    │ M3p-1 │        3 ^ 4 / 2 ^ 5 (1607.82 cents) │
    │ 4p-1  │            2 ^ 3 / 3 (1698.04 cents) │
    │ 4z-1  │      3 ^ 3 / (2 * 5) (1719.55 cents) │
    │ A3p-1 │      3 ^ 11 / 2 ^ 16 (1721.51 cents) │
    │ d5p-1 │       2 ^ 11 / 3 ^ 6 (1788.27 cents) │
    │ A4z-1 │  (3 ^ 2 * 5) / 2 ^ 4 (1790.22 cents) │
    │ d5z-1 │  2 ^ 7 / (3 ^ 2 * 5) (1809.78 cents) │
    │ A4p-1 │        3 ^ 6 / 2 ^ 8 (1811.73 cents) │
    │ d6p-1 │      2 ^ 19 / 3 ^ 11 (1878.49 cents) │
    │ 5z-1  │  (2 ^ 4 * 5) / 3 ^ 3 (1880.45 cents) │
    │ 5p-1  │                    3 (1901.96 cents) │
    │ m6p-1 │        2 ^ 8 / 3 ^ 4 (1992.18 cents) │
    │ m6z-1 │            2 ^ 4 / 5 (2013.69 cents) │
    │ A5p-1 │       3 ^ 8 / 2 ^ 11 (2015.64 cents) │
    │ d7p-1 │       2 ^ 16 / 3 ^ 9 (2082.40 cents) │
    │ M6z-1 │          (2 * 5) / 3 (2084.36 cents) │
    │ d7z-1 │ 2 ^ 12 / (3 ^ 5 * 5) (2103.91 cents) │
    │ M6p-1 │        3 ^ 3 / 2 ^ 3 (2105.87 cents) │
    │ m7p-1 │        2 ^ 5 / 3 ^ 2 (2196.09 cents) │
    │ A6p-1 │      3 ^ 10 / 2 ^ 14 (2219.55 cents) │
    │ d8p-1 │       2 ^ 13 / 3 ^ 7 (2286.31 cents) │
    │ M7z-1 │      (3 * 5) / 2 ^ 2 (2288.27 cents) │
    │ d8z-1 │  2 ^ 9 / (3 ^ 3 * 5) (2307.82 cents) │
    │ M7p-1 │        3 ^ 5 / 2 ^ 6 (2309.78 cents) │
    │ 8z-1  │  (2 ^ 6 * 5) / 3 ^ 4 (2378.49 cents) │
    │ 0-2   │            2 octaves (2400.00 cents) │
    │ m2p-2 │       2 ^ 10 / 3 ^ 5 (2490.22 cents) │
    │ A1z-2 │  (3 ^ 3 * 5) / 2 ^ 5 (2492.18 cents) │
    │ m2z-2 │      2 ^ 6 / (3 * 5) (2511.73 cents) │
    │ A1p-2 │        3 ^ 7 / 2 ^ 9 (2513.69 cents) │
    │ d3p-2 │      2 ^ 18 / 3 ^ 10 (2580.45 cents) │
    │ M2z-2 │  (2 ^ 3 * 5) / 3 ^ 2 (2582.40 cents) │
    │ d3z-2 │ 2 ^ 14 / (3 ^ 6 * 5) (2601.96 cents) │
    │ M2p-2 │            3 ^ 2 / 2 (2603.91 cents) │
    │ m3p-2 │        2 ^ 7 / 3 ^ 3 (2694.13 cents) │
    │ m3z-2 │      (2 ^ 3 * 3) / 5 (2715.64 cents) │
    │ A2p-2 │       3 ^ 9 / 2 ^ 12 (2717.60 cents) │
    │ d4p-2 │       2 ^ 15 / 3 ^ 8 (2784.36 cents) │
    │ M3z-2 │                    5 (2786.31 cents) │
    │ d4z-2 │ 2 ^ 11 / (3 ^ 4 * 5) (2805.87 cents) │
    │ M3p-2 │        3 ^ 4 / 2 ^ 4 (2807.82 cents) │
    │ 4p-2  │            2 ^ 4 / 3 (2898.04 cents) │
    │ 4z-2  │            3 ^ 3 / 5 (2919.55 cents) │
    │ A3p-2 │      3 ^ 11 / 2 ^ 15 (2921.51 cents) │
    │ d5p-2 │       2 ^ 12 / 3 ^ 6 (2988.27 cents) │
    │ A4z-2 │  (3 ^ 2 * 5) / 2 ^ 3 (2990.22 cents) │
    │ d5z-2 │  2 ^ 8 / (3 ^ 2 * 5) (3009.78 cents) │
    │ A4p-2 │        3 ^ 6 / 2 ^ 7 (3011.73 cents) │
    │ d6p-2 │      2 ^ 20 / 3 ^ 11 (3078.49 cents) │
    │ 5z-2  │  (2 ^ 5 * 5) / 3 ^ 3 (3080.45 cents) │
    │ 5p-2  │              (2 * 3) (3101.96 cents) │
    │ m6p-2 │        2 ^ 9 / 3 ^ 4 (3192.18 cents) │
    │ m6z-2 │            2 ^ 5 / 5 (3213.69 cents) │
    │ A5p-2 │       3 ^ 8 / 2 ^ 10 (3215.64 cents) │
    │ d7p-2 │       2 ^ 17 / 3 ^ 9 (3282.40 cents) │
    │ M6z-2 │      (2 ^ 2 * 5) / 3 (3284.36 cents) │
    │ d7z-2 │ 2 ^ 13 / (3 ^ 5 * 5) (3303.91 cents) │
    │ M6p-2 │        3 ^ 3 / 2 ^ 2 (3305.87 cents) │
    │ m7p-2 │        2 ^ 6 / 3 ^ 2 (3396.09 cents) │
    │ A6p-2 │      3 ^ 10 / 2 ^ 13 (3419.55 cents) │
    │ d8p-2 │       2 ^ 14 / 3 ^ 7 (3486.31 cents) │
    │ M7z-2 │          (3 * 5) / 2 (3488.27 cents) │
    │ d8z-2 │ 2 ^ 10 / (3 ^ 3 * 5) (3507.82 cents) │
    │ M7p-2 │        3 ^ 5 / 2 ^ 5 (3509.78 cents) │
    │ 8z-2  │  (2 ^ 7 * 5) / 3 ^ 4 (3578.49 cents) │
    └───────┴──────────────────────────────────────┘ |}]
;;
