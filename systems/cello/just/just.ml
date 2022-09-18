open! Core
open! Cemper

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