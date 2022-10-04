open! Core
open! Fingerboard

let create () =
  Cello.fifth_system
    ~acoustic_interval:
      (Acoustic_interval.equal_tempered_12
         { number = Fifth; quality = Perfect; additional_octaves = 0 })
    ()
;;

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (Cello.Fingerboard_position_name.Edo12.all :> Cello.Fingerboard_position_name.t list)
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
        (pitch 146.83238395870379) (roman_numeral II))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.998858995437331) (roman_numeral III))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.406391325149656) (roman_numeral IV))))
     (intervals_going_down
      (((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7))))))
     (fingerboard_positions
      (((name 0) (at_octave 0) (basis_acoustic_interval_to_the_open_string Zero))
       ((name m2e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1))))
       ((name M2e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2))))
       ((name m3e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3))))
       ((name M3e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4))))
       ((name 4e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5))))
       ((name A4e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6))))
       ((name 5e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7))))
       ((name m6e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8))))
       ((name M6e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 9))))
       ((name m7e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10))))
       ((name M7e) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11))))
       ((name 0) (at_octave 1) (basis_acoustic_interval_to_the_open_string Zero))
       ((name m2e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1))))
       ((name M2e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2))))
       ((name m3e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3))))
       ((name M3e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4))))
       ((name 4e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5))))
       ((name A4e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6))))
       ((name 5e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7))))
       ((name m6e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8))))
       ((name M6e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 9))))
       ((name m7e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10))))
       ((name M7e) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11))))
       ((name 0) (at_octave 2) (basis_acoustic_interval_to_the_open_string Zero))
       ((name m2e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1))))
       ((name M2e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2))))
       ((name m3e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3))))
       ((name M3e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4))))
       ((name 4e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5))))
       ((name A4e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6))))
       ((name 5e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7))))
       ((name m6e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8))))
       ((name M6e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 9))))
       ((name m7e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10))))
       ((name M7e) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11))))))) |}]
;;
