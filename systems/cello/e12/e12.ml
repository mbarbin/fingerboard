open! Core
open! Cemper

let create () =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create
      ~len:3
      (Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:(Acoustic_interval.equal_tempered_12 fifth))
  in
  System.create ~high_vibrating_string:a ~pitch ~intervals_going_down
;;

let name = function
  | 0 -> "0"
  | 1 -> "2m"
  | 2 -> "2M"
  | 3 -> "3m"
  | 4 -> "3M"
  | 5 -> "P4"
  | 6 -> "A4"
  | 7 -> "P5"
  | 8 -> "6m"
  | 9 -> "6M"
  | 10 -> "7m"
  | 11 -> "7M"
  | i -> raise_s [%sexp "Out of bounds", [%here], (i : int)]
;;

let add_positions t =
  for i = 0 to 11 do
    let fingerboard_position =
      Fingerboard_position.create_exn
        ~name:(name i)
        ~acoustic_interval_to_the_open_string:
          (Acoustic_interval.equal_division_of_the_octave
             ~divisor:12
             ~number_of_divisions:i)
    in
    System.add_fingerboard_position_exn t fingerboard_position
  done
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
      (((name 0)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
        (at_octave 0))
       ((name 2m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (at_octave 0))
       ((name 2M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (at_octave 0))
       ((name 3m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (at_octave 0))
       ((name 3M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
        (at_octave 0))
       ((name P4)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (at_octave 0))
       ((name A4)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (at_octave 0))
       ((name P5)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
        (at_octave 0))
       ((name 6m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (at_octave 0))
       ((name 6M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 9)))))
        (at_octave 0))
       ((name 7m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
        (at_octave 0))
       ((name 7M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11)))))
        (at_octave 0))
       ((name 0)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
        (at_octave 1))
       ((name 2m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (at_octave 1))
       ((name 2M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (at_octave 1))
       ((name 3m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (at_octave 1))
       ((name 3M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
        (at_octave 1))
       ((name P4)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (at_octave 1))
       ((name A4)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (at_octave 1))
       ((name P5)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
        (at_octave 1))
       ((name 6m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (at_octave 1))
       ((name 6M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 9)))))
        (at_octave 1))
       ((name 7m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
        (at_octave 1))
       ((name 7M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11)))))
        (at_octave 1))
       ((name 0)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 0)))))
        (at_octave 2))
       ((name 2m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 1)))))
        (at_octave 2))
       ((name 2M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 2)))))
        (at_octave 2))
       ((name 3m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 3)))))
        (at_octave 2))
       ((name 3M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 4)))))
        (at_octave 2))
       ((name P4)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 5)))))
        (at_octave 2))
       ((name A4)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 6)))))
        (at_octave 2))
       ((name P5)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 7)))))
        (at_octave 2))
       ((name 6m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 8)))))
        (at_octave 2))
       ((name 6M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 9)))))
        (at_octave 2))
       ((name 7m)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 10)))))
        (at_octave 2))
       ((name 7M)
        (basis_vibrating_string_portion
         ((acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 12) (number_of_divisions 11)))))
        (at_octave 2))))) |}]
;;
