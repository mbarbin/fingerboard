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
         ~acoustic_interval:
           (Acoustic_interval.equal_division_of_the_octave
              ~divisor:53
              ~number_of_divisions:31))
  in
  System.create ~high_vibrating_string:a ~pitch ~intervals_going_down
;;

let add_positions t =
  List.iter
    ~f:(fun name ->
      System.add_fingerboard_position_exn t (Cello.fingerboard_position name))
    (List.concat
       [ [ `open_string ]
       ; (Cello.Fingerboard_position_name.Edo53.all
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
        (pitch 146.67244526002719) (roman_numeral II))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.785482720707591) (roman_numeral III))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.192890279901448) (roman_numeral IV))))
     (intervals_going_down
      (((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31))))))
     (fingerboard_positions
      (((name 0) (at_octave 0) (basis_acoustic_interval_to_the_open_string Zero))
       ((name A1z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 4))))
       ((name m2z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 5))))
       ((name M2z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8))))
       ((name M2p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9))))
       ((name m3p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13))))
       ((name m3z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 14))))
       ((name M3z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 17))))
       ((name M3p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 18))))
       ((name 4p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22))))
       ((name 4z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 23))))
       ((name A4z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 26))))
       ((name d5z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 27))))
       ((name 5z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30))))
       ((name 5p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31))))
       ((name m6p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35))))
       ((name m6z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 36))))
       ((name m7p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44))))
       ((name m7z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 45))))
       ((name M7z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 48))))
       ((name M7p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 49))))
       ((name 8z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 52))))
       ((name 0) (at_octave 1) (basis_acoustic_interval_to_the_open_string Zero))
       ((name A1z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 4))))
       ((name m2z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 5))))
       ((name M2z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8))))
       ((name M2p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9))))
       ((name m3p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13))))
       ((name m3z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 14))))
       ((name M3z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 17))))
       ((name M3p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 18))))
       ((name 4p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22))))
       ((name 4z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 23))))
       ((name A4z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 26))))
       ((name d5z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 27))))
       ((name 5z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30))))
       ((name 5p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31))))
       ((name m6p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35))))
       ((name m6z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 36))))
       ((name m7p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44))))
       ((name m7z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 45))))
       ((name M7z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 48))))
       ((name M7p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 49))))
       ((name 8z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 52))))
       ((name 0) (at_octave 2) (basis_acoustic_interval_to_the_open_string Zero))
       ((name A1z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 4))))
       ((name m2z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 5))))
       ((name M2z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8))))
       ((name M2p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9))))
       ((name m3p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13))))
       ((name m3z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 14))))
       ((name M3z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 17))))
       ((name M3p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 18))))
       ((name 4p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22))))
       ((name 4z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 23))))
       ((name A4z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 26))))
       ((name d5z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 27))))
       ((name 5z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30))))
       ((name 5p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31))))
       ((name m6p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35))))
       ((name m6z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 36))))
       ((name m7p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44))))
       ((name m7z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 45))))
       ((name M7z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 48))))
       ((name M7p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 49))))
       ((name 8z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 52))))))) |}];
  ()
;;
