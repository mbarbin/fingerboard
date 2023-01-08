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

let%expect_test "tables" =
  let t = force t in
  print_endline (System.to_ascii_tables t);
  [%expect
    {|
    ┌────────┬──────┬────────┬──────────────┬───────┐
    │ String │ Note │  Pitch │ Interval     │ Cents │
    ├────────┼──────┼────────┼──────────────┼───────┤
    │      I │ A3   │ 220.00 │ P5 - 7-12edo │ 700   │
    │     II │ D3   │ 146.83 │ P5 - 7-12edo │ 700   │
    │    III │ G2   │  98.00 │ P5 - 7-12edo │ 700   │
    │     IV │ C2   │  65.41 │              │       │
    └────────┴──────┴────────┴──────────────┴───────┘

    ┌───────┬───────┬───────────┐
    │   Pos │ Cents │  Interval │
    ├───────┼───────┼───────────┤
    │     0 │     0 │    unison │
    │   m2e │   100 │   1-12edo │
    │   M2e │   200 │   2-12edo │
    │   m3e │   300 │   3-12edo │
    │   M3e │   400 │   4-12edo │
    │    4e │   500 │   5-12edo │
    │   A4e │   600 │   6-12edo │
    │    5e │   700 │   7-12edo │
    │   m6e │   800 │   8-12edo │
    │   M6e │   900 │   9-12edo │
    │   m7e │  1000 │  10-12edo │
    │   M7e │  1100 │  11-12edo │
    │   0-1 │  1200 │  1 octave │
    │ m2e-1 │  1300 │  13-12edo │
    │ M2e-1 │  1400 │  14-12edo │
    │ m3e-1 │  1500 │  15-12edo │
    │ M3e-1 │  1600 │  16-12edo │
    │  4e-1 │  1700 │  17-12edo │
    │ A4e-1 │  1800 │  18-12edo │
    │  5e-1 │  1900 │  19-12edo │
    │ m6e-1 │  2000 │  20-12edo │
    │ M6e-1 │  2100 │  21-12edo │
    │ m7e-1 │  2200 │  22-12edo │
    │ M7e-1 │  2300 │  23-12edo │
    │   0-2 │  2400 │ 2 octaves │
    │ m2e-2 │  2500 │  25-12edo │
    │ M2e-2 │  2600 │  26-12edo │
    │ m3e-2 │  2700 │  27-12edo │
    │ M3e-2 │  2800 │  28-12edo │
    │  4e-2 │  2900 │  29-12edo │
    │ A4e-2 │  3000 │  30-12edo │
    │  5e-2 │  3100 │  31-12edo │
    │ m6e-2 │  3200 │  32-12edo │
    │ M6e-2 │  3300 │  33-12edo │
    │ m7e-2 │  3400 │  34-12edo │
    │ M7e-2 │  3500 │  35-12edo │
    └───────┴───────┴───────────┘ |}]
;;
