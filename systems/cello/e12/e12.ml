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
    ┌────────┬──────┬────────┬─────────────────────┐
    │ String │ Note │  Pitch │ Interval going down │
    ├────────┼──────┼────────┼─────────────────────┤
    │ I      │ A3   │ 220.00 │ P5 - 7-12edo        │
    │ II     │ D3   │ 146.83 │ P5 - 7-12edo        │
    │ III    │ G2   │  98.00 │ P5 - 7-12edo        │
    │ IV     │ C2   │  65.41 │                     │
    └────────┴──────┴────────┴─────────────────────┘

    ┌───────┬─────────────────────────────┐
    │ Name  │ Interval to the open string │
    ├───────┼─────────────────────────────┤
    │ 0     │         unison (0.00 cents) │
    │ m2e   │      1-12edo (100.00 cents) │
    │ M2e   │      2-12edo (200.00 cents) │
    │ m3e   │      3-12edo (300.00 cents) │
    │ M3e   │      4-12edo (400.00 cents) │
    │ 4e    │      5-12edo (500.00 cents) │
    │ A4e   │      6-12edo (600.00 cents) │
    │ 5e    │      7-12edo (700.00 cents) │
    │ m6e   │      8-12edo (800.00 cents) │
    │ M6e   │      9-12edo (900.00 cents) │
    │ m7e   │    10-12edo (1000.00 cents) │
    │ M7e   │    11-12edo (1100.00 cents) │
    │ 0-1   │    1 octave (1200.00 cents) │
    │ m2e-1 │    13-12edo (1300.00 cents) │
    │ M2e-1 │    14-12edo (1400.00 cents) │
    │ m3e-1 │    15-12edo (1500.00 cents) │
    │ M3e-1 │    16-12edo (1600.00 cents) │
    │ 4e-1  │    17-12edo (1700.00 cents) │
    │ A4e-1 │    18-12edo (1800.00 cents) │
    │ 5e-1  │    19-12edo (1900.00 cents) │
    │ m6e-1 │    20-12edo (2000.00 cents) │
    │ M6e-1 │    21-12edo (2100.00 cents) │
    │ m7e-1 │    22-12edo (2200.00 cents) │
    │ M7e-1 │    23-12edo (2300.00 cents) │
    │ 0-2   │   2 octaves (2400.00 cents) │
    │ m2e-2 │    25-12edo (2500.00 cents) │
    │ M2e-2 │    26-12edo (2600.00 cents) │
    │ m3e-2 │    27-12edo (2700.00 cents) │
    │ M3e-2 │    28-12edo (2800.00 cents) │
    │ 4e-2  │    29-12edo (2900.00 cents) │
    │ A4e-2 │    30-12edo (3000.00 cents) │
    │ 5e-2  │    31-12edo (3100.00 cents) │
    │ m6e-2 │    32-12edo (3200.00 cents) │
    │ M6e-2 │    33-12edo (3300.00 cents) │
    │ m7e-2 │    34-12edo (3400.00 cents) │
    │ M7e-2 │    35-12edo (3500.00 cents) │
    └───────┴─────────────────────────────┘ |}]
;;
