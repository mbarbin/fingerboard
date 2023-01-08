open! Core
open! Fingerboard

let create () =
  Cello.fifth_system
    ~acoustic_interval:
      (Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:31)
    ()
;;

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (Cello.Fingerboard_position_name.Edo53.all :> Cello.Fingerboard_position_name.t list)
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
       ((name M6z-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 39))))
       ((name M6p-e53) (at_octave 0)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 40))))
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
       ((name M6z-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 39))))
       ((name M6p-e53) (at_octave 1)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 40))))
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
       ((name M6z-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 39))))
       ((name M6p-e53) (at_octave 2)
        (basis_acoustic_interval_to_the_open_string
         (Equal_division_of_the_octave (divisor 53) (number_of_divisions 40))))
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

let%expect_test "tables" =
  let t = force t in
  print_endline (System.to_ascii_tables t);
  [%expect
    {|
    ┌────────┬──────┬────────┬─────────────────────┐
    │ String │ Note │  Pitch │ Interval going down │
    ├────────┼──────┼────────┼─────────────────────┤
    │ I      │ A3   │ 220.00 │ P5 - 31-53edo       │
    │ II     │ D3   │ 146.67 │ P5 - 31-53edo       │
    │ III    │ G2   │  97.79 │ P5 - 31-53edo       │
    │ IV     │ C2   │  65.19 │                     │
    └────────┴──────┴────────┴─────────────────────┘

    ┌───────────┬─────────────────────────────┐
    │ Name      │ Interval to the open string │
    ├───────────┼─────────────────────────────┤
    │ 0         │         unison (0.00 cents) │
    │ A1z-e53   │       4-53edo (90.57 cents) │
    │ m2z-e53   │      5-53edo (113.21 cents) │
    │ M2z-e53   │      8-53edo (181.13 cents) │
    │ M2p-e53   │      9-53edo (203.77 cents) │
    │ m3p-e53   │     13-53edo (294.34 cents) │
    │ m3z-e53   │     14-53edo (316.98 cents) │
    │ M3z-e53   │     17-53edo (384.91 cents) │
    │ M3p-e53   │     18-53edo (407.55 cents) │
    │ 4p-e53    │     22-53edo (498.11 cents) │
    │ 4z-e53    │     23-53edo (520.75 cents) │
    │ A4z-e53   │     26-53edo (588.68 cents) │
    │ d5z-e53   │     27-53edo (611.32 cents) │
    │ 5z-e53    │     30-53edo (679.25 cents) │
    │ 5p-e53    │     31-53edo (701.89 cents) │
    │ m6p-e53   │     35-53edo (792.45 cents) │
    │ m6z-e53   │     36-53edo (815.09 cents) │
    │ M6z-e53   │     39-53edo (883.02 cents) │
    │ M6p-e53   │     40-53edo (905.66 cents) │
    │ m7p-e53   │     44-53edo (996.23 cents) │
    │ m7z-e53   │    45-53edo (1018.87 cents) │
    │ M7z-e53   │    48-53edo (1086.79 cents) │
    │ M7p-e53   │    49-53edo (1109.43 cents) │
    │ 8z-e53    │    52-53edo (1177.36 cents) │
    │ 0-1       │    1 octave (1200.00 cents) │
    │ A1z-e53-1 │    57-53edo (1290.57 cents) │
    │ m2z-e53-1 │    58-53edo (1313.21 cents) │
    │ M2z-e53-1 │    61-53edo (1381.13 cents) │
    │ M2p-e53-1 │    62-53edo (1403.77 cents) │
    │ m3p-e53-1 │    66-53edo (1494.34 cents) │
    │ m3z-e53-1 │    67-53edo (1516.98 cents) │
    │ M3z-e53-1 │    70-53edo (1584.91 cents) │
    │ M3p-e53-1 │    71-53edo (1607.55 cents) │
    │ 4p-e53-1  │    75-53edo (1698.11 cents) │
    │ 4z-e53-1  │    76-53edo (1720.75 cents) │
    │ A4z-e53-1 │    79-53edo (1788.68 cents) │
    │ d5z-e53-1 │    80-53edo (1811.32 cents) │
    │ 5z-e53-1  │    83-53edo (1879.25 cents) │
    │ 5p-e53-1  │    84-53edo (1901.89 cents) │
    │ m6p-e53-1 │    88-53edo (1992.45 cents) │
    │ m6z-e53-1 │    89-53edo (2015.09 cents) │
    │ M6z-e53-1 │    92-53edo (2083.02 cents) │
    │ M6p-e53-1 │    93-53edo (2105.66 cents) │
    │ m7p-e53-1 │    97-53edo (2196.23 cents) │
    │ m7z-e53-1 │    98-53edo (2218.87 cents) │
    │ M7z-e53-1 │   101-53edo (2286.79 cents) │
    │ M7p-e53-1 │   102-53edo (2309.43 cents) │
    │ 8z-e53-1  │   105-53edo (2377.36 cents) │
    │ 0-2       │   2 octaves (2400.00 cents) │
    │ A1z-e53-2 │   110-53edo (2490.57 cents) │
    │ m2z-e53-2 │   111-53edo (2513.21 cents) │
    │ M2z-e53-2 │   114-53edo (2581.13 cents) │
    │ M2p-e53-2 │   115-53edo (2603.77 cents) │
    │ m3p-e53-2 │   119-53edo (2694.34 cents) │
    │ m3z-e53-2 │   120-53edo (2716.98 cents) │
    │ M3z-e53-2 │   123-53edo (2784.91 cents) │
    │ M3p-e53-2 │   124-53edo (2807.55 cents) │
    │ 4p-e53-2  │   128-53edo (2898.11 cents) │
    │ 4z-e53-2  │   129-53edo (2920.75 cents) │
    │ A4z-e53-2 │   132-53edo (2988.68 cents) │
    │ d5z-e53-2 │   133-53edo (3011.32 cents) │
    │ 5z-e53-2  │   136-53edo (3079.25 cents) │
    │ 5p-e53-2  │   137-53edo (3101.89 cents) │
    │ m6p-e53-2 │   141-53edo (3192.45 cents) │
    │ m6z-e53-2 │   142-53edo (3215.09 cents) │
    │ M6z-e53-2 │   145-53edo (3283.02 cents) │
    │ M6p-e53-2 │   146-53edo (3305.66 cents) │
    │ m7p-e53-2 │   150-53edo (3396.23 cents) │
    │ m7z-e53-2 │   151-53edo (3418.87 cents) │
    │ M7z-e53-2 │   154-53edo (3486.79 cents) │
    │ M7p-e53-2 │   155-53edo (3509.43 cents) │
    │ 8z-e53-2  │   158-53edo (3577.36 cents) │
    └───────────┴─────────────────────────────┘ |}]
;;
