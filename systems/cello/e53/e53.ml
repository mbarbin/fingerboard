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

let%expect_test "tables" =
  let t = force t in
  print_endline (System.to_ascii_tables t);
  [%expect
    {|
    ┌────────┬──────┬────────┬───────────────┬───────┐
    │ String │ Note │  Pitch │ Interval      │ Cents │
    ├────────┼──────┼────────┼───────────────┼───────┤
    │      I │ A3   │ 220.00 │ P5 - 31-53edo │ 702   │
    │     II │ D3   │ 146.67 │ P5 - 31-53edo │ 702   │
    │    III │ G2   │  97.79 │ P5 - 31-53edo │ 702   │
    │     IV │ C2   │  65.19 │               │       │
    └────────┴──────┴────────┴───────────────┴───────┘

    ┌───────────┬───────┬───────────┐
    │       Pos │ Cents │  Interval │
    ├───────────┼───────┼───────────┤
    │         0 │     0 │    unison │
    │   A1z-e53 │    91 │   4-53edo │
    │   m2z-e53 │   113 │   5-53edo │
    │   M2z-e53 │   181 │   8-53edo │
    │   M2p-e53 │   204 │   9-53edo │
    │   m3p-e53 │   294 │  13-53edo │
    │   m3z-e53 │   317 │  14-53edo │
    │   M3z-e53 │   385 │  17-53edo │
    │   M3p-e53 │   408 │  18-53edo │
    │    4p-e53 │   498 │  22-53edo │
    │    4z-e53 │   521 │  23-53edo │
    │   A4z-e53 │   589 │  26-53edo │
    │   d5z-e53 │   611 │  27-53edo │
    │    5z-e53 │   679 │  30-53edo │
    │    5p-e53 │   702 │  31-53edo │
    │   m6p-e53 │   792 │  35-53edo │
    │   m6z-e53 │   815 │  36-53edo │
    │   M6z-e53 │   883 │  39-53edo │
    │   M6p-e53 │   906 │  40-53edo │
    │   m7p-e53 │   996 │  44-53edo │
    │   m7z-e53 │  1019 │  45-53edo │
    │   M7z-e53 │  1087 │  48-53edo │
    │   M7p-e53 │  1109 │  49-53edo │
    │    8z-e53 │  1177 │  52-53edo │
    │       0-1 │  1200 │  1 octave │
    │ A1z-e53-1 │  1291 │  57-53edo │
    │ m2z-e53-1 │  1313 │  58-53edo │
    │ M2z-e53-1 │  1381 │  61-53edo │
    │ M2p-e53-1 │  1404 │  62-53edo │
    │ m3p-e53-1 │  1494 │  66-53edo │
    │ m3z-e53-1 │  1517 │  67-53edo │
    │ M3z-e53-1 │  1585 │  70-53edo │
    │ M3p-e53-1 │  1608 │  71-53edo │
    │  4p-e53-1 │  1698 │  75-53edo │
    │  4z-e53-1 │  1721 │  76-53edo │
    │ A4z-e53-1 │  1789 │  79-53edo │
    │ d5z-e53-1 │  1811 │  80-53edo │
    │  5z-e53-1 │  1879 │  83-53edo │
    │  5p-e53-1 │  1902 │  84-53edo │
    │ m6p-e53-1 │  1992 │  88-53edo │
    │ m6z-e53-1 │  2015 │  89-53edo │
    │ M6z-e53-1 │  2083 │  92-53edo │
    │ M6p-e53-1 │  2106 │  93-53edo │
    │ m7p-e53-1 │  2196 │  97-53edo │
    │ m7z-e53-1 │  2219 │  98-53edo │
    │ M7z-e53-1 │  2287 │ 101-53edo │
    │ M7p-e53-1 │  2309 │ 102-53edo │
    │  8z-e53-1 │  2377 │ 105-53edo │
    │       0-2 │  2400 │ 2 octaves │
    │ A1z-e53-2 │  2491 │ 110-53edo │
    │ m2z-e53-2 │  2513 │ 111-53edo │
    │ M2z-e53-2 │  2581 │ 114-53edo │
    │ M2p-e53-2 │  2604 │ 115-53edo │
    │ m3p-e53-2 │  2694 │ 119-53edo │
    │ m3z-e53-2 │  2717 │ 120-53edo │
    │ M3z-e53-2 │  2785 │ 123-53edo │
    │ M3p-e53-2 │  2808 │ 124-53edo │
    │  4p-e53-2 │  2898 │ 128-53edo │
    │  4z-e53-2 │  2921 │ 129-53edo │
    │ A4z-e53-2 │  2989 │ 132-53edo │
    │ d5z-e53-2 │  3011 │ 133-53edo │
    │  5z-e53-2 │  3079 │ 136-53edo │
    │  5p-e53-2 │  3102 │ 137-53edo │
    │ m6p-e53-2 │  3192 │ 141-53edo │
    │ m6z-e53-2 │  3215 │ 142-53edo │
    │ M6z-e53-2 │  3283 │ 145-53edo │
    │ M6p-e53-2 │  3306 │ 146-53edo │
    │ m7p-e53-2 │  3396 │ 150-53edo │
    │ m7z-e53-2 │  3419 │ 151-53edo │
    │ M7z-e53-2 │  3487 │ 154-53edo │
    │ M7p-e53-2 │  3509 │ 155-53edo │
    │  8z-e53-2 │  3577 │ 158-53edo │
    └───────────┴───────┴───────────┘ |}]
;;
