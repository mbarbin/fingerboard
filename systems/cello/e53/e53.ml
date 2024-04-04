(**********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*                                                                                *)
(*  This file is part of Fingerboard.                                             *)
(*                                                                                *)
(*  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*  the terms of the GNU Affero General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or any later version.   *)
(*                                                                                *)
(*  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*  for more details.                                                             *)
(*                                                                                *)
(*  You should have received a copy of the GNU Affero General Public License      *)
(*  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(**********************************************************************************)

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

let%expect_test "approximating just and pythagorean intervals" =
  let module Kind = struct
    type t =
      | Exact
      | E53
      | Equal_temperament
    [@@deriving enumerate, sexp_of]
  end
  in
  let module Row = struct
    type t =
      | Octave
      | Pythagorean_major_sixth
      | Just_major_sixth
      | Pythagorean_minor_sixth
      | Just_minor_sixth
      | Fifth
      | Fourth
      | Pythagorean_major_third
      | Just_major_third
      | Pythagorean_minor_third
      | Just_minor_third
      | Pythagorean_major_second
      | Just_minor_ton
      | Pythagorean_diatonic_semiton
      | Pythagorean_chromatic_semiton
      | Just_diatonic_semiton
    [@@deriving enumerate, sexp_of]
  end
  in
  let acoustic_interval (interval : Row.t) (kind : Kind.t) =
    match (kind : Kind.t) with
    | Exact ->
      (match interval with
       | Octave -> Acoustic_interval.octave
       | Pythagorean_major_sixth ->
         Acoustic_interval.pythagorean
           { number = Sixth; quality = Major; additional_octaves = 0 }
       | Just_major_sixth -> Acoustic_interval.just_major_sixth
       | Pythagorean_minor_sixth ->
         Acoustic_interval.pythagorean
           { number = Sixth; quality = Minor; additional_octaves = 0 }
       | Just_minor_sixth -> Acoustic_interval.just_minor_sixth
       | Fifth ->
         Acoustic_interval.pythagorean
           { number = Fifth; quality = Perfect; additional_octaves = 0 }
       | Fourth ->
         Acoustic_interval.pythagorean
           { number = Fourth; quality = Perfect; additional_octaves = 0 }
       | Pythagorean_major_third ->
         Acoustic_interval.pythagorean
           { number = Third; quality = Major; additional_octaves = 0 }
       | Just_major_third -> Acoustic_interval.just_major_third
       | Pythagorean_minor_third ->
         Acoustic_interval.pythagorean
           { number = Third; quality = Minor; additional_octaves = 0 }
       | Just_minor_third -> Acoustic_interval.just_minor_third
       | Pythagorean_major_second ->
         Acoustic_interval.pythagorean
           { number = Second; quality = Major; additional_octaves = 0 }
       | Just_minor_ton -> Acoustic_interval.just_minor_ton
       | Pythagorean_diatonic_semiton -> Acoustic_interval.pythagorean_diatonic_semiton
       | Pythagorean_chromatic_semiton -> Acoustic_interval.pythagorean_chromatic_semiton
       | Just_diatonic_semiton -> Acoustic_interval.just_diatonic_semiton)
    | Equal_temperament ->
      let number_of_divisions =
        match interval with
        | Octave -> 12
        | Pythagorean_major_sixth | Just_major_sixth -> 9
        | Pythagorean_minor_sixth | Just_minor_sixth -> 8
        | Fifth -> 7
        | Fourth -> 5
        | Pythagorean_major_third | Just_major_third -> 4
        | Pythagorean_minor_third | Just_minor_third -> 3
        | Pythagorean_major_second | Just_minor_ton -> 2
        | Pythagorean_diatonic_semiton
        | Pythagorean_chromatic_semiton
        | Just_diatonic_semiton -> 1
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions
    | E53 ->
      let number_of_divisions =
        match interval with
        | Octave -> 53
        | Pythagorean_major_sixth -> 40
        | Just_major_sixth -> 39
        | Pythagorean_minor_sixth -> 35
        | Just_minor_sixth -> 36
        | Fifth -> 31
        | Fourth -> 22
        | Pythagorean_major_third -> 18
        | Just_major_third -> 17
        | Pythagorean_minor_third -> 13
        | Just_minor_third -> 14
        | Pythagorean_major_second -> 9
        | Just_minor_ton -> 8
        | Pythagorean_diatonic_semiton -> 4
        | Pythagorean_chromatic_semiton -> 5
        | Just_diatonic_semiton -> 5
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions
  in
  let columns =
    let cents_column kind =
      Ascii_table.Column.create_attr
        ~align:Right
        (Sexp.to_string [%sexp (kind : Kind.t)])
        (fun (t : Row.t) ->
          acoustic_interval t kind
          |> Acoustic_interval.to_cents
          |> Float.iround_exn ~dir:`Nearest
          |> Int.to_string
          |> fun i -> [], i)
    in
    Ascii_table.Column.(
      [ [ create_attr "Interval" (fun (t : Row.t) ->
            ( []
            , Sexp.to_string_hum [%sexp (t : Row.t)]
              |> String.substr_replace_all ~pattern:"_" ~with_:" " ))
        ]
      ; List.map Kind.all ~f:cents_column
      ]
      |> List.concat)
  in
  Ascii_table.to_string columns Row.all |> print_endline;
  [%expect
    {|
    ┌───────────────────────────────┬───────┬──────┬───────────────────┐
    │ Interval                      │ Exact │  E53 │ Equal_temperament │
    ├───────────────────────────────┼───────┼──────┼───────────────────┤
    │ Octave                        │  1200 │ 1200 │              1200 │
    │ Pythagorean major sixth       │   906 │  906 │               900 │
    │ Just major sixth              │   884 │  883 │               900 │
    │ Pythagorean minor sixth       │   792 │  792 │               800 │
    │ Just minor sixth              │   814 │  815 │               800 │
    │ Fifth                         │   702 │  702 │               700 │
    │ Fourth                        │   498 │  498 │               500 │
    │ Pythagorean major third       │   408 │  408 │               400 │
    │ Just major third              │   386 │  385 │               400 │
    │ Pythagorean minor third       │   294 │  294 │               300 │
    │ Just minor third              │   316 │  317 │               300 │
    │ Pythagorean major second      │   204 │  204 │               200 │
    │ Just minor ton                │   182 │  181 │               200 │
    │ Pythagorean diatonic semiton  │    90 │   91 │               100 │
    │ Pythagorean chromatic semiton │   114 │  113 │               100 │
    │ Just diatonic semiton         │   112 │  113 │               100 │
    └───────────────────────────────┴───────┴──────┴───────────────────┘ |}]
;;
