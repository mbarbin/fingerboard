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

let%expect_test "first comparison" =
  let module Kind = struct
    type t =
      | Equal_temperament
      | Just
      | Pythagorean
      | E53
      | E55
    [@@deriving enumerate]

    let constructor_name = function
      | Equal_temperament -> "Equal_temperament"
      | Just -> "Just"
      | Pythagorean -> "Pythagorean"
      | E53 -> "E53"
      | E55 -> "E55"
    ;;
  end
  in
  let module Row = struct
    type t = { interval : Interval.t }
  end
  in
  let acoustic_interval (t : Row.t) (kind : Kind.t) =
    let unimplemented () =
      Code_error.raise "Unimplemented" [ "interval", t.interval |> Interval.to_dyn ]
    in
    match (kind : Kind.t) with
    | Equal_temperament -> Acoustic_interval.equal_tempered_12 t.interval
    | Pythagorean -> Acoustic_interval.pythagorean t.interval
    | Just ->
      (match t.interval.number with
       | Octave | Fifth | Fourth ->
         assert (Interval.Quality.equal t.interval.quality Perfect);
         Acoustic_interval.pythagorean t.interval
       | Second ->
         (match t.interval.quality with
          | Minor -> Acoustic_interval.just_diatonic_semiton
          | Major -> Acoustic_interval.just_major_ton
          | _ -> unimplemented ())
       | Third ->
         (match t.interval.quality with
          | Minor -> Acoustic_interval.just_minor_third
          | Major -> Acoustic_interval.just_major_third
          | _ -> unimplemented ())
       | Sixth ->
         (match t.interval.quality with
          | Minor -> Acoustic_interval.just_minor_sixth
          | Major -> Acoustic_interval.just_major_sixth
          | _ -> unimplemented ())
       | _ -> unimplemented ())
    | E53 ->
      let number_of_divisions =
        match t.interval.number with
        | Octave ->
          assert (Interval.Quality.equal t.interval.quality Perfect);
          53
        | Fifth ->
          assert (Interval.Quality.equal t.interval.quality Perfect);
          31
        | Fourth ->
          assert (Interval.Quality.equal t.interval.quality Perfect);
          22
        | Second ->
          (match t.interval.quality with
           | Minor -> 5
           | Major -> 9
           | _ -> unimplemented ())
        | Third ->
          (match t.interval.quality with
           | Minor -> 14
           | Major -> 17
           | _ -> unimplemented ())
        | Sixth ->
          (match t.interval.quality with
           | Minor -> 36
           | Major -> 39
           | _ -> unimplemented ())
        | _ -> unimplemented ()
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions
    | E55 ->
      let number_of_divisions =
        match t.interval.number with
        | Octave ->
          assert (Interval.Quality.equal t.interval.quality Perfect);
          55
        | Fifth ->
          assert (Interval.Quality.equal t.interval.quality Perfect);
          32
        | Fourth ->
          assert (Interval.Quality.equal t.interval.quality Perfect);
          23
        | Second ->
          (match t.interval.quality with
           | Minor -> 5
           | Major -> 9
           | _ -> unimplemented ())
        | Third ->
          (match t.interval.quality with
           | Minor -> 14
           | Major -> 18
           | _ -> unimplemented ())
        | Sixth ->
          (match t.interval.quality with
           | Minor -> 37
           | Major -> 41
           | _ -> unimplemented ())
        | _ -> unimplemented ()
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:55 ~number_of_divisions
  in
  let columns =
    let cents_column kind =
      Print_table.Column.make
        ~align:Right
        ~header:(Kind.constructor_name kind)
        (fun (t : Row.t) ->
           acoustic_interval t kind
           |> Acoustic_interval.to_cents
           |> Float.iround_exn ~dir:`Nearest
           |> Int.to_string
           |> fun i -> Print_table.Cell.text i)
    in
    Print_table.O.
      [ [ Column.make ~header:"Interval" (fun (t : Row.t) ->
            Cell.text (Interval.name t.interval |> String.capitalize))
        ]
      ; List.map Kind.all ~f:cents_column
      ]
    |> List.concat
  in
  let rows =
    Row.
      [ { interval =
            { Interval.number = Octave; quality = Perfect; additional_octaves = 0 }
        }
      ; { interval = { Interval.number = Sixth; quality = Major; additional_octaves = 0 }
        }
      ; { interval = { Interval.number = Sixth; quality = Minor; additional_octaves = 0 }
        }
      ; { interval =
            { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 }
        }
      ; { interval =
            { Interval.number = Fourth; quality = Perfect; additional_octaves = 0 }
        }
      ; { interval = { Interval.number = Third; quality = Major; additional_octaves = 0 }
        }
      ; { interval = { Interval.number = Third; quality = Minor; additional_octaves = 0 }
        }
      ; { interval = { Interval.number = Second; quality = Major; additional_octaves = 0 }
        }
      ; { interval = { Interval.number = Second; quality = Minor; additional_octaves = 0 }
        }
      ]
  in
  Print_table.to_string_text (Print_table.make ~columns ~rows) |> print_endline;
  [%expect
    {|
    ┌──────────────┬───────────────────┬──────┬─────────────┬──────┬──────┐
    │ Interval     │ Equal_temperament │ Just │ Pythagorean │  E53 │  E55 │
    ├──────────────┼───────────────────┼──────┼─────────────┼──────┼──────┤
    │ Octave       │              1200 │ 1200 │        1200 │ 1200 │ 1200 │
    │ Major sixth  │               900 │  884 │         906 │  883 │  895 │
    │ Minor sixth  │               800 │  814 │         792 │  815 │  807 │
    │ Fifth        │               700 │  702 │         702 │  702 │  698 │
    │ Fourth       │               500 │  498 │         498 │  498 │  502 │
    │ Major third  │               400 │  386 │         408 │  385 │  393 │
    │ Minor third  │               300 │  316 │         294 │  317 │  305 │
    │ Major second │               200 │  204 │         204 │  204 │  196 │
    │ Minor second │               100 │  112 │          90 │  113 │  109 │
    └──────────────┴───────────────────┴──────┴─────────────┴──────┴──────┘ |}]
;;

let%expect_test "harmonic series and cents" =
  let module Row = struct
    type t =
      { harmonic : int
      ; interval : Interval.t
      }
  end
  in
  let columns =
    Print_table.O.
      [ Column.make ~header:"Harmonic" (fun (t : Row.t) ->
          Cell.text (Int.to_string t.harmonic))
      ; Column.make ~header:"Interval Above Fundamental" (fun (t : Row.t) ->
          Cell.text (Interval.name t.interval))
      ; Column.make
          ~align:Right
          ~header:"Deviation in Cents From Equal"
          (fun (t : Row.t) ->
             let harmonic =
               Acoustic_interval.small_natural_ratio_exn
                 ~numerator:t.harmonic
                 ~denominator:1
             in
             let equal = Acoustic_interval.equal_tempered_12 t.interval in
             let deviation =
               Acoustic_interval.to_cents harmonic -. Acoustic_interval.to_cents equal
               |> Float.iround_exn ~dir:`Nearest
             in
             Cell.text
               (if deviation > 0
                then "+" ^ Int.to_string deviation
                else Int.to_string deviation))
      ]
  in
  let rows =
    Row.
      [ { harmonic = 1
        ; interval = { number = Unison; quality = Perfect; additional_octaves = 0 }
        }
      ; { harmonic = 2
        ; interval = { number = Unison; quality = Perfect; additional_octaves = 1 }
        }
      ; { harmonic = 3
        ; interval = { number = Fifth; quality = Perfect; additional_octaves = 1 }
        }
      ; { harmonic = 4
        ; interval = { number = Unison; quality = Perfect; additional_octaves = 2 }
        }
      ; { harmonic = 5
        ; interval = { number = Third; quality = Major; additional_octaves = 2 }
        }
      ; { harmonic = 6
        ; interval = { number = Fifth; quality = Perfect; additional_octaves = 2 }
        }
      ; { harmonic = 7
        ; interval = { number = Seventh; quality = Minor; additional_octaves = 2 }
        }
      ; { harmonic = 8
        ; interval = { number = Unison; quality = Perfect; additional_octaves = 3 }
        }
      ]
  in
  Print_table.to_string_text (Print_table.make ~columns ~rows) |> print_endline;
  [%expect
    {|
    ┌──────────┬────────────────────────────┬───────────────────────────────┐
    │ Harmonic │ Interval Above Fundamental │ Deviation in Cents From Equal │
    ├──────────┼────────────────────────────┼───────────────────────────────┤
    │ 1        │ unison                     │                             0 │
    │ 2        │ octave                     │                             0 │
    │ 3        │ octave + fifth             │                            +2 │
    │ 4        │ 2 octaves                  │                             0 │
    │ 5        │ 2 octaves + major third    │                           -14 │
    │ 6        │ 2 octaves + fifth          │                            +2 │
    │ 7        │ 2 octaves + minor seventh  │                           -31 │
    │ 8        │ 3 octaves                  │                             0 │
    └──────────┴────────────────────────────┴───────────────────────────────┘ |}]
;;

let%expect_test "harmonic series and cents bis" =
  let module Row = struct
    type t =
      { harmonic : int
      ; cents : Cents.t
      }
  end
  in
  let columns =
    Print_table.O.
      [ Column.make ~header:"Harmonic" (fun (t : Row.t) ->
          Cell.text (Int.to_string t.harmonic))
      ; Column.make ~header:"Cents" (fun (t : Row.t) ->
          Cell.text (t.cents |> Cents.to_string_nearest))
      ]
  in
  let rows =
    List.init 16 ~f:(fun i ->
      let i = i + 1 in
      let cents =
        let harmonic =
          Acoustic_interval.small_natural_ratio_exn ~numerator:i ~denominator:1
        in
        Acoustic_interval.to_cents harmonic %. 1200.
      in
      { Row.harmonic = i; cents })
  in
  Print_table.to_string_text (Print_table.make ~columns ~rows) |> print_endline;
  [%expect
    {|
    ┌──────────┬───────┐
    │ Harmonic │ Cents │
    ├──────────┼───────┤
    │ 1        │ 0     │
    │ 2        │ 0     │
    │ 3        │ 702   │
    │ 4        │ 0     │
    │ 5        │ 386   │
    │ 6        │ 702   │
    │ 7        │ 969   │
    │ 8        │ 0     │
    │ 9        │ 204   │
    │ 10       │ 386   │
    │ 11       │ 551   │
    │ 12       │ 702   │
    │ 13       │ 841   │
    │ 14       │ 969   │
    │ 15       │ 1088  │
    │ 16       │ 0     │
    └──────────┴───────┘
    |}]
;;

let%expect_test "equal corner cases" =
  let base =
    Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:52
  in
  let i1 =
    Acoustic_interval.(
      add base (equal_division_of_the_octave ~divisor:53 ~number_of_divisions:9))
  in
  let i2 =
    Acoustic_interval.(
      add octave (equal_division_of_the_octave ~divisor:53 ~number_of_divisions:8))
  in
  print_s [%sexp (Acoustic_interval.equal i1 i2 : bool)];
  [%expect {| true |}];
  let i3 =
    Acoustic_interval.(
      remove i2 (equal_division_of_the_octave ~divisor:53 ~number_of_divisions:9))
    |> Option.value_exn ~here:[%here]
  in
  print_s [%sexp (Acoustic_interval.equal i3 base : bool)];
  [%expect {| true |}];
  ()
;;

let%expect_test "ratios" =
  let module Interval_kind = struct
    type t =
      | Unison
      | Syntonic_comma
      | Pythagorean_comma
      | Pythagorean_diatonic_semiton
      | Just_diatonic_semiton
      | Pythagorean_chromatic_semiton
      | Just_minor_ton
      | Just_major_ton
      | Pythagorean_minor_third
      | Just_minor_third
      | Just_major_third
      | Pythagorean_major_third
      | Fourth
      | Fifth
      | Pythagorean_minor_sixth
      | Just_minor_sixth
      | Just_major_sixth
      | Pythagorean_major_sixth
      | Pythagorean_minor_seventh
      | Minor_seventh_as_fifth_plus_just_minor_third
      | Major_seventh_as_fifth_plus_just_major_third
      | Pythagorean_major_seventh
      | Octave
    [@@deriving enumerate]

    let constructor_name = function
      | Unison -> "Unison"
      | Syntonic_comma -> "Syntonic_comma"
      | Pythagorean_comma -> "Pythagorean_comma"
      | Pythagorean_diatonic_semiton -> "Pythagorean_diatonic_semiton"
      | Just_diatonic_semiton -> "Just_diatonic_semiton"
      | Pythagorean_chromatic_semiton -> "Pythagorean_chromatic_semiton"
      | Just_minor_ton -> "Just_minor_ton"
      | Just_major_ton -> "Just_major_ton"
      | Pythagorean_minor_third -> "Pythagorean_minor_third"
      | Just_minor_third -> "Just_minor_third"
      | Just_major_third -> "Just_major_third"
      | Pythagorean_major_third -> "Pythagorean_major_third"
      | Fourth -> "Fourth"
      | Fifth -> "Fifth"
      | Pythagorean_minor_sixth -> "Pythagorean_minor_sixth"
      | Just_minor_sixth -> "Just_minor_sixth"
      | Just_major_sixth -> "Just_major_sixth"
      | Pythagorean_major_sixth -> "Pythagorean_major_sixth"
      | Pythagorean_minor_seventh -> "Pythagorean_minor_seventh"
      | Minor_seventh_as_fifth_plus_just_minor_third ->
        "Minor_seventh_as_fifth_plus_just_minor_third"
      | Major_seventh_as_fifth_plus_just_major_third ->
        "Major_seventh_as_fifth_plus_just_major_third"
      | Pythagorean_major_seventh -> "Pythagorean_major_seventh"
      | Octave -> "Octave"
    ;;

    let pythagorean_comma =
      (* 6 tons minus an octave. *)
      [ Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:81 ~denominator:64
      ; Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:81 ~denominator:64
      ; Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:81 ~denominator:64
      ; Natural_ratio.Reduced.create_exn ~prime:2 ~exponent:(-1)
      ]
      |> Natural_ratio.Reduced.compound
    ;;

    let syntonic_comma =
      (* 2 P tons minus a Just major third *)
      [ Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:9 ~denominator:8
      ; Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:9 ~denominator:8
      ; Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:4 ~denominator:5
      ]
      |> Natural_ratio.Reduced.compound
    ;;

    let acoustic_interval = function
      | Unison -> Acoustic_interval.unison
      | Syntonic_comma -> Acoustic_interval.reduced_natural_ratio syntonic_comma
      | Pythagorean_comma -> Acoustic_interval.reduced_natural_ratio pythagorean_comma
      | Pythagorean_diatonic_semiton -> Acoustic_interval.pythagorean_diatonic_semiton
      | Just_diatonic_semiton -> Acoustic_interval.just_diatonic_semiton
      | Pythagorean_chromatic_semiton -> Acoustic_interval.pythagorean_chromatic_semiton
      | Just_minor_ton -> Acoustic_interval.just_minor_ton
      | Just_major_ton -> Acoustic_interval.just_major_ton
      | Pythagorean_minor_third ->
        Acoustic_interval.pythagorean
          { number = Third; quality = Minor; additional_octaves = 0 }
      | Just_minor_third -> Acoustic_interval.just_minor_third
      | Just_major_third -> Acoustic_interval.just_major_third
      | Pythagorean_major_third ->
        Acoustic_interval.pythagorean
          { number = Third; quality = Major; additional_octaves = 0 }
      | Fourth ->
        Acoustic_interval.pythagorean
          { number = Fourth; quality = Perfect; additional_octaves = 0 }
      | Fifth ->
        Acoustic_interval.pythagorean
          { number = Fifth; quality = Perfect; additional_octaves = 0 }
      | Pythagorean_minor_sixth ->
        Acoustic_interval.pythagorean
          { number = Sixth; quality = Minor; additional_octaves = 0 }
      | Just_minor_sixth -> Acoustic_interval.just_minor_sixth
      | Just_major_sixth -> Acoustic_interval.just_major_sixth
      | Pythagorean_major_sixth ->
        Acoustic_interval.pythagorean
          { number = Sixth; quality = Major; additional_octaves = 0 }
      | Pythagorean_minor_seventh ->
        Acoustic_interval.pythagorean
          { number = Seventh; quality = Minor; additional_octaves = 0 }
      | Minor_seventh_as_fifth_plus_just_minor_third ->
        Acoustic_interval.(
          add
            (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
            just_minor_third)
      | Major_seventh_as_fifth_plus_just_major_third ->
        Acoustic_interval.(
          add
            (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
            just_major_third)
      | Pythagorean_major_seventh ->
        Acoustic_interval.pythagorean
          { number = Seventh; quality = Major; additional_octaves = 0 }
      | Octave -> Acoustic_interval.octave
    ;;

    let to_53_edo = function
      | Unison -> 0
      | Syntonic_comma -> 1
      | Pythagorean_comma -> 1
      | Pythagorean_diatonic_semiton -> 4
      | Just_diatonic_semiton -> 5
      | Pythagorean_chromatic_semiton -> 5
      | Just_minor_ton -> 8
      | Just_major_ton -> 9
      | Pythagorean_minor_third -> 13
      | Just_minor_third -> 14
      | Just_major_third -> 17
      | Pythagorean_major_third -> 18
      | Fourth -> 22
      | Fifth -> 31
      | Pythagorean_minor_sixth -> 35
      | Just_minor_sixth -> 36
      | Just_major_sixth -> 39
      | Pythagorean_major_sixth -> 40
      | Pythagorean_minor_seventh -> 44
      | Minor_seventh_as_fifth_plus_just_minor_third -> 45
      | Major_seventh_as_fifth_plus_just_major_third -> 48
      | Pythagorean_major_seventh -> 49
      | Octave -> 53
    ;;
  end
  in
  let module Row = struct
    type t =
      { interval_kind : Interval_kind.t
      ; acoustic_interval : Acoustic_interval.t
      ; reduced_natural_ratio : Natural_ratio.Reduced.t
      ; to_53_edo : int
      }
  end
  in
  let reduced_natural_ratio acoustic_interval =
    match (acoustic_interval : Acoustic_interval.t) with
    | Zero -> Natural_ratio.Reduced.one
    | Octaves { number_of_octaves } ->
      Natural_ratio.Reduced.create_exn ~prime:2 ~exponent:number_of_octaves
    | Reduced_natural_ratio nr -> nr
    | (Equal_division_of_the_octave _ | Cents _) as i ->
      Code_error.raise
        "Unexpected non ratio."
        [ "interval", i |> Acoustic_interval.to_dyn ]
  in
  let columns =
    Print_table.O.
      [ Column.make ~header:"Interval" (fun (t : Row.t) ->
          Cell.text (Interval_kind.constructor_name t.interval_kind))
      ; Column.make ~align:Right ~header:"Ratio" (fun (t : Row.t) ->
          Cell.text
            (Natural_ratio.to_string
               (Natural_ratio.Reduced.to_natural_ratio t.reduced_natural_ratio)))
      ; Column.make ~align:Right ~header:"Reduced ratio" (fun (t : Row.t) ->
          Cell.text (Natural_ratio.Reduced.to_string t.reduced_natural_ratio))
      ; Column.make ~align:Right ~header:"Cents" (fun (t : Row.t) ->
          Cell.text
            (t.acoustic_interval
             |> Acoustic_interval.to_cents
             |> Float.iround_exn ~dir:`Nearest
             |> Int.to_string))
      ; Column.make ~align:Right ~header:"53 EDO #" (fun (t : Row.t) ->
          Cell.text (Int.to_string t.to_53_edo))
      ; Column.make ~align:Right ~header:"53 EDO cents" (fun (t : Row.t) ->
          let cents =
            Acoustic_interval.equal_division_of_the_octave
              ~divisor:53
              ~number_of_divisions:t.to_53_edo
            |> Acoustic_interval.to_cents
          in
          let ref_cents = t.acoustic_interval |> Acoustic_interval.to_cents in
          let diff = cents -. ref_cents in
          let sign = if Float.compare diff 0. >= 0 then "+" else "" in
          Cell.text
            (Printf.sprintf
               "%d (%s%0.3f)"
               (Float.iround_exn ~dir:`Nearest cents)
               sign
               diff))
      ]
  in
  let rows =
    List.map Interval_kind.all ~f:(fun interval_kind ->
      let acoustic_interval = Interval_kind.acoustic_interval interval_kind in
      { Row.interval_kind
      ; acoustic_interval
      ; reduced_natural_ratio = reduced_natural_ratio acoustic_interval
      ; to_53_edo = Interval_kind.to_53_edo interval_kind
      })
  in
  Print_table.to_string_text (Print_table.make ~columns ~rows) |> print_endline;
  [%expect
    {|
    ┌──────────────────────────────────────────────┬─────────────────┬─────────────────┬───────┬──────────┬───────────────┐
    │ Interval                                     │           Ratio │   Reduced ratio │ Cents │ 53 EDO # │  53 EDO cents │
    ├──────────────────────────────────────────────┼─────────────────┼─────────────────┼───────┼──────────┼───────────────┤
    │ Unison                                       │               1 │               1 │     0 │        0 │    0 (+0.000) │
    │ Syntonic_comma                               │         81 / 80 │ 3^4 / (2^4 * 5) │    22 │        1 │   23 (+1.135) │
    │ Pythagorean_comma                            │ 531441 / 524288 │     3^12 / 2^19 │    23 │        1 │   23 (-0.819) │
    │ Pythagorean_diatonic_semiton                 │       256 / 243 │       2^8 / 3^5 │    90 │        4 │   91 (+0.341) │
    │ Just_diatonic_semiton                        │         16 / 15 │   2^4 / (3 * 5) │   112 │        5 │  113 (+1.476) │
    │ Pythagorean_chromatic_semiton                │     2187 / 2048 │      3^7 / 2^11 │   114 │        5 │  113 (-0.477) │
    │ Just_minor_ton                               │          10 / 9 │   (2 * 5) / 3^2 │   182 │        8 │  181 (-1.272) │
    │ Just_major_ton                               │           9 / 8 │       3^2 / 2^3 │   204 │        9 │  204 (-0.136) │
    │ Pythagorean_minor_third                      │         32 / 27 │       2^5 / 3^3 │   294 │       13 │  294 (+0.205) │
    │ Just_minor_third                             │           6 / 5 │     (2 * 3) / 5 │   316 │       14 │  317 (+1.340) │
    │ Just_major_third                             │           5 / 4 │         5 / 2^2 │   386 │       17 │  385 (-1.408) │
    │ Pythagorean_major_third                      │         81 / 64 │       3^4 / 2^6 │   408 │       18 │  408 (-0.273) │
    │ Fourth                                       │           4 / 3 │         2^2 / 3 │   498 │       22 │  498 (+0.068) │
    │ Fifth                                        │           3 / 2 │           3 / 2 │   702 │       31 │  702 (-0.068) │
    │ Pythagorean_minor_sixth                      │        128 / 81 │       2^7 / 3^4 │   792 │       35 │  792 (+0.273) │
    │ Just_minor_sixth                             │           8 / 5 │         2^3 / 5 │   814 │       36 │  815 (+1.408) │
    │ Just_major_sixth                             │           5 / 3 │           5 / 3 │   884 │       39 │  883 (-1.340) │
    │ Pythagorean_major_sixth                      │         27 / 16 │       3^3 / 2^4 │   906 │       40 │  906 (-0.205) │
    │ Pythagorean_minor_seventh                    │          16 / 9 │       2^4 / 3^2 │   996 │       44 │  996 (+0.136) │
    │ Minor_seventh_as_fifth_plus_just_minor_third │           9 / 5 │         3^2 / 5 │  1018 │       45 │ 1019 (+1.272) │
    │ Major_seventh_as_fifth_plus_just_major_third │          15 / 8 │   (3 * 5) / 2^3 │  1088 │       48 │ 1087 (-1.476) │
    │ Pythagorean_major_seventh                    │       243 / 128 │       3^5 / 2^7 │  1110 │       49 │ 1109 (-0.341) │
    │ Octave                                       │               2 │               2 │  1200 │       53 │ 1200 (+0.000) │
    └──────────────────────────────────────────────┴─────────────────┴─────────────────┴───────┴──────────┴───────────────┘ |}]
;;
