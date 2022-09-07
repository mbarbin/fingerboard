open! Core
open! Cemper

let%expect_test "first comparison" =
  let module Kind = struct
    type t =
      | Equal_temperament
      | Just
      | Pythagorean
    [@@deriving enumerate, sexp_of]
  end
  in
  let module Row = struct
    type t = { interval : Interval.t }
  end
  in
  let acoustic_interval (t : Row.t) (kind : Kind.t) =
    let unimplemented here =
      raise_s
        [%sexp
          "Unimplemented", (here : Source_code_position.t), (t.interval : Interval.t)]
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
          | _ -> unimplemented [%here])
       | Third ->
         (match t.interval.quality with
          | Minor -> Acoustic_interval.just_minor_third
          | Major -> Acoustic_interval.just_major_third
          | _ -> unimplemented [%here])
       | Sixth ->
         (match t.interval.quality with
          | Minor -> Acoustic_interval.just_minor_sixth
          | Major -> Acoustic_interval.just_major_sixth
          | _ -> unimplemented [%here])
       | _ -> unimplemented [%here])
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
            [], Interval.name t.interval |> String.capitalize)
        ]
      ; List.map Kind.all ~f:cents_column
      ]
      |> List.concat)
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
      ; { interval = { Interval.number = Second; quality = Minor; additional_octaves = 0 }
        }
      ]
  in
  Ascii_table.to_string columns rows |> print_endline;
  [%expect
    {|
    ┌──────────────┬───────────────────┬──────┬─────────────┐
    │ Interval     │ Equal_temperament │ Just │ Pythagorean │
    ├──────────────┼───────────────────┼──────┼─────────────┤
    │ Octave       │              1200 │ 1200 │        1200 │
    │ Major sixth  │               900 │  884 │         906 │
    │ Minor sixth  │               800 │  814 │         792 │
    │ Fifth        │               700 │  702 │         702 │
    │ Fourth       │               500 │  498 │         498 │
    │ Major third  │               400 │  386 │         408 │
    │ Minor third  │               300 │  316 │         294 │
    │ Minor second │               100 │  112 │          90 │
    └──────────────┴───────────────────┴──────┴─────────────┘ |}]
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
    Ascii_table.Column.
      [ create_attr "Harmonic" (fun (t : Row.t) -> [], Int.to_string t.harmonic)
      ; create_attr "Interval Above Fundamental" (fun (t : Row.t) ->
          [], Interval.name t.interval)
      ; create_attr ~align:Right "Deviation in Cents From Equal" (fun (t : Row.t) ->
          let harmonic =
            Acoustic_interval.small_natural_ratio_exn ~numerator:t.harmonic ~denominator:1
          in
          let equal = Acoustic_interval.equal_tempered_12 t.interval in
          let deviation =
            Acoustic_interval.to_cents harmonic -. Acoustic_interval.to_cents equal
            |> Float.iround_exn ~dir:`Nearest
          in
          ( []
          , if deviation > 0
            then "+" ^ Int.to_string deviation
            else Int.to_string deviation ))
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
  Ascii_table.to_string columns rows |> print_endline;
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

let%expect_test "ratios" =
  let module Interval_kind = struct
    type t =
      | Unison
      | Just_diatonic_semiton
      | Just_minor_ton
      | Just_major_ton
      | Just_minor_third
      | Just_major_third
      | Fourth
      | Fifth
      | Just_minor_sixth
      | Just_major_sixth
      | Octave
    [@@deriving enumerate, sexp_of]

    let acoustic_interval = function
      | Unison -> Acoustic_interval.unison
      | Just_diatonic_semiton -> Acoustic_interval.just_diatonic_semiton
      | Just_minor_ton -> Acoustic_interval.just_minor_ton
      | Just_major_ton -> Acoustic_interval.just_major_ton
      | Just_minor_third -> Acoustic_interval.just_minor_third
      | Just_major_third -> Acoustic_interval.just_major_third
      | Fourth ->
        Acoustic_interval.pythagorean
          { number = Fourth; quality = Perfect; additional_octaves = 0 }
      | Fifth ->
        Acoustic_interval.pythagorean
          { number = Fifth; quality = Perfect; additional_octaves = 0 }
      | Just_minor_sixth -> Acoustic_interval.just_minor_sixth
      | Just_major_sixth -> Acoustic_interval.just_major_sixth
      | Octave -> Acoustic_interval.octave
    ;;
  end
  in
  let module Row = struct
    type t =
      { interval_kind : Interval_kind.t
      ; acoustic_interval : Acoustic_interval.t
      ; reduced_natural_ratio : Natural_ratio.Reduced.t
      }
  end
  in
  let reduced_natural_ratio acoustic_interval =
    match (acoustic_interval : Acoustic_interval.t) with
    | Zero -> Natural_ratio.Reduced.one
    | Reduced_natural_ratio nr -> nr
    | (Equal_division_of_the_octave _ | Cents _) as i ->
      raise_s [%sexp "Unexpected non ratio", [%here], (i : Acoustic_interval.t)]
  in
  let columns =
    Ascii_table.Column.
      [ create_attr "Interval" (fun (t : Row.t) ->
          [], Sexp.to_string [%sexp (t.interval_kind : Interval_kind.t)])
      ; create_attr ~align:Right "Ratio" (fun (t : Row.t) ->
          ( []
          , Natural_ratio.to_string
              (Natural_ratio.Reduced.to_natural_ratio t.reduced_natural_ratio) ))
      ; create_attr ~align:Right "Reduced ratio" (fun (t : Row.t) ->
          [], Natural_ratio.Reduced.to_string t.reduced_natural_ratio)
      ; create_attr ~align:Right "Cents" (fun (t : Row.t) ->
          ( []
          , t.acoustic_interval
            |> Acoustic_interval.to_cents
            |> Float.iround_exn ~dir:`Nearest
            |> Int.to_string ))
      ]
  in
  let rows =
    List.map Interval_kind.all ~f:(fun interval_kind ->
      let acoustic_interval = Interval_kind.acoustic_interval interval_kind in
      { Row.interval_kind
      ; acoustic_interval
      ; reduced_natural_ratio = reduced_natural_ratio acoustic_interval
      })
  in
  Ascii_table.to_string columns rows |> print_endline;
  [%expect
    {|
    ┌───────────────────────┬─────────┬─────────────────┬───────┐
    │ Interval              │   Ratio │   Reduced ratio │ Cents │
    ├───────────────────────┼─────────┼─────────────────┼───────┤
    │ Unison                │       1 │               1 │     0 │
    │ Just_diatonic_semiton │ 16 / 15 │ 2 ^ 4 / (3 * 5) │   112 │
    │ Just_minor_ton        │  10 / 9 │ (2 * 5) / 3 ^ 2 │   182 │
    │ Just_major_ton        │   9 / 8 │   3 ^ 2 / 2 ^ 3 │   204 │
    │ Just_minor_third      │   6 / 5 │     (2 * 3) / 5 │   316 │
    │ Just_major_third      │   5 / 4 │       5 / 2 ^ 2 │   386 │
    │ Fourth                │   4 / 3 │       2 ^ 2 / 3 │   498 │
    │ Fifth                 │   3 / 2 │           3 / 2 │   702 │
    │ Just_minor_sixth      │   8 / 5 │       2 ^ 3 / 5 │   814 │
    │ Just_major_sixth      │   5 / 3 │           5 / 3 │   884 │
    │ Octave                │       2 │               2 │  1200 │
    └───────────────────────┴─────────┴─────────────────┴───────┘ |}]
;;
