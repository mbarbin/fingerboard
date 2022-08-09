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
  let columns =
    let cents_column kind =
      Ascii_table.Column.create_attr
        ~align:Right
        (Sexp.to_string [%sexp (kind : Kind.t)])
        (fun (t : Row.t) ->
          let acoustic_interval =
            let unimplemented here =
              raise_s
                [%sexp
                  "Unimplemented"
                  , (here : Source_code_position.t)
                  , (t.interval : Interval.t)]
            in
            match (kind : Kind.t) with
            | Equal_temperament -> Acoustic_interval.Symbolic.Equal_tempered_12 t.interval
            | Pythagorean -> Acoustic_interval.Symbolic.Pythagorean t.interval
            | Just ->
              (match t.interval.number with
               | Octave | Fifth | Fourth ->
                 assert (Interval.Quality.equal t.interval.quality Perfect);
                 Acoustic_interval.Symbolic.Pythagorean t.interval
               | Second ->
                 (match t.interval.quality with
                  | Minor -> Acoustic_interval.Symbolic.Just_diatonic_semiton
                  | _ -> unimplemented [%here])
               | Third ->
                 (match t.interval.quality with
                  | Minor -> Acoustic_interval.Symbolic.Just_minor_third
                  | Major -> Acoustic_interval.Symbolic.Just_major_third
                  | _ -> unimplemented [%here])
               | Sixth ->
                 (match t.interval.quality with
                  | Minor -> Acoustic_interval.Symbolic.Just_minor_sixth
                  | Major -> Acoustic_interval.Symbolic.Just_major_sixth
                  | _ -> unimplemented [%here])
               | _ -> unimplemented [%here])
          in
          acoustic_interval
          |> Acoustic_interval.of_symbolic
          |> Acoustic_interval.cents
          |> Float.iround_exn ~dir:`Nearest
          |> Int.to_string
          |> fun i -> [], i)
    in
    Ascii_table.Column.(
      [ [ create_attr "Interval" (fun (t : Row.t) ->
            [], Interval.to_name t.interval |> String.capitalize)
        ]
      ; List.map Kind.all ~f:cents_column
      ]
      |> List.concat)
  in
  let rows =
    Row.
      [ { interval = { Interval.number = Octave; quality = Perfect } }
      ; { interval = { Interval.number = Sixth; quality = Major } }
      ; { interval = { Interval.number = Sixth; quality = Minor } }
      ; { interval = { Interval.number = Fifth; quality = Perfect } }
      ; { interval = { Interval.number = Fourth; quality = Perfect } }
      ; { interval = { Interval.number = Third; quality = Major } }
      ; { interval = { Interval.number = Third; quality = Minor } }
      ; { interval = { Interval.number = Second; quality = Minor } }
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
      ; octaves : int
      ; plus_interval : Interval.t
      }
  end
  in
  let columns =
    Ascii_table.Column.
      [ create_attr "Harmonic" (fun (t : Row.t) -> [], Int.to_string t.harmonic)
      ; create_attr "Interval Above Fundamental" (fun (t : Row.t) ->
          ( []
          , let octaves =
              match t.octaves with
              | 0 -> ""
              | 1 -> "octave"
              | n -> sprintf "%d octaves" n
            and interval =
              let name = Interval.to_name t.plus_interval in
              if Interval.number_of_semitons t.plus_interval = 0 then "" else name
            in
            match octaves, interval with
            | "", s | s, "" -> s
            | a, b -> a ^ " + " ^ b ))
      ; create_attr ~align:Right "Deviation in Cents From Equal" (fun (t : Row.t) ->
          let harmonic =
            Acoustic_interval.of_symbolic
              (Natural_ratio { numerator = t.harmonic; denominator = 1 })
          in
          let equal =
            Acoustic_interval.of_symbolic
              (Compound
                 (Equal_tempered_12 t.plus_interval
                 :: List.init
                      t.octaves
                      ~f:
                        (const
                           (Acoustic_interval.Symbolic.Equal_tempered_12
                              { number = Octave; quality = Perfect }))))
          in
          let deviation =
            Acoustic_interval.cents harmonic -. Acoustic_interval.cents equal
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
        ; octaves = 0
        ; plus_interval = { number = Unison; quality = Perfect }
        }
      ; { harmonic = 2
        ; octaves = 1
        ; plus_interval = { number = Unison; quality = Perfect }
        }
      ; { harmonic = 3
        ; octaves = 1
        ; plus_interval = { number = Fifth; quality = Perfect }
        }
      ; { harmonic = 4
        ; octaves = 2
        ; plus_interval = { number = Unison; quality = Perfect }
        }
      ; { harmonic = 5; octaves = 2; plus_interval = { number = Third; quality = Major } }
      ; { harmonic = 6
        ; octaves = 2
        ; plus_interval = { number = Fifth; quality = Perfect }
        }
      ; { harmonic = 7
        ; octaves = 2
        ; plus_interval = { number = Seventh; quality = Minor }
        }
      ; { harmonic = 8
        ; octaves = 3
        ; plus_interval = { number = Unison; quality = Perfect }
        }
      ]
  in
  Ascii_table.to_string columns rows |> print_endline;
  [%expect
    {|
    ┌──────────┬────────────────────────────┬───────────────────────────────┐
    │ Harmonic │ Interval Above Fundamental │ Deviation in Cents From Equal │
    ├──────────┼────────────────────────────┼───────────────────────────────┤
    │ 1        │                            │                             0 │
    │ 2        │ octave                     │                             0 │
    │ 3        │ octave + fifth             │                            +2 │
    │ 4        │ 2 octaves                  │                             0 │
    │ 5        │ 2 octaves + major third    │                           -14 │
    │ 6        │ 2 octaves + fifth          │                            +2 │
    │ 7        │ 2 octaves + minor seventh  │                           -31 │
    │ 8        │ 3 octaves                  │                             0 │
    └──────────┴────────────────────────────┴───────────────────────────────┘ |}]
;;
