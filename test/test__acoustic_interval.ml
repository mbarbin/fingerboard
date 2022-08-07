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
    │ Minor second │               100 │  112 │          90 │
    └──────────────┴───────────────────┴──────┴─────────────┘ |}]
;;
