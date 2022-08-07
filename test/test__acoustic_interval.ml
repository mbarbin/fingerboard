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
    type t =
      { name : string
      ; interval : Interval.t
      }
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
            ignore t.name;
            [], Interval.to_name t.interval)
        ]
      ; List.map Kind.all ~f:cents_column
      ]
      |> List.concat)
  in
  let rows =
    Row.
      [ { name = "Octave"; interval = { Interval.number = Octave; quality = Perfect } }
      ; { name = "Fifth"; interval = { Interval.number = Fifth; quality = Perfect } }
      ; { name = "Fourth"; interval = { Interval.number = Fourth; quality = Perfect } }
      ; { name = "Major third"; interval = { Interval.number = Third; quality = Major } }
      ; { name = "Semiton"; interval = { Interval.number = Second; quality = Minor } }
      ]
  in
  Ascii_table.to_string columns rows |> print_endline;
  [%expect {|
    ┌────────────────┬───────────────────┬──────┬─────────────┐
    │ Interval       │ Equal_temperament │ Just │ Pythagorean │
    ├────────────────┼───────────────────┼──────┼─────────────┤
    │ perfect octave │              1200 │ 1200 │        1200 │
    │ perfect fifth  │               700 │  702 │         702 │
    │ perfect fourth │               500 │  498 │         498 │
    │ major third    │               400 │  386 │         408 │
    │ minor second   │               100 │  112 │          90 │
    └────────────────┴───────────────────┴──────┴─────────────┘ |}]
;;
