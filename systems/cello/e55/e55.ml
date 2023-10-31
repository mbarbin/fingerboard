let create () =
  Cello.fifth_system
    ~acoustic_interval:
      (Acoustic_interval.equal_division_of_the_octave ~divisor:55 ~number_of_divisions:32)
    ()
;;

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (Cello.Fingerboard_position_name.Edo55.all :> Cello.Fingerboard_position_name.t list)
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
    │      I │ A3   │ 220.00 │ P5 - 32-55edo │ 698   │
    │     II │ D3   │ 146.99 │ P5 - 32-55edo │ 698   │
    │    III │ G2   │  98.20 │ P5 - 32-55edo │ 698   │
    │     IV │ C2   │  65.61 │               │       │
    └────────┴──────┴────────┴───────────────┴───────┘

    ┌──────────┬───────┬───────────┐
    │      Pos │ Cents │  Interval │
    ├──────────┼───────┼───────────┤
    │        0 │     0 │    unison │
    │   A1-e55 │    87 │   4-55edo │
    │   m2-e55 │   109 │   5-55edo │
    │   M2-e55 │   196 │   9-55edo │
    │   d3-e55 │   218 │  10-55edo │
    │   A2-e55 │   284 │  13-55edo │
    │   m3-e55 │   305 │  14-55edo │
    │   M3-e55 │   393 │  18-55edo │
    │   d4-e55 │   415 │  19-55edo │
    │   A3-e55 │   480 │  22-55edo │
    │    4-e55 │   502 │  23-55edo │
    │   A4-e55 │   589 │  27-55edo │
    │   d5-e55 │   611 │  28-55edo │
    │    5-e55 │   698 │  32-55edo │
    │   A5-e55 │   785 │  36-55edo │
    │   m6-e55 │   807 │  37-55edo │
    │   M6-e55 │   895 │  41-55edo │
    │   d7-e55 │   916 │  42-55edo │
    │   m7-e55 │  1004 │  46-55edo │
    │   M7-e55 │  1091 │  50-55edo │
    │   d8-e55 │  1113 │  51-55edo │
    │      0-1 │  1200 │  1 octave │
    │ A1-e55-1 │  1287 │  59-55edo │
    │ m2-e55-1 │  1309 │  60-55edo │
    │ M2-e55-1 │  1396 │  64-55edo │
    │ d3-e55-1 │  1418 │  65-55edo │
    │ A2-e55-1 │  1484 │  68-55edo │
    │ m3-e55-1 │  1505 │  69-55edo │
    │ M3-e55-1 │  1593 │  73-55edo │
    │ d4-e55-1 │  1615 │  74-55edo │
    │ A3-e55-1 │  1680 │  77-55edo │
    │  4-e55-1 │  1702 │  78-55edo │
    │ A4-e55-1 │  1789 │  82-55edo │
    │ d5-e55-1 │  1811 │  83-55edo │
    │  5-e55-1 │  1898 │  87-55edo │
    │ A5-e55-1 │  1985 │  91-55edo │
    │ m6-e55-1 │  2007 │  92-55edo │
    │ M6-e55-1 │  2095 │  96-55edo │
    │ d7-e55-1 │  2116 │  97-55edo │
    │ m7-e55-1 │  2204 │ 101-55edo │
    │ M7-e55-1 │  2291 │ 105-55edo │
    │ d8-e55-1 │  2313 │ 106-55edo │
    │      0-2 │  2400 │ 2 octaves │
    │ A1-e55-2 │  2487 │ 114-55edo │
    │ m2-e55-2 │  2509 │ 115-55edo │
    │ M2-e55-2 │  2596 │ 119-55edo │
    │ d3-e55-2 │  2618 │ 120-55edo │
    │ A2-e55-2 │  2684 │ 123-55edo │
    │ m3-e55-2 │  2705 │ 124-55edo │
    │ M3-e55-2 │  2793 │ 128-55edo │
    │ d4-e55-2 │  2815 │ 129-55edo │
    │ A3-e55-2 │  2880 │ 132-55edo │
    │  4-e55-2 │  2902 │ 133-55edo │
    │ A4-e55-2 │  2989 │ 137-55edo │
    │ d5-e55-2 │  3011 │ 138-55edo │
    │  5-e55-2 │  3098 │ 142-55edo │
    │ A5-e55-2 │  3185 │ 146-55edo │
    │ m6-e55-2 │  3207 │ 147-55edo │
    │ M6-e55-2 │  3295 │ 151-55edo │
    │ d7-e55-2 │  3316 │ 152-55edo │
    │ m7-e55-2 │  3404 │ 156-55edo │
    │ M7-e55-2 │  3491 │ 160-55edo │
    │ d8-e55-2 │  3513 │ 161-55edo │
    └──────────┴───────┴───────────┘ |}]
;;

let%expect_test "approximating just intervals" =
  let module Kind = struct
    type t =
      | Exact
      | E55
      | E53
      | Equal_temperament
    [@@deriving enumerate, sexp_of]
  end
  in
  let module Row = struct
    type t =
      | Octave
      | Just_major_sixth
      | Just_minor_sixth
      | Fifth
      | Fourth
      | Just_major_third
      | Just_minor_third
      | Pythagorean_major_second
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
       | Just_major_sixth -> Acoustic_interval.just_major_sixth
       | Just_minor_sixth -> Acoustic_interval.just_minor_sixth
       | Fifth ->
         Acoustic_interval.pythagorean
           { number = Fifth; quality = Perfect; additional_octaves = 0 }
       | Fourth ->
         Acoustic_interval.pythagorean
           { number = Fourth; quality = Perfect; additional_octaves = 0 }
       | Just_major_third -> Acoustic_interval.just_major_third
       | Just_minor_third -> Acoustic_interval.just_minor_third
       | Pythagorean_major_second ->
         Acoustic_interval.pythagorean
           { number = Second; quality = Major; additional_octaves = 0 }
       | Pythagorean_diatonic_semiton -> Acoustic_interval.pythagorean_diatonic_semiton
       | Pythagorean_chromatic_semiton -> Acoustic_interval.pythagorean_chromatic_semiton
       | Just_diatonic_semiton -> Acoustic_interval.just_diatonic_semiton)
    | Equal_temperament ->
      let number_of_divisions =
        match interval with
        | Octave -> 12
        | Just_major_sixth -> 9
        | Just_minor_sixth -> 8
        | Fifth -> 7
        | Fourth -> 5
        | Just_major_third -> 4
        | Just_minor_third -> 3
        | Pythagorean_major_second -> 2
        | Pythagorean_diatonic_semiton
        | Pythagorean_chromatic_semiton
        | Just_diatonic_semiton -> 1
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions
    | E55 ->
      let number_of_divisions =
        match interval with
        | Octave -> 55
        | Just_major_sixth -> 41
        | Just_minor_sixth -> 37
        | Fifth -> 32
        | Fourth -> 23
        | Just_major_third -> 18
        | Just_minor_third -> 14
        | Pythagorean_major_second -> 9
        | Pythagorean_diatonic_semiton -> 4
        | Pythagorean_chromatic_semiton -> 5
        | Just_diatonic_semiton -> 5
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:55 ~number_of_divisions
    | E53 ->
      let number_of_divisions =
        match interval with
        | Octave -> 53
        | Just_major_sixth -> 39
        | Just_minor_sixth -> 36
        | Fifth -> 31
        | Fourth -> 22
        | Just_major_third -> 17
        | Just_minor_third -> 14
        | Pythagorean_major_second -> 9
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
    ┌───────────────────────────────┬───────┬──────┬──────┬───────────────────┐
    │ Interval                      │ Exact │  E55 │  E53 │ Equal_temperament │
    ├───────────────────────────────┼───────┼──────┼──────┼───────────────────┤
    │ Octave                        │  1200 │ 1200 │ 1200 │              1200 │
    │ Just major sixth              │   884 │  895 │  883 │               900 │
    │ Just minor sixth              │   814 │  807 │  815 │               800 │
    │ Fifth                         │   702 │  698 │  702 │               700 │
    │ Fourth                        │   498 │  502 │  498 │               500 │
    │ Just major third              │   386 │  393 │  385 │               400 │
    │ Just minor third              │   316 │  305 │  317 │               300 │
    │ Pythagorean major second      │   204 │  196 │  204 │               200 │
    │ Pythagorean diatonic semiton  │    90 │   87 │   91 │               100 │
    │ Pythagorean chromatic semiton │   114 │  109 │  113 │               100 │
    │ Just diatonic semiton         │   112 │  109 │  113 │               100 │
    └───────────────────────────────┴───────┴──────┴──────┴───────────────────┘ |}]
;;
