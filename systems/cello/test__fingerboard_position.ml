open! Core
open! Cemper

let fingerboard_positions =
  List.map Cello.Fingerboard_position_name.all ~f:(fun t ->
    t, Cello.Fingerboard_position_name.acoustic_interval_to_the_open_string t)
  |> List.sort ~compare:(fun (_, i1) (_, i2) -> Acoustic_interval.compare i1 i2)
;;

let%expect_test "positions and cents" =
  let module Row = struct
    type t =
      { name : Cello.Fingerboard_position_name.t
      ; acoustic_interval_to_the_open_string : Acoustic_interval.t
      }
  end
  in
  let columns =
    Ascii_table.Column.
      [ create_attr "Position" (fun (t : Row.t) ->
          [], Cello.Fingerboard_position_name.to_string t.name)
      ; create_attr ~align:Right "Cents" (fun (t : Row.t) ->
          ( []
          , t.acoustic_interval_to_the_open_string
            |> Acoustic_interval.to_cents
            |> sprintf "%0.2f" ))
      ]
  in
  let rows =
    List.map fingerboard_positions ~f:(fun (name, acoustic_interval_to_the_open_string) ->
      { Row.name; acoustic_interval_to_the_open_string })
  in
  Ascii_table.to_string columns rows |> print_endline;
  [%expect
    {|
    ┌──────────┬─────────┐
    │ Position │   Cents │
    ├──────────┼─────────┤
    │ 0        │    0.00 │
    │ m2p      │   90.22 │
    │ A1z-e53  │   90.57 │
    │ m2e      │  100.00 │
    │ m2z      │  111.73 │
    │ m2z-e53  │  113.21 │
    │ A1p      │  113.69 │
    │ d3p      │  180.45 │
    │ M2z-e53  │  181.13 │
    │ M2z      │  182.40 │
    │ M2e      │  200.00 │
    │ M2p-e53  │  203.77 │
    │ M2p      │  203.91 │
    │ m3p      │  294.13 │
    │ m3p-e53  │  294.34 │
    │ m3e      │  300.00 │
    │ m3z      │  315.64 │
    │ m3z-e53  │  316.98 │
    │ A2p      │  317.60 │
    │ d4p      │  384.36 │
    │ M3z-e53  │  384.91 │
    │ M3z      │  386.31 │
    │ M3e      │  400.00 │
    │ M3p-e53  │  407.55 │
    │ M3p      │  407.82 │
    │ 4p       │  498.04 │
    │ 4p-e53   │  498.11 │
    │ 4e       │  500.00 │
    │ 4z       │  519.55 │
    │ 4z-e53   │  520.75 │
    │ A3p      │  521.51 │
    │ d5p      │  588.27 │
    │ A4z-e53  │  588.68 │
    │ A4e      │  600.00 │
    │ d5z      │  609.78 │
    │ d5z-e53  │  611.32 │
    │ A4p      │  611.73 │
    │ d6p      │  678.49 │
    │ 5z-e53   │  679.25 │
    │ 5z       │  680.45 │
    │ 5e       │  700.00 │
    │ 5p-e53   │  701.89 │
    │ 5p       │  701.96 │
    │ m6p      │  792.18 │
    │ m6p-e53  │  792.45 │
    │ m6e      │  800.00 │
    │ m6z      │  813.69 │
    │ m6z-e53  │  815.09 │
    │ A5p      │  815.64 │
    │ d7p      │  882.40 │
    │ M6e      │  900.00 │
    │ M6p      │  905.87 │
    │ m7p      │  996.09 │
    │ m7p-e53  │  996.23 │
    │ m7e      │ 1000.00 │
    │ m7z-e53  │ 1018.87 │
    │ A6p      │ 1019.55 │
    │ d8p      │ 1086.31 │
    │ M7z-e53  │ 1086.79 │
    │ M7e      │ 1100.00 │
    │ d8z      │ 1107.82 │
    │ M7p-e53  │ 1109.43 │
    │ M7p      │ 1109.78 │
    │ 8z-e53   │ 1177.36 │
    │ 8z       │ 1178.49 │
    └──────────┴─────────┘ |}];
  ()
;;
