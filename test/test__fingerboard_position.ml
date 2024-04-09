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
            |> Printf.sprintf "%0.2f" ))
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
    │ A1-e19   │   63.16 │
    │ A1-e31   │   77.42 │
    │ A1-e55   │   87.27 │
    │ m2p      │   90.22 │
    │ A1z-e53  │   90.57 │
    │ A1z      │   92.18 │
    │ m2e      │  100.00 │
    │ m2-e55   │  109.09 │
    │ m2z      │  111.73 │
    │ m2z-e53  │  113.21 │
    │ A1p      │  113.69 │
    │ m2-e31   │  116.13 │
    │ m2-e19   │  126.32 │
    │ d3p      │  180.45 │
    │ M2z-e53  │  181.13 │
    │ M2z      │  182.40 │
    │ M2-e19   │  189.47 │
    │ M2-e31   │  193.55 │
    │ M2-e55   │  196.36 │
    │ M2e      │  200.00 │
    │ d3z      │  201.96 │
    │ M2p-e53  │  203.77 │
    │ M2p      │  203.91 │
    │ d3-e55   │  218.18 │
    │ d3-e31   │  232.26 │
    │ A2-e19   │  252.63 │
    │ A2-e31   │  270.97 │
    │ A2-e55   │  283.64 │
    │ m3p      │  294.13 │
    │ m3p-e53  │  294.34 │
    │ m3e      │  300.00 │
    │ m3-e55   │  305.45 │
    │ m3-e31   │  309.68 │
    │ m3z      │  315.64 │
    │ m3-e19   │  315.79 │
    │ m3z-e53  │  316.98 │
    │ A2p      │  317.60 │
    │ M3-e19   │  378.95 │
    │ d4p      │  384.36 │
    │ M3z-e53  │  384.91 │
    │ M3z      │  386.31 │
    │ M3-e31   │  387.10 │
    │ M3-e55   │  392.73 │
    │ M3e      │  400.00 │
    │ d4z      │  405.87 │
    │ M3p-e53  │  407.55 │
    │ M3p      │  407.82 │
    │ d4-e55   │  414.55 │
    │ d4-e31   │  425.81 │
    │ A3-e19   │  442.11 │
    │ A3-e31   │  464.52 │
    │ A3-e55   │  480.00 │
    │ 4p       │  498.04 │
    │ 4p-e53   │  498.11 │
    │ 4e       │  500.00 │
    │ 4-e55    │  501.82 │
    │ 4-e31    │  503.23 │
    │ 4-e19    │  505.26 │
    │ 4z       │  519.55 │
    │ 4z-e53   │  520.75 │
    │ A3p      │  521.51 │
    │ A4-e19   │  568.42 │
    │ A4-e31   │  580.65 │
    │ d5p      │  588.27 │
    │ A4z-e53  │  588.68 │
    │ A4-e55   │  589.09 │
    │ A4z      │  590.22 │
    │ A4e      │  600.00 │
    │ d5z      │  609.78 │
    │ d5-e55   │  610.91 │
    │ d5z-e53  │  611.32 │
    │ A4p      │  611.73 │
    │ d5-e31   │  619.35 │
    │ d5-e19   │  631.58 │
    │ d6p      │  678.49 │
    │ 5z-e53   │  679.25 │
    │ 5z       │  680.45 │
    │ 5-e19    │  694.74 │
    │ 5-e31    │  696.77 │
    │ 5-e55    │  698.18 │
    │ 5e       │  700.00 │
    │ 5p-e53   │  701.89 │
    │ 5p       │  701.96 │
    │ A5-e19   │  757.89 │
    │ A5-e31   │  774.19 │
    │ A5-e55   │  785.45 │
    │ m6p      │  792.18 │
    │ m6p-e53  │  792.45 │
    │ A5z      │  794.13 │
    │ m6e      │  800.00 │
    │ m6-e55   │  807.27 │
    │ m6-e31   │  812.90 │
    │ m6z      │  813.69 │
    │ m6z-e53  │  815.09 │
    │ A5p      │  815.64 │
    │ m6-e19   │  821.05 │
    │ d7p      │  882.40 │
    │ M6z-e53  │  883.02 │
    │ M6-e19   │  884.21 │
    │ M6z      │  884.36 │
    │ M6-e31   │  890.32 │
    │ M6-e55   │  894.55 │
    │ M6e      │  900.00 │
    │ d7z      │  903.91 │
    │ M6p-e53  │  905.66 │
    │ M6p      │  905.87 │
    │ d7-e55   │  916.36 │
    │ d7-e31   │  929.03 │
    │ d7-e19   │  947.37 │
    │ m7p      │  996.09 │
    │ m7p-e53  │  996.23 │
    │ m7e      │ 1000.00 │
    │ m7-e55   │ 1003.64 │
    │ m7-e31   │ 1006.45 │
    │ m7-e19   │ 1010.53 │
    │ m7z-e53  │ 1018.87 │
    │ A6p      │ 1019.55 │
    │ M7-e19   │ 1073.68 │
    │ M7-e31   │ 1083.87 │
    │ d8p      │ 1086.31 │
    │ M7z-e53  │ 1086.79 │
    │ M7z      │ 1088.27 │
    │ M7-e55   │ 1090.91 │
    │ M7e      │ 1100.00 │
    │ d8z      │ 1107.82 │
    │ M7p-e53  │ 1109.43 │
    │ M7p      │ 1109.78 │
    │ d8-e55   │ 1112.73 │
    │ d8-e31   │ 1122.58 │
    │ d8-e19   │ 1136.84 │
    │ 8z-e53   │ 1177.36 │
    │ 8z       │ 1178.49 │
    └──────────┴─────────┘ |}];
  ()
;;
