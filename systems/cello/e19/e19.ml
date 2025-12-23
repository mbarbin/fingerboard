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
      (Acoustic_interval.equal_division_of_the_octave ~divisor:19 ~number_of_divisions:11)
    ()
;;

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (Cello.Fingerboard_position_name.Edo19.all :> Cello.Fingerboard_position_name.t list)
;;

let t =
  lazy
    (let t = create () in
     add_positions t;
     t)
;;

let%expect_test "tables" =
  let t = Lazy.force t in
  print_endline (System.to_ascii_tables t);
  [%expect
    {|
    ┌────────┬──────┬────────┬───────────────┬───────┐
    │ String │ Note │  Pitch │ Interval      │ Cents │
    ├────────┼──────┼────────┼───────────────┼───────┤
    │      I │ A3   │ 220.00 │ P5 - 11-19edo │   695 │
    │     II │ D3   │ 147.28 │ P5 - 11-19edo │   695 │
    │    III │ G2   │  98.60 │ P5 - 11-19edo │   695 │
    │     IV │ C2   │  66.01 │               │       │
    └────────┴──────┴────────┴───────────────┴───────┘

    ┌──────────┬───────┬───────────┐
    │      Pos │ Cents │  Interval │
    ├──────────┼───────┼───────────┤
    │        0 │     0 │    unison │
    │   A1-e19 │    63 │   1-19edo │
    │   m2-e19 │   126 │   2-19edo │
    │   M2-e19 │   189 │   3-19edo │
    │   A2-e19 │   253 │   4-19edo │
    │   m3-e19 │   316 │   5-19edo │
    │   M3-e19 │   379 │   6-19edo │
    │   A3-e19 │   442 │   7-19edo │
    │    4-e19 │   505 │   8-19edo │
    │   A4-e19 │   568 │   9-19edo │
    │   d5-e19 │   632 │  10-19edo │
    │    5-e19 │   695 │  11-19edo │
    │   A5-e19 │   758 │  12-19edo │
    │   m6-e19 │   821 │  13-19edo │
    │   M6-e19 │   884 │  14-19edo │
    │   d7-e19 │   947 │  15-19edo │
    │   m7-e19 │  1011 │  16-19edo │
    │   M7-e19 │  1074 │  17-19edo │
    │   d8-e19 │  1137 │  18-19edo │
    │      0-1 │  1200 │  1 octave │
    │ A1-e19-1 │  1263 │  20-19edo │
    │ m2-e19-1 │  1326 │  21-19edo │
    │ M2-e19-1 │  1389 │  22-19edo │
    │ A2-e19-1 │  1453 │  23-19edo │
    │ m3-e19-1 │  1516 │  24-19edo │
    │ M3-e19-1 │  1579 │  25-19edo │
    │ A3-e19-1 │  1642 │  26-19edo │
    │  4-e19-1 │  1705 │  27-19edo │
    │ A4-e19-1 │  1768 │  28-19edo │
    │ d5-e19-1 │  1832 │  29-19edo │
    │  5-e19-1 │  1895 │  30-19edo │
    │ A5-e19-1 │  1958 │  31-19edo │
    │ m6-e19-1 │  2021 │  32-19edo │
    │ M6-e19-1 │  2084 │  33-19edo │
    │ d7-e19-1 │  2147 │  34-19edo │
    │ m7-e19-1 │  2211 │  35-19edo │
    │ M7-e19-1 │  2274 │  36-19edo │
    │ d8-e19-1 │  2337 │  37-19edo │
    │      0-2 │  2400 │ 2 octaves │
    │ A1-e19-2 │  2463 │  39-19edo │
    │ m2-e19-2 │  2526 │  40-19edo │
    │ M2-e19-2 │  2589 │  41-19edo │
    │ A2-e19-2 │  2653 │  42-19edo │
    │ m3-e19-2 │  2716 │  43-19edo │
    │ M3-e19-2 │  2779 │  44-19edo │
    │ A3-e19-2 │  2842 │  45-19edo │
    │  4-e19-2 │  2905 │  46-19edo │
    │ A4-e19-2 │  2968 │  47-19edo │
    │ d5-e19-2 │  3032 │  48-19edo │
    │  5-e19-2 │  3095 │  49-19edo │
    │ A5-e19-2 │  3158 │  50-19edo │
    │ m6-e19-2 │  3221 │  51-19edo │
    │ M6-e19-2 │  3284 │  52-19edo │
    │ d7-e19-2 │  3347 │  53-19edo │
    │ m7-e19-2 │  3411 │  54-19edo │
    │ M7-e19-2 │  3474 │  55-19edo │
    │ d8-e19-2 │  3537 │  56-19edo │
    └──────────┴───────┴───────────┘ |}]
;;
