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

let create () = Cello.fifth_system ()

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (Cello.Fingerboard_position_name.Pythagorean.all
      :> Cello.Fingerboard_position_name.t list)
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
    ┌────────┬──────┬────────┬────────────┬───────┐
    │ String │ Note │  Pitch │ Interval   │ Cents │
    ├────────┼──────┼────────┼────────────┼───────┤
    │      I │ A3   │ 220.00 │ P5 - 3 / 2 │   702 │
    │     II │ D3   │ 146.67 │ P5 - 3 / 2 │   702 │
    │    III │ G2   │  97.78 │ P5 - 3 / 2 │   702 │
    │     IV │ C2   │  65.19 │            │       │
    └────────┴──────┴────────┴────────────┴───────┘

    ┌───────┬───────┬─────────────┐
    │   Pos │ Cents │    Interval │
    ├───────┼───────┼─────────────┤
    │     0 │     0 │      unison │
    │   m2p │    90 │   2^8 / 3^5 │
    │   A1p │   114 │  3^7 / 2^11 │
    │   d3p │   180 │ 2^16 / 3^10 │
    │   M2p │   204 │   3^2 / 2^3 │
    │   m3p │   294 │   2^5 / 3^3 │
    │   A2p │   318 │  3^9 / 2^14 │
    │   d4p │   384 │  2^13 / 3^8 │
    │   M3p │   408 │   3^4 / 2^6 │
    │    4p │   498 │     2^2 / 3 │
    │   A3p │   522 │ 3^11 / 2^17 │
    │   d5p │   588 │  2^10 / 3^6 │
    │   A4p │   612 │   3^6 / 2^9 │
    │   d6p │   678 │ 2^18 / 3^11 │
    │    5p │   702 │       3 / 2 │
    │   m6p │   792 │   2^7 / 3^4 │
    │   A5p │   816 │  3^8 / 2^12 │
    │   d7p │   882 │  2^15 / 3^9 │
    │   M6p │   906 │   3^3 / 2^4 │
    │   m7p │   996 │   2^4 / 3^2 │
    │   A6p │  1020 │ 3^10 / 2^15 │
    │   d8p │  1086 │  2^12 / 3^7 │
    │   M7p │  1110 │   3^5 / 2^7 │
    │   0-1 │  1200 │    1 octave │
    │ m2p-1 │  1290 │   2^9 / 3^5 │
    │ A1p-1 │  1314 │  3^7 / 2^10 │
    │ d3p-1 │  1380 │ 2^17 / 3^10 │
    │ M2p-1 │  1404 │   3^2 / 2^2 │
    │ m3p-1 │  1494 │   2^6 / 3^3 │
    │ A2p-1 │  1518 │  3^9 / 2^13 │
    │ d4p-1 │  1584 │  2^14 / 3^8 │
    │ M3p-1 │  1608 │   3^4 / 2^5 │
    │  4p-1 │  1698 │     2^3 / 3 │
    │ A3p-1 │  1722 │ 3^11 / 2^16 │
    │ d5p-1 │  1788 │  2^11 / 3^6 │
    │ A4p-1 │  1812 │   3^6 / 2^8 │
    │ d6p-1 │  1878 │ 2^19 / 3^11 │
    │  5p-1 │  1902 │           3 │
    │ m6p-1 │  1992 │   2^8 / 3^4 │
    │ A5p-1 │  2016 │  3^8 / 2^11 │
    │ d7p-1 │  2082 │  2^16 / 3^9 │
    │ M6p-1 │  2106 │   3^3 / 2^3 │
    │ m7p-1 │  2196 │   2^5 / 3^2 │
    │ A6p-1 │  2220 │ 3^10 / 2^14 │
    │ d8p-1 │  2286 │  2^13 / 3^7 │
    │ M7p-1 │  2310 │   3^5 / 2^6 │
    │   0-2 │  2400 │   2 octaves │
    │ m2p-2 │  2490 │  2^10 / 3^5 │
    │ A1p-2 │  2514 │   3^7 / 2^9 │
    │ d3p-2 │  2580 │ 2^18 / 3^10 │
    │ M2p-2 │  2604 │     3^2 / 2 │
    │ m3p-2 │  2694 │   2^7 / 3^3 │
    │ A2p-2 │  2718 │  3^9 / 2^12 │
    │ d4p-2 │  2784 │  2^15 / 3^8 │
    │ M3p-2 │  2808 │   3^4 / 2^4 │
    │  4p-2 │  2898 │     2^4 / 3 │
    │ A3p-2 │  2922 │ 3^11 / 2^15 │
    │ d5p-2 │  2988 │  2^12 / 3^6 │
    │ A4p-2 │  3012 │   3^6 / 2^7 │
    │ d6p-2 │  3078 │ 2^20 / 3^11 │
    │  5p-2 │  3102 │     (2 * 3) │
    │ m6p-2 │  3192 │   2^9 / 3^4 │
    │ A5p-2 │  3216 │  3^8 / 2^10 │
    │ d7p-2 │  3282 │  2^17 / 3^9 │
    │ M6p-2 │  3306 │   3^3 / 2^2 │
    │ m7p-2 │  3396 │   2^6 / 3^2 │
    │ A6p-2 │  3420 │ 3^10 / 2^13 │
    │ d8p-2 │  3486 │  2^14 / 3^7 │
    │ M7p-2 │  3510 │   3^5 / 2^5 │
    └───────┴───────┴─────────────┘ |}]
;;
