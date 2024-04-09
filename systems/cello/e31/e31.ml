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
      (Acoustic_interval.equal_division_of_the_octave ~divisor:31 ~number_of_divisions:18)
    ()
;;

let add_positions t =
  List.iter
    ~f:(fun name -> Cello.add_fingerboard_position_exn t name)
    (Cello.Fingerboard_position_name.Edo31.all :> Cello.Fingerboard_position_name.t list)
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
    │      I │ A3   │ 220.00 │ P5 - 18-31edo │ 697   │
    │     II │ D3   │ 147.11 │ P5 - 18-31edo │ 697   │
    │    III │ G2   │  98.36 │ P5 - 18-31edo │ 697   │
    │     IV │ C2   │  65.77 │               │       │
    └────────┴──────┴────────┴───────────────┴───────┘

    ┌──────────┬───────┬───────────┐
    │      Pos │ Cents │  Interval │
    ├──────────┼───────┼───────────┤
    │        0 │     0 │    unison │
    │   A1-e31 │    77 │   2-31edo │
    │   m2-e31 │   116 │   3-31edo │
    │   M2-e31 │   194 │   5-31edo │
    │   d3-e31 │   232 │   6-31edo │
    │   A2-e31 │   271 │   7-31edo │
    │   m3-e31 │   310 │   8-31edo │
    │   M3-e31 │   387 │  10-31edo │
    │   d4-e31 │   426 │  11-31edo │
    │   A3-e31 │   465 │  12-31edo │
    │    4-e31 │   503 │  13-31edo │
    │   A4-e31 │   581 │  15-31edo │
    │   d5-e31 │   619 │  16-31edo │
    │    5-e31 │   697 │  18-31edo │
    │   A5-e31 │   774 │  20-31edo │
    │   m6-e31 │   813 │  21-31edo │
    │   M6-e31 │   890 │  23-31edo │
    │   d7-e31 │   929 │  24-31edo │
    │   m7-e31 │  1006 │  26-31edo │
    │   M7-e31 │  1084 │  28-31edo │
    │   d8-e31 │  1123 │  29-31edo │
    │      0-1 │  1200 │  1 octave │
    │ A1-e31-1 │  1277 │  33-31edo │
    │ m2-e31-1 │  1316 │  34-31edo │
    │ M2-e31-1 │  1394 │  36-31edo │
    │ d3-e31-1 │  1432 │  37-31edo │
    │ A2-e31-1 │  1471 │  38-31edo │
    │ m3-e31-1 │  1510 │  39-31edo │
    │ M3-e31-1 │  1587 │  41-31edo │
    │ d4-e31-1 │  1626 │  42-31edo │
    │ A3-e31-1 │  1665 │  43-31edo │
    │  4-e31-1 │  1703 │  44-31edo │
    │ A4-e31-1 │  1781 │  46-31edo │
    │ d5-e31-1 │  1819 │  47-31edo │
    │  5-e31-1 │  1897 │  49-31edo │
    │ A5-e31-1 │  1974 │  51-31edo │
    │ m6-e31-1 │  2013 │  52-31edo │
    │ M6-e31-1 │  2090 │  54-31edo │
    │ d7-e31-1 │  2129 │  55-31edo │
    │ m7-e31-1 │  2206 │  57-31edo │
    │ M7-e31-1 │  2284 │  59-31edo │
    │ d8-e31-1 │  2323 │  60-31edo │
    │      0-2 │  2400 │ 2 octaves │
    │ A1-e31-2 │  2477 │  64-31edo │
    │ m2-e31-2 │  2516 │  65-31edo │
    │ M2-e31-2 │  2594 │  67-31edo │
    │ d3-e31-2 │  2632 │  68-31edo │
    │ A2-e31-2 │  2671 │  69-31edo │
    │ m3-e31-2 │  2710 │  70-31edo │
    │ M3-e31-2 │  2787 │  72-31edo │
    │ d4-e31-2 │  2826 │  73-31edo │
    │ A3-e31-2 │  2865 │  74-31edo │
    │  4-e31-2 │  2903 │  75-31edo │
    │ A4-e31-2 │  2981 │  77-31edo │
    │ d5-e31-2 │  3019 │  78-31edo │
    │  5-e31-2 │  3097 │  80-31edo │
    │ A5-e31-2 │  3174 │  82-31edo │
    │ m6-e31-2 │  3213 │  83-31edo │
    │ M6-e31-2 │  3290 │  85-31edo │
    │ d7-e31-2 │  3329 │  86-31edo │
    │ m7-e31-2 │  3406 │  88-31edo │
    │ M7-e31-2 │  3484 │  90-31edo │
    │ d8-e31-2 │  3523 │  91-31edo │
    └──────────┴───────┴───────────┘ |}]
;;
