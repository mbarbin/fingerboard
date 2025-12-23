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
  let t = Lazy.force t in
  print_endline (System.to_ascii_tables t);
  [%expect
    {|
    ┌────────┬──────┬────────┬───────────────┬───────┐
    │ String │ Note │  Pitch │ Interval      │ Cents │
    ├────────┼──────┼────────┼───────────────┼───────┤
    │      I │ A3   │ 220.00 │ P5 - 32-55edo │   698 │
    │     II │ D3   │ 146.99 │ P5 - 32-55edo │   698 │
    │    III │ G2   │  98.20 │ P5 - 32-55edo │   698 │
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
