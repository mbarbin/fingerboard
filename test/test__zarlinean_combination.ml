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

let%expect_test "combinations" =
  let all = Lazy.force Zarlinean_combination.all in
  List.iter all ~f:(fun t -> print_endline (Zarlinean_combination.to_ascii_tables t));
  [%expect
    {|
    ┌─────┬────┬─────┬────┬───┬────┬─────┐
    │ I   │ II │ III │ IV │ V │ VI │ VII │
    ├─────┼────┼─────┼────┼───┼────┼─────┤
    │ p,z │ p  │ z   │ z  │ p │ p  │ z   │
    └─────┴────┴─────┴────┴───┴────┴─────┘

    ┌─────┬────┬─────┬────┬───┬────┬─────┐
    │ I   │ II │ III │ IV │ V │ VI │ VII │
    ├─────┼────┼─────┼────┼───┼────┼─────┤
    │ p,z │ z  │ p   │ p  │ z │ z  │ p   │
    └─────┴────┴─────┴────┴───┴────┴─────┘

    ┌───┬─────┬─────┬────┬───┬────┬─────┐
    │ I │ II  │ III │ IV │ V │ VI │ VII │
    ├───┼─────┼─────┼────┼───┼────┼─────┤
    │ p │ p,z │ z   │ p  │ p │ z  │ z   │
    └───┴─────┴─────┴────┴───┴────┴─────┘

    ┌───┬─────┬─────┬────┬───┬────┬─────┐
    │ I │ II  │ III │ IV │ V │ VI │ VII │
    ├───┼─────┼─────┼────┼───┼────┼─────┤
    │ z │ p,z │ p   │ z  │ z │ p  │ p   │
    └───┴─────┴─────┴────┴───┴────┴─────┘

    ┌───┬────┬─────┬────┬───┬────┬─────┐
    │ I │ II │ III │ IV │ V │ VI │ VII │
    ├───┼────┼─────┼────┼───┼────┼─────┤
    │ p │ z  │ p,z │ p  │ z │ z  │ p   │
    └───┴────┴─────┴────┴───┴────┴─────┘

    ┌───┬────┬─────┬────┬───┬────┬─────┐
    │ I │ II │ III │ IV │ V │ VI │ VII │
    ├───┼────┼─────┼────┼───┼────┼─────┤
    │ z │ p  │ p,z │ z  │ p │ p  │ z   │
    └───┴────┴─────┴────┴───┴────┴─────┘

    ┌───┬────┬─────┬─────┬───┬────┬─────┐
    │ I │ II │ III │ IV  │ V │ VI │ VII │
    ├───┼────┼─────┼─────┼───┼────┼─────┤
    │ p │ p  │ z   │ p,z │ p │ z  │ z   │
    └───┴────┴─────┴─────┴───┴────┴─────┘

    ┌───┬────┬─────┬─────┬───┬────┬─────┐
    │ I │ II │ III │ IV  │ V │ VI │ VII │
    ├───┼────┼─────┼─────┼───┼────┼─────┤
    │ z │ z  │ p   │ p,z │ z │ p  │ p   │
    └───┴────┴─────┴─────┴───┴────┴─────┘

    ┌───┬────┬─────┬────┬─────┬────┬─────┐
    │ I │ II │ III │ IV │ V   │ VI │ VII │
    ├───┼────┼─────┼────┼─────┼────┼─────┤
    │ p │ z  │ z   │ p  │ p,z │ z  │ p   │
    └───┴────┴─────┴────┴─────┴────┴─────┘

    ┌───┬────┬─────┬────┬─────┬────┬─────┐
    │ I │ II │ III │ IV │ V   │ VI │ VII │
    ├───┼────┼─────┼────┼─────┼────┼─────┤
    │ z │ p  │ p   │ z  │ p,z │ p  │ z   │
    └───┴────┴─────┴────┴─────┴────┴─────┘

    ┌───┬────┬─────┬────┬───┬─────┬─────┐
    │ I │ II │ III │ IV │ V │ VI  │ VII │
    ├───┼────┼─────┼────┼───┼─────┼─────┤
    │ p │ p  │ z   │ z  │ p │ p,z │ z   │
    └───┴────┴─────┴────┴───┴─────┴─────┘

    ┌───┬────┬─────┬────┬───┬─────┬─────┐
    │ I │ II │ III │ IV │ V │ VI  │ VII │
    ├───┼────┼─────┼────┼───┼─────┼─────┤
    │ z │ z  │ p   │ p  │ z │ p,z │ p   │
    └───┴────┴─────┴────┴───┴─────┴─────┘

    ┌───┬────┬─────┬────┬───┬────┬─────┐
    │ I │ II │ III │ IV │ V │ VI │ VII │
    ├───┼────┼─────┼────┼───┼────┼─────┤
    │ p │ z  │ z   │ p  │ p │ z  │ p,z │
    └───┴────┴─────┴────┴───┴────┴─────┘

    ┌───┬────┬─────┬────┬───┬────┬─────┐
    │ I │ II │ III │ IV │ V │ VI │ VII │
    ├───┼────┼─────┼────┼───┼────┼─────┤
    │ z │ p  │ p   │ z  │ z │ p  │ p,z │
    └───┴────┴─────┴────┴───┴────┴─────┘ |}]
;;
