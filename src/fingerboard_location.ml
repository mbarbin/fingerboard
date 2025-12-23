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

type t =
  { fingerboard_position : Fingerboard_position.t
  ; string_number : Roman_numeral.t
  }

let compare t ({ fingerboard_position; string_number } as t2) : Ordering.t =
  if phys_equal t t2
  then Eq
  else (
    match Fingerboard_position.compare t.fingerboard_position fingerboard_position with
    | (Lt | Gt) as r -> r
    | Eq -> Roman_numeral.compare t.string_number string_number)
;;

let equal t1 t2 = Ordering.is_eq (compare t1 t2)

let to_dyn { fingerboard_position; string_number } =
  Dyn.record
    [ "fingerboard_position", fingerboard_position |> Fingerboard_position.to_dyn
    ; "string_number", string_number |> Roman_numeral.to_dyn
    ]
;;
