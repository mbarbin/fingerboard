(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

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
