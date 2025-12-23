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
  { interval : Interval.t
  ; acoustic_interval : Acoustic_interval.t
  }

let to_dyn { interval; acoustic_interval } =
  Dyn.record
    [ "interval", interval |> Interval.to_dyn
    ; "acoustic_interval", acoustic_interval |> Acoustic_interval.to_dyn
    ]
;;

let sexp_of_t t = Dyn.to_sexp (to_dyn t)

let allowed_deviation_from_equal_tempered_12_in_cents =
  (* This is just a place holder for now, to be refined as needed. *)
  30.
;;

let check_deviation_exn ~interval ~acoustic_interval =
  let tempered_12_equivalent_in_cents =
    Acoustic_interval.equal_tempered_12 interval |> Acoustic_interval.to_cents
  in
  let in_cents = Acoustic_interval.to_cents acoustic_interval in
  let deviation = Float.abs (tempered_12_equivalent_in_cents -. in_cents) in
  if Float.compare deviation allowed_deviation_from_equal_tempered_12_in_cents > 0
  then
    raise_s
      [%sexp
        "Deviation is out of allowed bounds"
      , [%here]
      , { interval : Interval.t
        ; acoustic_interval : Acoustic_interval.t
        ; tempered_12_equivalent_in_cents : Float.t
        ; in_cents : Float.t
        ; deviation : Float.t
        ; allowed_deviation_from_equal_tempered_12_in_cents : Float.t
        }]
;;

let create_exn ~interval ~acoustic_interval =
  check_deviation_exn ~interval ~acoustic_interval;
  { interval; acoustic_interval }
;;
