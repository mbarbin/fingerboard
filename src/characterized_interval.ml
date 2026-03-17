(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

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
  match Float.compare deviation allowed_deviation_from_equal_tempered_12_in_cents with
  | Eq | Lt -> ()
  | Gt ->
    Code_error.raise
      "Deviation is out of allowed bounds."
      [ "interval", interval |> Interval.to_dyn
      ; "acoustic_interval", acoustic_interval |> Acoustic_interval.to_dyn
      ; "tempered_12_equivalent_in_cents", tempered_12_equivalent_in_cents |> Dyn.float
      ; "in_cents", in_cents |> Dyn.float
      ; "deviation", deviation |> Dyn.float
      ; ( "allowed_deviation_from_equal_tempered_12_in_cents"
        , allowed_deviation_from_equal_tempered_12_in_cents |> Dyn.float )
      ]
;;

let create_exn ~interval ~acoustic_interval =
  check_deviation_exn ~interval ~acoustic_interval;
  { interval; acoustic_interval }
;;
