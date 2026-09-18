(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

type t = float

let iround_exn t =
  match Float.classify_float t with
  | FP_zero | FP_subnormal -> 0
  | FP_normal -> t |> Float.round |> Float.to_int
  | FP_infinite | FP_nan ->
    Code_error.raise "Cents.iround_exn: unexpected float value" [ "t", Dyn.float t ]
;;
