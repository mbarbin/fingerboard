(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

type t =
  | I
  | II
  | III
  | IV
  | V
  | VI
  | VII

let constructor_name = function
  | I -> "I"
  | II -> "II"
  | III -> "III"
  | IV -> "IV"
  | V -> "V"
  | VI -> "VI"
  | VII -> "VII"
;;

let to_string = constructor_name
let to_dyn t = Dyn.variant (constructor_name t) []
let all = [ I; II; III; IV; V; VI; VII ]

let to_int = function
  | I -> 1
  | II -> 2
  | III -> 3
  | IV -> 4
  | V -> 5
  | VI -> 6
  | VII -> 7
;;

let compare t1 t2 = Int.compare (to_int t1) (to_int t2)

let equal t1 t2 =
  match compare t1 t2 with
  | Eq -> true
  | Lt | Gt -> false
;;

let of_int_exn = function
  | 1 -> I
  | 2 -> II
  | 3 -> III
  | 4 -> IV
  | 5 -> V
  | 6 -> VI
  | 7 -> VII
  | i ->
    Code_error.raise
      "Roman_numeral.of_int_exn: Input is out of bounds."
      [ "input", i |> Dyn.int ]
;;

let one = I
let succ_exn t = of_int_exn (Int.succ (to_int t))
