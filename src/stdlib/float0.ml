(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

include Stdlib.Float

let compare a b = Ordering.of_int (Float.compare a b)

let iround_nearest_exn f =
  let i = Int.of_float f in
  if i >= 0
  then (
    match compare (f -. Int.to_float i) 0.5 with
    | Gt -> i + 1
    | Eq | Lt -> i)
  else (
    match compare (Int.to_float i -. f) 0.5 with
    | Gt -> i - 1
    | Eq | Lt -> i)
;;
