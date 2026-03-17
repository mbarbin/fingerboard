(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

type t = float

let compare = Float.compare
let equal = Float.equal
let to_dyn = Dyn.float

let check_exn f =
  match Float.compare f 0. with
  | Eq | Gt -> ()
  | Lt -> Code_error.raise "Out of bounds." [ "f", f |> Dyn.float ]
;;

let of_float_exn f =
  check_exn f;
  f
;;

let to_float f = f
let a4_440 = 440.
