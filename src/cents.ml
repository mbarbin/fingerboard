(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

type t = float

let to_string_nearest t =
  let nearest = t |> Float.iround_nearest_exn in
  Int.to_string nearest
;;
