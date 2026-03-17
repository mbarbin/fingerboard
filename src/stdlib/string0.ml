(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

include Stdlib.StringLabels

let chop_prefix t ~prefix =
  if String.starts_with t ~prefix
  then (
    let prefix_len = String.length prefix in
    Some (StringLabels.sub t ~pos:prefix_len ~len:(String.length t - prefix_len)))
  else None
;;

let compare a b = String.compare a b |> Ordering.of_int
