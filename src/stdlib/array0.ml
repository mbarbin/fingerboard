(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

include Stdlib.ArrayLabels

let create ~len a = make len a
let exists t ~f = exists ~f t
let iter t ~f = iter ~f t
let fold t ~init ~f = fold_left t ~init ~f
let map t ~f = map ~f t

let rev t =
  let len = Array.length t in
  let pred_len = pred len in
  init len ~f:(fun i -> t.(pred_len - i))
;;
