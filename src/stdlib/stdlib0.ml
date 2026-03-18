(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

module Array = Array0
module Bool = Bool0
module Code_error = Code_error0
module Dyn = Dyn0
module Float = Float0
module Int = Int0
module List = List0
module Myers = Myers0
module Option = Option0
module Ordering = Ordering0
module String = String0

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)
let phys_equal a b = a == b
