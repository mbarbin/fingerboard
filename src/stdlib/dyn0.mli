(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

include module type of struct
  include Dyn
end

val inline_record : string -> (string * Dyn.t) list -> Dyn.t
