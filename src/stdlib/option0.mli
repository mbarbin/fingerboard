(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.Option
end

val bind : 'a t -> f:('a -> 'b t) -> 'b t
val map : 'a t -> f:('a -> 'b) -> 'b t
