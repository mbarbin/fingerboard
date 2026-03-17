(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ArrayLabels
end

val create : len:int -> 'a -> 'a t
val exists : 'a t -> f:('a -> bool) -> bool
val iter : 'a t -> f:('a -> unit) -> unit
val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
val map : 'a t -> f:('a -> 'b) -> 'b t
val rev : 'a t -> 'a t
