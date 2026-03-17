(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

include module type of struct
  include Stdlib.ListLabels
end

val filter : 'a t -> f:('a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val group : 'a t -> break:('a -> 'a -> bool) -> 'a t t
val groupi : 'a t -> break:(int -> 'a -> 'a -> bool) -> 'a t t
val init : int -> f:(int -> 'a) -> 'a t
val iter : 'a t -> f:('a -> unit) -> unit
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val partition : 'a t -> f:('a -> bool) -> 'a t * 'a t
val range : int -> int -> int list
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a option
val sort : 'a t -> compare:('a -> 'a -> Ordering.t) -> 'a t
val sort_then_dedup : 'a t -> compare:('a -> 'a -> Ordering.t) -> 'a t
