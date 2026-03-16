(*_*********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*_  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*_                                                                                *)
(*_  This file is part of Fingerboard.                                             *)
(*_                                                                                *)
(*_  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*_  the terms of the GNU Affero General Public License as published by the Free   *)
(*_  Software Foundation, either version 3 of the License, or any later version.   *)
(*_                                                                                *)
(*_  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*_  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*_  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*_  for more details.                                                             *)
(*_                                                                                *)
(*_  You should have received a copy of the GNU Affero General Public License      *)
(*_  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(*_*********************************************************************************)

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
