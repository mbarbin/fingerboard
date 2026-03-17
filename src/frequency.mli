(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

(** The pitch in Hz of a note. *)
type t

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

(** Raises if the float is negative. *)
val of_float_exn : float -> t

val to_float : t -> float
val a4_440 : t
