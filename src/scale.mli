(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

(** *)

type t

val ascending : t -> from:Note.t -> Note.t array
val descending : t -> from:Note.t -> Note.t array

(** Create a scale from a sequence of consecutive intervals. *)
val create : Interval.t array -> t

val major : t Lazy.t
val minor : t Lazy.t
