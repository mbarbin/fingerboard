(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

(** C2, G2, D3, A3 being the standard strings on the cello, and A4 the standard
    440 Hz pitch. *)

type t = int

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
