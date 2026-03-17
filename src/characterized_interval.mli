(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

type t = private
  { interval : Interval.t
  ; acoustic_interval : Acoustic_interval.t
  }

val to_dyn : t -> Dyn.t

(** A number representing, in cents, the maximum deviation that is allowed as
    between an acoustic interval and its 12-equal tempered equivalent. Trying
    to create a [t] exceeding this bound will result in [create_exn] to raise. *)
val allowed_deviation_from_equal_tempered_12_in_cents : float

val create_exn : interval:Interval.t -> acoustic_interval:Acoustic_interval.t -> t
