(*_********************************************************************************)
(*_  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*_  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*_********************************************************************************)

(** A note played at a given location within a system. *)

type t =
  { note : Note.t
  ; fingerboard_location : Fingerboard_location.t
  }

val to_dyn : t -> Dyn.t

module Abbrev : sig
  (** An abbreviated representation for printing in expect tests. *)
  type t

  val to_dyn : t -> Dyn.t
end

val to_abbrev : t -> Abbrev.t

module Scale_abbrev : sig
  type t

  val to_dyn : t -> Dyn.t
end

val to_scale_abbrev : t list -> Scale_abbrev.t
