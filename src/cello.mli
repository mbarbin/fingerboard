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

(** A number of utils applicable to the applications of the library to the cello. *)

module Fingerboard_position_name : sig
  module type S = sig
    type t [@@deriving compare, equal, enumerate]

    val to_dyn : t -> Dyn.t
    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
  end

  module Edo12 : sig
    type t =
      [ `m2e
      | `M2e
      | `m3e
      | `M3e
      | `P4e
      | `A4e
      | `P5e
      | `m6e
      | `M6e
      | `m7e
      | `M7e
      ]

    include S with type t := t
  end

  module Edo19 : sig
    type t =
      [ `A1_e19
      | `m2_e19
      | `M2_e19
      | `A2_e19
      | `m3_e19
      | `M3_e19
      | `A3_e19
      | `P4_e19
      | `A4_e19
      | `d5_e19
      | `P5_e19
      | `A5_e19
      | `m6_e19
      | `M6_e19
      | `d7_e19
      | `m7_e19
      | `M7_e19
      | `d8_e19
      ]

    include S with type t := t
  end

  module Edo31 : sig
    type t =
      [ `A1_e31
      | `m2_e31
      | `M2_e31
      | `d3_e31
      | `A2_e31
      | `m3_e31
      | `M3_e31
      | `d4_e31
      | `A3_e31
      | `P4_e31
      | `A4_e31
      | `d5_e31
      | `P5_e31
      | `A5_e31
      | `m6_e31
      | `M6_e31
      | `d7_e31
      | `m7_e31
      | `M7_e31
      | `d8_e31
      ]

    include S with type t := t
  end

  module Edo53 : sig
    type t =
      [ `A1z_e53
      | `m2z_e53
      | `M2z_e53
      | `M2p_e53
      | `m3p_e53
      | `m3z_e53
      | `M3z_e53
      | `M3p_e53
      | `P4p_e53
      | `P4z_e53
      | `A4z_e53
      | `d5z_e53
      | `P5z_e53
      | `P5p_e53
      | `m6p_e53
      | `m6z_e53
      | `M6z_e53
      | `M6p_e53
      | `m7p_e53
      | `m7z_e53
      | `M7z_e53
      | `M7p_e53
      | `P8z_e53
      ]

    include S with type t := t
  end

  module Edo55 : sig
    type t =
      [ `A1_e55
      | `m2_e55
      | `M2_e55
      | `d3_e55
      | `A2_e55
      | `m3_e55
      | `M3_e55
      | `d4_e55
      | `A3_e55
      | `P4_e55
      | `A4_e55
      | `d5_e55
      | `P5_e55
      | `A5_e55
      | `m6_e55
      | `M6_e55
      | `d7_e55
      | `m7_e55
      | `M7_e55
      | `d8_e55
      ]

    include S with type t := t
  end

  module Pythagorean : sig
    type t =
      [ `m2p
      | `A1p
      | `d3p
      | `M2p
      | `m3p
      | `A2p
      | `d4p
      | `M3p
      | `P4p
      | `A3p
      | `d5p
      | `A4p
      | `d6p
      | `P5p
      | `m6p
      | `A5p
      | `d7p
      | `M6p
      | `m7p
      | `A6p
      | `d8p
      | `M7p
      ]

    include S with type t := t
  end

  module Just : sig
    type t =
      [ `A1z
      | `m2z
      | `M2z
      | `d3z
      | `m3z
      | `M3z
      | `d4z
      | `P4z
      | `A4z
      | `d5z
      | `P5z
      | `A5z
      | `m6z
      | `M6z
      | `d7z
      | `M7z
      | `d8z
      | `P8z
      ]

    include S with type t := t
  end

  type t =
    [ `open_string
    | Edo12.t
    | Edo19.t
    | Edo31.t
    | Edo53.t
    | Edo55.t
    | Pythagorean.t
    | Just.t
    ]

  include S with type t := t

  val to_string : t -> string
end

(** Create a system with 4 strings tuned in fifth and initialize the open_string
    position. Default tuning the cello to pythagorean fifths, but this may be
    overridden. *)
val fifth_system : ?acoustic_interval:Acoustic_interval.t -> unit -> System.t

val fingerboard_position : Fingerboard_position_name.t -> Fingerboard_position.t

val add_fingerboard_position_exn
  :  ?on_n_octaves:int
  -> System.t
  -> Fingerboard_position_name.t
  -> unit

(** The selection of this particular note is somewhat arbitrary, anything around
    that note is realistic here. This is used to limit the generation of scales
    and positions. *)
val fingerboard_highest_note : Note.t

val find_fingerboard_position_exn
  :  System.t
  -> Fingerboard_position_name.t
  -> Fingerboard_position.t
