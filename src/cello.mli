open! Base

(** A number of utils applicable to the applications of the library to the cello. *)

module Fingerboard_position_name : sig
  module type S = sig
    type t [@@deriving compare, equal, enumerate, hash, sexp_of]

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
