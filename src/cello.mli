open! Core

(** A number of utils applicable to the applications of the library to the cello. *)

module Fingerboard_position_name : sig
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
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
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
      | `m7p_e53
      | `m7z_e53
      | `M7z_e53
      | `M7p_e53
      | `P8z_e53
      ]
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
  end

  type t =
    [ `open_string
    | Edo12.t
    | Edo53.t
    ]
  [@@deriving compare, equal, hash, sexp_of]

  val to_string : t -> string
  val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
end

val fingerboard_position : Fingerboard_position_name.t -> Fingerboard_position.t
