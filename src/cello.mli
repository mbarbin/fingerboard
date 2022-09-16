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
    [@@deriving compare, equal, hash, sexp_of]

    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
  end

  type t =
    [ `open_string
    | Edo12.t
    ]
  [@@deriving compare, equal, hash, sexp_of]

  val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
end

val fingerboard_position : Fingerboard_position_name.t -> Fingerboard_position.t
