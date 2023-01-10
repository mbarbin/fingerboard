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
      | `M6z_e53
      | `M6p_e53
      | `m7p_e53
      | `m7z_e53
      | `M7z_e53
      | `M7p_e53
      | `P8z_e53
      ]
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
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
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
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
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
  end

  type t =
    [ `open_string
    | Edo12.t
    | Edo53.t
    | Pythagorean.t
    | Just.t
    ]
  [@@deriving compare, equal, enumerate, hash, sexp_of]

  val to_string : t -> string
  val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
end

(** Create a system with 4 strings tuned in fifth and initialise the
   open_string position. Default tuning the cello to pythagorean
   fifths, but this may be overrided. *)
val fifth_system : ?acoustic_interval:Acoustic_interval.t -> unit -> System.t

val fingerboard_position : Fingerboard_position_name.t -> Fingerboard_position.t

val add_fingerboard_position_exn
  :  ?on_n_octaves:int
  -> System.t
  -> Fingerboard_position_name.t
  -> unit

(** The selection of this particular note is somewhat arbitrary,
   anything around that note is realistic here. This is used to limit
   the generation of scales and positions. *)
val fingerboard_highest_note : Note.t

val find_fingerboard_position_exn
  :  System.t
  -> Fingerboard_position_name.t
  -> Fingerboard_position.t
