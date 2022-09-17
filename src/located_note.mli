open! Core

(** A note played at a given location within a system. *)

type t =
  { note : Note.t
  ; fingerboard_location : Fingerboard_location.t
  }
[@@deriving sexp_of]

module Abbrev : sig
  (** An abbreviated representation for printing in expect tests. *)
  type t [@@deriving sexp_of]
end

val to_abbrev : t -> Abbrev.t

module Scales_abbrev : sig
  type t [@@deriving sexp_of]
end

val to_scales_abbrev : t list -> Scales_abbrev.t
