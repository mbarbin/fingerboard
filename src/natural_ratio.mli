open! Core

(** A representant of strictly positive ratio of two natural numbers. *)

type t = private
  { numerator : int
  ; denominator : int
  }
[@@deriving sexp_of]

val equal : t -> t -> bool
val to_string : t -> string
val create_exn : numerator:int -> denominator:int -> t
val inverse : t -> t
val multiply : t -> t -> t
val divide : t -> t -> t

module Reduced : sig
  type natural_ratio

  (** A [natural_ratio] kept in its primed decomposition form. *)
  type t [@@deriving sexp_of]

  val to_string : t -> string
  val equal : t -> t -> bool
  val one : t

  (** [prime] is assumed to be a prime number. The resulting [t] will
      be invalid and the behavior of the rest of the module unspecified
      if it isn't. The exponent is expected to be non null. *)
  val create_exn : prime:int -> exponent:int -> t

  val compound : t list -> t
  val inverse : t -> t
  val multiply : t -> t -> t
  val divide : t -> t -> t
  val to_natural_ratio : t -> natural_ratio
end
with type natural_ratio := t
