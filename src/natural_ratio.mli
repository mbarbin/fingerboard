(** A strictly positive ratio of two natural numbers. *)

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
  type natural_ratio := t

  (** A [natural_ratio] kept in its primed decomposition form. *)
  type t [@@deriving sexp_of]

  val to_string : t -> string
  val equal : t -> t -> bool
  val one : t

  (** [prime] is assumed to be a prime number. The resulting [t] will be invalid
      and the behavior of the rest of the module unspecified if it isn't. The
      exponent is expected to be non null. Checking that [prime] is indeed
      prime is only done if its value is small enough. This should suffice in
      practice, since creating such values is done with small ps. *)
  val create_exn : prime:int -> exponent:int -> t

  val compound : t list -> t
  val inverse : t -> t
  val multiply : t -> t -> t
  val divide : t -> t -> t
  val to_natural_ratio : t -> natural_ratio

  (** When both the numerator and denominator and below a small bound, this
      function can be used to instantiate a [t]. Raises if out of supported
      bounds. *)
  val of_small_natural_ratio_exn : numerator:int -> denominator:int -> t
end
