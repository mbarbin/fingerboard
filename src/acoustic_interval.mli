open! Core

(** *)

type t

val add : t -> t -> t
val cents : t -> float

module Symbolic : sig
  type t =
    | Equal_tempered_12 of Interval.t
    | Pythagorean of Interval.t
    | Natural_ratio of
        { numerator : int
        ; denominator : int
        }
    | Equal_division_of_the_octave of
        { divisor : int
        ; parts : int
        }
    | Just_diatonic_semiton
    | Just_minor_ton
    | Just_major_ton
    | Just_minor_third
    | Just_major_third
    | Just_minor_sixth
    | Just_major_sixth
    | Compound of t list
end

val of_symbolic : Symbolic.t -> t
val of_cents : float -> t
