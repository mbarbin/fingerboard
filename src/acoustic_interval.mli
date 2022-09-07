open! Core

(** *)

type t = private
  | Zero
  | Equal_division_of_the_octave of
      { divisor : int
      ; number_of_divisions : int
      }
  | Natural_ratio of Natural_ratio.t
  | Reduced_natural_ratio of Natural_ratio.Reduced.t
  | Cents of float
[@@deriving sexp_of]

val add : t -> t -> t
val to_cents : t -> float

module Symbolic : sig
  type t =
    | Equal_tempered_12 of Interval.t
    | Pythagorean of Interval.t
    | Natural_ratio of Natural_ratio.t
    | Reduced_natural_ratio of Natural_ratio.Reduced.t
    | Equal_division_of_the_octave of
        { divisor : int
        ; number_of_divisions : int
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
val unison : t
val octave : t
val of_cents : float -> t
val shift_up : t -> Frequency.t -> Frequency.t
val shift_down : t -> Frequency.t -> Frequency.t
