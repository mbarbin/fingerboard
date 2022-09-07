open! Core

(** *)

type t = private
  | Zero
  | Equal_division_of_the_octave of
      { divisor : int
      ; number_of_divisions : int
      }
  | Reduced_natural_ratio of Natural_ratio.Reduced.t
  | Cents of float
[@@deriving sexp_of]

val compound : t list -> t
val add : t -> t -> t
val to_cents : t -> float
val unison : t
val octave : t
val equal_tempered_12 : Interval.t -> t
val pythagorean : Interval.t -> t
val reduced_natural_ratio : Natural_ratio.Reduced.t -> t
val small_natural_ratio_exn : numerator:int -> denominator:int -> t
val equal_division_of_the_octave : divisor:int -> number_of_divisions:int -> t
val just_diatonic_semiton : t
val just_minor_ton : t
val just_major_ton : t
val just_minor_third : t
val just_major_third : t
val just_minor_sixth : t
val just_major_sixth : t
val of_cents : float -> t

(** Operating on frequencies. *)

val shift_up : t -> Frequency.t -> Frequency.t
val shift_down : t -> Frequency.t -> Frequency.t
