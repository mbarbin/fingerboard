(** *)

type t = private
  | Zero
  | Equal_division_of_the_octave of
      { divisor : int
      ; number_of_divisions : int
      }
  | Reduced_natural_ratio of Natural_ratio.Reduced.t
  | Octaves of { number_of_octaves : int }
  | Cents of float
[@@deriving hash, sexp_of]

val to_string : t -> string
val compound : t list -> t
val add : t -> t -> t

(** [remove t1 t2] Returns None if t2 is larger than t1. *)
val remove : t -> t -> t option

val to_cents : t -> float

(** Convert to cents if the two are not of the same shape. *)
val equal : t -> t -> bool

(** Convert to cents, and compare the two. *)
val compare : t -> t -> int

val unison : t
val octave : t
val equal_tempered_12 : Interval.t -> t
val pythagorean_diatonic_semiton : t
val pythagorean_chromatic_semiton : t
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
