open! Core

type t = private
  { interval : Interval.t
  ; acoustic_interval : Acoustic_interval.t
  }
[@@deriving sexp_of]

(** A number representing, in cents, the maximum deviation that is allowed as
    between an acoustic interval and its 12-equal tempered equivalent. Trying
    to create a [t] exceeding this bound will result in [create_exn] to raise.
*)
val allowed_deviation_from_equal_tempered_12_in_cents : float

val create_exn : interval:Interval.t -> acoustic_interval:Acoustic_interval.t -> t
