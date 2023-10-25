open! Base

(** The pitch in Hz of a note. *)
type t [@@deriving compare, equal, hash, sexp_of]

(** Raises if the float is negative. *)
val of_float_exn : float -> t

val to_float : t -> float
val a4_440 : t
