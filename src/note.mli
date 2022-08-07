open! Core

(** *)

module Letter_name : sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  val to_string : t -> string
  val next : t -> t

  (** How many semitons are there from [t] to [next t]. *)
  val semitons_step : from:t -> int
end

module Symbol : sig
  type t =
    | Triple_flat
    | Double_flat
    | Flat
    | Natural
    | Sharp
    | Double_sharp
    | Triple_sharp
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  val to_string : t -> string
  val prefix_notation : t -> string
  val semitons_shift : t -> int
end

type t =
  { letter_name : Letter_name.t
  ; symbol : Symbol.t
  }
[@@deriving compare, enumerate, equal, hash, sexp_of]

val to_string : t -> string
