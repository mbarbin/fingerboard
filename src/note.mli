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
end

module Symbol : sig
  type t =
    | Natural
    | Flat
    | Sharp
    | Double_sharp
    | Double_flat
    | Triple_sharp
    | Triple_flat
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  val to_string : t -> string
  val prefix_notation : t -> string
end

type t =
  { letter_name : Letter_name.t
  ; symbol : Symbol.t
  }
[@@deriving compare, enumerate, equal, hash, sexp_of]

val to_string : t -> string
