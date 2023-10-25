open! Base

type t =
  { fingerboard_position : Fingerboard_position.t
  ; string_number : Roman_numeral.t
  }
[@@deriving compare, equal, hash, sexp_of]
