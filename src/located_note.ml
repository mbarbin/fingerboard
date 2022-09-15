open! Core

type t =
  { note : Note.t
  ; fingerboard_location : Fingerboard_location.t
  }
[@@deriving sexp_of]
