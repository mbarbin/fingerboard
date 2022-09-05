open! Core

(** C2 being the lower C string on the cello, and A4 the standard 440 Hz pitch. *)

type t = int [@@deriving compare, equal, hash, sexp_of]
