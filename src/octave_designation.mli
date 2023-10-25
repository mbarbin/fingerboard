open! Base

(** C2, G2, D3, A3 being the standard strings on the cello, and A4 the standard
    440 Hz pitch. *)

type t = int [@@deriving compare, equal, hash, sexp_of]
