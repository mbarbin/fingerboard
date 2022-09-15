open! Core

(** A note played at a given location within a system. *)

type t =
  { note : Note.t
  ; fingerboard_location : Fingerboard_location.t
  }
[@@deriving sexp_of]
