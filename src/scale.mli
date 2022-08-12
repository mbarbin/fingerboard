open! Core

(** *)

type t

val ascending : t -> from:Note.t -> Note.t array
val descending : t -> from:Note.t -> Note.t array

(** Create a scale from a sequence of consecutive intervals. *)
val create : Interval.t array -> t

val major : t Lazy.t
val minor : t Lazy.t
