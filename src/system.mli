open! Core

type t [@@deriving sexp_of]

(** Instantiate a system with open strings starting from the highest
   vibrating strings, and with open strings following the given
   intervals while going down the instruments. The resulting system is
   set with 1 more string than the length of the array. *)
val create
  :  high_vibrating_string:Note.t
  -> pitch:Frequency.t
  -> intervals_going_down:(Interval.t * Acoustic_interval.t) array
  -> t

(** Reset the pitch from any given string. *)
val reset_pitch : t -> Roman_numeral.t -> pitch:Frequency.t -> unit
