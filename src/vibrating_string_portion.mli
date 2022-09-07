open! Core

(** Determines the location of a finger in a vibrating string, or
   rather a characterization of the relative pitch of the remaining
   portion of the string that vibrates. *)
type t [@@deriving compare, equal, hash, sexp_of]

(** Letting the entire string vibrate. *)
val open_string : t

(** The acoustic_interval relative to the open string. *)
val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t

(** The portion defined from the acoustic interval relative to the
   open string. *)
val of_acoustic_interval_to_the_open_string : Acoustic_interval.t -> t
