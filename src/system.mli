open! Core

type t [@@deriving sexp_of]

(** Instantiate a system with open strings starting from the highest
   vibrating strings, and with open strings following the given
   intervals while going down the instruments. The resulting system is
   set with 1 more string than the length of the array. *)
val create
  :  high_vibrating_string:Note.t
  -> pitch:Frequency.t
  -> intervals_going_down:Characterized_interval.t array
  -> t

(** Reset the pitch from any given string. *)
val reset_pitch : t -> Roman_numeral.t -> pitch:Frequency.t -> unit

val pitch : t -> Fingerboard_location.t -> Frequency.t

(** Returns None if [from] has a higher pitch than [to_]. *)
val acoustic_interval
  :  t
  -> from:Fingerboard_location.t
  -> to_:Fingerboard_location.t
  -> Acoustic_interval.t option

(** Add a new position to the system. A call to this function will
   cause multiple fingerboard locations to be added to the system
   systematically, in particular:

   - the locations that are at the same distance to the open string on
   all other strings;

   - the locations on the same string at higher octaves.

   The name must be unique and not previously known by that system.
   Because positions are systematically added at the octaves of the
   given string portion, the one given as reference for a new position
   is expected to be lower than one octave to the open string. *)
val add_fingerboard_position_exn : t -> Fingerboard_position.t -> unit

(** Returns the list of fingerboard_positions known by this system, retuned in the
   order they are found going up the fingerboard starting from the
   open string. *)
val fingerboard_positions : t -> Fingerboard_position.t list

(** Find the position created with the given name if present or raise. *)
val find_fingerboard_position_exn : t -> name:string -> Fingerboard_position.t
