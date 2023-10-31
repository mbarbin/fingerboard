(** *)

module Quality : sig
  type t =
    | Doubly_diminished
    | Diminished
    | Minor
    | Perfect
    | Major
    | Augmented
    | Doubly_augmented
  [@@deriving compare, equal, hash, sexp_of]

  val prefix_notation : t -> string
end

module Number : sig
  type t =
    | Unison
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Octave
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  val to_int : t -> int
  val accepts_minor_major_quality : t -> bool
end

type t =
  { number : Number.t
  ; quality : Quality.t
  ; additional_octaves : int
  }
[@@deriving compare, equal, hash, sexp_of]

val name : t -> string
val to_string : t -> string
val number_of_semitons : t -> int
val compute : from:Note.t -> to_:Note.t -> unit -> t option
val shift_up : t -> Note.t -> Note.t option
val shift_down : t -> Note.t -> Note.t option
