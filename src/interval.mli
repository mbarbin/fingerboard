open! Core

(** *)

module Quality : sig
  type t =
    | Perfect
    | Major
    | Minor
    | Augmented
    | Diminished
    | Augmented' of { multiple : int }
    | Diminished' of { multiple : int }
  [@@deriving compare, equal, hash, sexp_of]
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
    | Ninth
    | Tenth
    | Eleventh
    | Twelfth
    | Thirteenth
    | Fourteeth
    | Double_octave
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  val to_int : t -> int
end
