open! Core

module Quality = struct
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

module Number = struct
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

  let to_int t =
    match List.find_mapi all ~f:(fun i t' -> Option.some_if (equal t t') i) with
    | Some i -> i
    | None -> raise_s [%sexp "Index not found", (t : t), [%here]]
  ;;
end
