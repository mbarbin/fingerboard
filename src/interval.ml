open! Core

module Quality = struct
  type t =
    | Doubly_diminished
    | Diminished
    | Minor
    | Perfect
    | Major
    | Augmented
    | Doubly_augmented
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  let to_name t = Sexp_to_string.atom_to_name t [%sexp_of: t]
  let repeat str ~times = String.concat (List.init times ~f:(const str)) ~sep:""

  let rec prefix_notation = function
    | Doubly_diminished -> repeat (prefix_notation Diminished) ~times:2
    | Diminished -> "d"
    | Minor -> "m"
    | Perfect -> "P"
    | Major -> "M"
    | Augmented -> "A"
    | Doubly_augmented -> repeat (prefix_notation Augmented) ~times:2
  ;;
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

  let to_name t = Sexp_to_string.atom_to_name t [%sexp_of: t]

  let to_int t =
    match List.find_mapi all ~f:(fun i t' -> Option.some_if (equal t t') i) with
    | Some i -> i + 1
    | None -> raise_s [%sexp "Index not found", (t : t), [%here]]
  ;;

  let accepts_minor_major_quality = function
    | Unison | Fourth | Fifth | Octave | Eleventh | Twelfth | Double_octave -> false
    | Second | Third | Sixth | Seventh | Ninth | Tenth | Thirteenth | Fourteeth -> true
  ;;

  let basis_for_number_of_semitons = function
    | Unison -> 0
    | Second -> 2
    | Third -> 4
    | Fourth -> 5
    | Fifth -> 7
    | Sixth -> 9
    | Seventh -> 11
    | Octave -> 12
    | Ninth -> 14
    | Tenth -> 16
    | Eleventh -> 17
    | Twelfth -> 19
    | Thirteenth -> 21
    | Fourteeth -> 23
    | Double_octave -> 24
  ;;
end

type t =
  { number : Number.t
  ; quality : Quality.t
  }
[@@deriving compare, equal, hash, sexp_of]

let to_string { number; quality } =
  Quality.prefix_notation quality ^ (Number.to_int number |> Int.to_string)
;;

let to_name { number; quality } = Quality.to_name quality ^ " " ^ Number.to_name number

let number_of_semitons t =
  let accepts_minor_major_quality = Number.accepts_minor_major_quality t.number in
  let basis = Number.basis_for_number_of_semitons t.number in
  let shift =
    match t.quality with
    | Perfect -> 0
    | Major -> 0
    | Minor -> -1
    | Augmented -> 1
    | Diminished -> if accepts_minor_major_quality then -2 else -1
    | Doubly_augmented -> 2
    | Doubly_diminished -> if accepts_minor_major_quality then -3 else -2
  in
  basis + shift
;;
