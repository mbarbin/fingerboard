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

  let name t = Sexp_to_string.atom_to_name t [%sexp_of: t]
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

  let succ t ~accepts_minor_major_quality =
    match t with
    | Doubly_diminished -> Some Diminished
    | Diminished -> Some (if accepts_minor_major_quality then Minor else Perfect)
    | Minor -> Some Major
    | Perfect -> Some Augmented
    | Major -> Some Augmented
    | Augmented -> Some Doubly_augmented
    | Doubly_augmented -> None
  ;;

  let pred t ~accepts_minor_major_quality =
    match t with
    | Doubly_diminished -> None
    | Diminished -> Some Doubly_diminished
    | Minor -> Some Diminished
    | Perfect -> Some Diminished
    | Major -> Some Minor
    | Augmented -> Some (if accepts_minor_major_quality then Major else Perfect)
    | Doubly_augmented -> Some Augmented
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

  let name t = Sexp_to_string.atom_to_name t [%sexp_of: t]

  let to_int t =
    match List.find_mapi all ~f:(fun i t' -> Option.some_if (equal t t') i) with
    | Some i -> i + 1
    | None -> raise_s [%sexp "Index not found", (t : t), [%here]]
  ;;

  let of_int i =
    List.nth all (i - 1)
    |> function
    | Some t -> t
    | None -> raise_s [%sexp "Index out of bounds", (i : int), [%here]]
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
  ; additional_octaves : int
  }
[@@deriving compare, equal, hash, sexp_of]

let to_string { number; quality; additional_octaves } =
  (if additional_octaves = 1
  then "P8 + "
  else if additional_octaves >= 2
  then sprintf "%d P8 + " additional_octaves
  else "")
  ^ Quality.prefix_notation quality
  ^ (Number.to_int number |> Int.to_string)
;;

let name { number; quality; additional_octaves } =
  let skip_quality =
    match quality with
    | Perfect -> true
    | Doubly_diminished | Diminished | Minor | Major | Augmented | Doubly_augmented ->
      false
  in
  let skip_unison =
    Number.equal number Unison && skip_quality && additional_octaves >= 1
  in
  (if additional_octaves = 1
  then sprintf "octave%s" (if skip_unison then "" else " + ")
  else if additional_octaves >= 2
  then sprintf "%d octaves%s" additional_octaves (if skip_unison then "" else " + ")
  else "")
  ^ (if skip_quality then "" else Quality.name quality ^ " ")
  ^ if skip_unison then "" else Number.name number
;;

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
  (t.additional_octaves * 12) + basis + shift
;;

let compute ?(plus_one_octave = false) ~(from : Note.t) ~(to_ : Note.t) () =
  let open Option.Let_syntax in
  let cross =
    Note.Letter_name.equal from.letter_name to_.letter_name
    && Note.Symbol.compare from.symbol to_.symbol > 0
  in
  let number_of_letter_names =
    let rec aux acc note =
      if Note.Letter_name.equal note to_.letter_name
      then acc + if plus_one_octave then 7 else 0
      else aux (succ acc) (Note.Letter_name.next note)
    in
    aux (if cross then 8 else 1) from.letter_name
  in
  let number_of_semitons =
    let rec aux acc note =
      if Note.Letter_name.equal note to_.letter_name
      then acc + if plus_one_octave then 12 else 0
      else
        aux (acc + Note.Letter_name.semitons_step ~from:note) (Note.Letter_name.next note)
    in
    aux (if cross then 12 else 0) from.letter_name
    - Note.Symbol.semitons_shift from.symbol
    + Note.Symbol.semitons_shift to_.symbol
  in
  let%bind () = if number_of_semitons < 0 then None else return () in
  let number = Number.of_int number_of_letter_names in
  let basis = Number.basis_for_number_of_semitons number in
  let accepts_minor_major_quality = Number.accepts_minor_major_quality number in
  let basis_quality =
    if accepts_minor_major_quality then Quality.Major else Quality.Perfect
  in
  let%map quality =
    let rec aux missing quality =
      if Int.equal missing 0
      then return quality
      else if missing > 0
      then (
        let%bind quality = Quality.succ quality ~accepts_minor_major_quality in
        aux (pred missing) quality)
      else (
        let%bind quality = Quality.pred quality ~accepts_minor_major_quality in
        aux (succ missing) quality)
    in
    aux (number_of_semitons - basis) basis_quality
  in
  { number; quality; additional_octaves = 0 }
;;
