(**********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*                                                                                *)
(*  This file is part of Fingerboard.                                             *)
(*                                                                                *)
(*  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*  the terms of the GNU Affero General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or any later version.   *)
(*                                                                                *)
(*  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*  for more details.                                                             *)
(*                                                                                *)
(*  You should have received a copy of the GNU Affero General Public License      *)
(*  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(**********************************************************************************)

let make_name constructor_name =
  constructor_name
  |> Stdlib.String.uncapitalize_ascii
  |> String.map ~f:(function
    | '_' -> ' '
    | c -> c)
;;

module Quality = struct
  type t =
    | Doubly_diminished
    | Diminished
    | Minor
    | Perfect
    | Major
    | Augmented
    | Doubly_augmented
  [@@deriving compare, enumerate, equal, hash]

  let constructor_name = function
    | Doubly_diminished -> "Doubly_diminished"
    | Diminished -> "Diminished"
    | Minor -> "Minor"
    | Perfect -> "Perfect"
    | Major -> "Major"
    | Augmented -> "Augmented"
    | Doubly_augmented -> "Doubly_augmented"
  ;;

  let to_dyn t = Dyn.Variant (constructor_name t, [])
  let name t = make_name (constructor_name t)
  let repeat str ~times = String.concat (List.init times ~f:(Fn.const str)) ~sep:""

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
  [@@deriving compare, enumerate, equal, hash]

  let constructor_rank = function
    | Unison -> 0
    | Second -> 1
    | Third -> 2
    | Fourth -> 3
    | Fifth -> 4
    | Sixth -> 5
    | Seventh -> 6
    | Octave -> 7
  ;;

  let constructor_name = function
    | Unison -> "Unison"
    | Second -> "Second"
    | Third -> "Third"
    | Fourth -> "Fourth"
    | Fifth -> "Fifth"
    | Sixth -> "Sixth"
    | Seventh -> "Seventh"
    | Octave -> "Octave"
  ;;

  let to_dyn t = Dyn.Variant (constructor_name t, [])
  let name t = make_name (constructor_name t)
  let to_int t = 1 + constructor_rank t

  let of_int i =
    List.nth all (i - 1)
    |> function
    | Some t -> t
    | None -> Code_error.raise "Index out of bounds." [ "i", i |> Dyn.int ]
  ;;

  let accepts_minor_major_quality = function
    | Unison | Fourth | Fifth | Octave -> false
    | Second | Third | Sixth | Seventh -> true
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
  ;;
end

type t =
  { number : Number.t
  ; quality : Quality.t
  ; additional_octaves : int
  }
[@@deriving compare, equal, hash]

let to_dyn { number; quality; additional_octaves } =
  Dyn.record
    [ "number", number |> Number.to_dyn
    ; "quality", quality |> Quality.to_dyn
    ; "additional_octaves", additional_octaves |> Dyn.int
    ]
;;

let to_string { number; quality; additional_octaves } =
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
   then Printf.sprintf "P8%s" (if skip_unison then "" else " + ")
   else if additional_octaves >= 2
   then Printf.sprintf "%d P8%s" additional_octaves (if skip_unison then "" else " + ")
   else "")
  ^
  if skip_unison
  then ""
  else Quality.prefix_notation quality ^ (Number.to_int number |> Int.to_string)
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
   then Printf.sprintf "octave%s" (if skip_unison then "" else " + ")
   else if additional_octaves >= 2
   then
     Printf.sprintf "%d octaves%s" additional_octaves (if skip_unison then "" else " + ")
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

let compute ~(from : Note.t) ~(to_ : Note.t) () =
  let open Option.Let_syntax in
  let%bind number_of_letter_names, number_of_semitons =
    let rec aux number_of_letter_names number_of_semitons letter_name octave_designation =
      if octave_designation > to_.octave_designation
      then None
      else if
        Note.Letter_name.equal letter_name to_.letter_name
        && octave_designation = to_.octave_designation
      then
        return
          ( number_of_letter_names
          , number_of_semitons
            - Note.Symbol.semitons_shift from.symbol
            + Note.Symbol.semitons_shift to_.symbol )
      else
        aux
          (Int.succ number_of_letter_names)
          (number_of_semitons + Note.Letter_name.semitons_step ~from:letter_name)
          (Note.Letter_name.succ letter_name)
          (Note.Letter_name.succ_octave_designation letter_name ~octave_designation)
    in
    aux 0 0 from.letter_name from.octave_designation
  in
  let number_of_letter_names, number_of_semitons, additional_octaves =
    if number_of_letter_names >= 7 && number_of_semitons >= 12
    then (
      let additional_octaves = number_of_letter_names / 7 in
      ( number_of_letter_names - (7 * additional_octaves)
      , number_of_semitons - (12 * additional_octaves)
      , additional_octaves ))
    else number_of_letter_names, number_of_semitons, 0
  in
  let%bind () =
    if number_of_semitons < 0 || number_of_letter_names > 7 then None else return ()
  in
  let number = Number.of_int (number_of_letter_names + 1) in
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
        aux (Int.pred missing) quality)
      else (
        let%bind quality = Quality.pred quality ~accepts_minor_major_quality in
        aux (Int.succ missing) quality)
    in
    aux (number_of_semitons - basis) basis_quality
  in
  { number; quality; additional_octaves }
;;

let shift_up
      ({ number; quality = _; additional_octaves } as interval)
      ({ Note.letter_name; symbol = _; octave_designation } as from)
  =
  let open Option.Let_syntax in
  let step = Number.to_int number - 1 + (7 * additional_octaves) in
  let target =
    let rec aux step letter_name octave_designation =
      if step = 0
      then { Note.letter_name; symbol = from.symbol; octave_designation }
      else
        aux
          (Int.pred step)
          (Note.Letter_name.succ letter_name)
          (Note.Letter_name.succ_octave_designation letter_name ~octave_designation)
    in
    aux step letter_name octave_designation
  in
  let%bind candidate = compute ~from ~to_:target () in
  let semiton_shift = number_of_semitons interval - number_of_semitons candidate in
  let%map symbol =
    let rec aux shift symbol =
      if shift = 0
      then Some symbol
      else if shift > 0
      then (
        let%bind symbol = Note.Symbol.succ symbol in
        aux (Int.pred shift) symbol)
      else (
        let%bind symbol = Note.Symbol.pred symbol in
        aux (Int.succ shift) symbol)
    in
    aux semiton_shift target.symbol
  in
  { target with symbol }
;;

let shift_down
      ({ number; quality = _; additional_octaves } as interval)
      ({ Note.letter_name; symbol = _; octave_designation } as to_)
  =
  let step = Number.to_int number - 1 + (7 * additional_octaves) in
  let target =
    let rec aux step letter_name octave_designation =
      if step = 0
      then { Note.letter_name; symbol = to_.symbol; octave_designation }
      else
        aux
          (Int.pred step)
          (Note.Letter_name.pred letter_name)
          (Note.Letter_name.pred_octave_designation letter_name ~octave_designation)
    in
    aux step letter_name octave_designation
  in
  let open Option.Let_syntax in
  let%bind candidate = compute ~from:target ~to_ () in
  let semiton_shift = number_of_semitons interval - number_of_semitons candidate in
  let%map symbol =
    let rec aux shift symbol =
      if shift = 0
      then Some symbol
      else if shift > 0
      then (
        let%bind symbol = Note.Symbol.pred symbol in
        aux (Int.pred shift) symbol)
      else (
        let%bind symbol = Note.Symbol.succ symbol in
        aux (Int.succ shift) symbol)
    in
    aux semiton_shift target.symbol
  in
  { target with symbol }
;;

let unison = { number = Unison; quality = Perfect; additional_octaves = 0 }
