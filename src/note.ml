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

module Letter_name = struct
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G

  let constructor_name = function
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | E -> "E"
    | F -> "F"
    | G -> "G"
  ;;

  let constructor_rank = function
    | A -> 0
    | B -> 1
    | C -> 2
    | D -> 3
    | E -> 4
    | F -> 5
    | G -> 6
  ;;

  let all = [ A; B; C; D; E; F; G ]
  let compare t1 t2 = Int.compare (constructor_rank t1) (constructor_rank t2)
  let equal t1 t2 = Int.equal (constructor_rank t1) (constructor_rank t2)
  let to_dyn t = Dyn.variant (constructor_name t) []
  let to_string = constructor_name

  let succ = function
    | A -> B
    | B -> C
    | C -> D
    | D -> E
    | E -> F
    | F -> G
    | G -> A
  ;;

  let pred = function
    | A -> G
    | B -> A
    | C -> B
    | D -> C
    | E -> D
    | F -> E
    | G -> F
  ;;

  let semitons_step ~from =
    match from with
    | A -> 2
    | B -> 1
    | C -> 2
    | D -> 2
    | E -> 1
    | F -> 2
    | G -> 2
  ;;

  let succ_octave_designation t ~octave_designation =
    octave_designation + if equal t B then 1 else 0
  ;;

  let pred_octave_designation t ~octave_designation =
    octave_designation - if equal t C then 1 else 0
  ;;
end

module Symbol = struct
  type t =
    | Triple_flat
    | Double_flat
    | Flat
    | Natural
    | Sharp
    | Double_sharp
    | Triple_sharp

  let constructor_rank = function
    | Triple_flat -> 0
    | Double_flat -> 1
    | Flat -> 2
    | Natural -> 3
    | Sharp -> 4
    | Double_sharp -> 5
    | Triple_sharp -> 6
  ;;

  let constructor_name = function
    | Triple_flat -> "Triple_flat"
    | Double_flat -> "Double_flat"
    | Flat -> "Flat"
    | Natural -> "Natural"
    | Sharp -> "Sharp"
    | Double_sharp -> "Double_sharp"
    | Triple_sharp -> "Triple_sharp"
  ;;

  let all = [ Triple_flat; Double_flat; Flat; Natural; Sharp; Double_sharp; Triple_sharp ]
  let compare t1 t2 = Int.compare (constructor_rank t1) (constructor_rank t2)
  let equal t1 t2 = Int.equal (constructor_rank t1) (constructor_rank t2)
  let to_dyn t = Dyn.variant (constructor_name t) []

  let to_string = function
    | Triple_flat -> "bbb"
    | Double_flat -> "bb"
    | Flat -> "b"
    | Natural -> ""
    | Sharp -> "#"
    | Double_sharp -> "##"
    | Triple_sharp -> "###"
  ;;

  let prefix_notation = function
    | Triple_flat -> "bbb"
    | Double_flat -> "bb"
    | Flat -> "b"
    | Natural -> ""
    | Sharp -> "#"
    | Double_sharp -> "x"
    | Triple_sharp -> "#x"
  ;;

  let semitons_shift = function
    | Triple_flat -> -3
    | Double_flat -> -2
    | Flat -> -1
    | Natural -> 0
    | Sharp -> 1
    | Double_sharp -> 2
    | Triple_sharp -> 3
  ;;

  let succ = function
    | Triple_flat -> Some Double_flat
    | Double_flat -> Some Flat
    | Flat -> Some Natural
    | Natural -> Some Sharp
    | Sharp -> Some Double_sharp
    | Double_sharp -> Some Triple_sharp
    | Triple_sharp -> None
  ;;

  let pred = function
    | Triple_flat -> None
    | Double_flat -> Some Triple_flat
    | Flat -> Some Double_flat
    | Natural -> Some Flat
    | Sharp -> Some Natural
    | Double_sharp -> Some Sharp
    | Triple_sharp -> Some Double_sharp
  ;;
end

type t =
  { letter_name : Letter_name.t
  ; symbol : Symbol.t
  ; octave_designation : Octave_designation.t
  }

let compare t ({ letter_name; symbol; octave_designation } as t2) : Ordering.t =
  if phys_equal t t2
  then Eq
  else (
    match Letter_name.compare t.letter_name letter_name with
    | (Lt | Gt) as r -> r
    | Eq ->
      (match Symbol.compare t.symbol symbol with
       | (Lt | Gt) as r -> r
       | Eq -> Octave_designation.compare t.octave_designation octave_designation))
;;

let equal t1 t2 =
  match compare t1 t2 with
  | Eq -> true
  | Lt | Gt -> false
;;

let to_dyn { letter_name; symbol; octave_designation } =
  Dyn.record
    [ "letter_name", letter_name |> Letter_name.to_dyn
    ; "symbol", symbol |> Symbol.to_dyn
    ; "octave_designation", octave_designation |> Octave_designation.to_dyn
    ]
;;

let to_string { letter_name; symbol; octave_designation } =
  Letter_name.to_string letter_name
  ^ Symbol.to_string symbol
  ^ Int.to_string octave_designation
;;
