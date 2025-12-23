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

type t =
  { note : Note.t
  ; fingerboard_location : Fingerboard_location.t
  }

let to_dyn { note; fingerboard_location } =
  Dyn.record
    [ "note", note |> Note.to_dyn
    ; "fingerboard_location", fingerboard_location |> Fingerboard_location.to_dyn
    ]
;;

module Abbrev = struct
  type t = string * string * Roman_numeral.t

  let to_dyn (a, b, r) =
    Dyn.Tuple [ a |> Dyn.string; b |> Dyn.string; r |> Roman_numeral.to_dyn ]
  ;;
end

let to_abbrev { note; fingerboard_location = { fingerboard_position; string_number } } =
  Note.to_string note, Fingerboard_position.to_string fingerboard_position, string_number
;;

module Scale_abbrev = struct
  type t = (Roman_numeral.t * (string * string) list) list

  let to_dyn t =
    Dyn.list
      (fun (a, ts) ->
         Dyn.Tuple
           [ a |> Roman_numeral.to_dyn
           ; ts |> Dyn.list (fun (a, b) -> Dyn.Tuple [ a |> Dyn.string; b |> Dyn.string ])
           ])
      t
  ;;
end

let to_scale_abbrev ts =
  List.group ts ~break:(fun t1 t2 ->
    not
      (Roman_numeral.equal
         t1.fingerboard_location.string_number
         t2.fingerboard_location.string_number))
  |> List.map ~f:(fun ts ->
    let string_number = (List.hd ts).fingerboard_location.string_number in
    ( string_number
    , List.map ts ~f:(fun t ->
        ( Note.to_string t.note
        , Fingerboard_position.to_string t.fingerboard_location.fingerboard_position )) ))
;;
