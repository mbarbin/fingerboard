(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

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
