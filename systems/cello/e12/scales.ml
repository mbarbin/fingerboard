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

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  let t = Lazy.force E12.t in
  make_scale t ~characterized_scale:Characterized_scale.major_e12 ~from
;;

let lower_c =
  let t = Lazy.force E12.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let%expect_test "c_major" =
  let scale = make_major_scale ~from:lower_c in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("C2", "0"); ("D2", "M2e"); ("E2", "M3e"); ("F2", "4e") ])
    ; (III, [ ("G2", "0"); ("A2", "M2e"); ("B2", "M3e"); ("C3", "4e") ])
    ; (II, [ ("D3", "0"); ("E3", "M2e"); ("F3", "m3e"); ("G3", "4e") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2e")
       ; ("C4", "m3e")
       ; ("D4", "4e")
       ; ("E4", "5e")
       ; ("F4", "m6e")
       ; ("G4", "m7e")
       ; ("A4", "0-1")
       ; ("B4", "M2e-1")
       ; ("C5", "m3e-1")
       ; ("D5", "4e-1")
       ; ("E5", "5e-1")
       ; ("F5", "m6e-1")
       ; ("G5", "m7e-1")
       ; ("A5", "0-2")
       ; ("B5", "M2e-2")
       ; ("C6", "m3e-2")
       ; ("D6", "4e-2")
       ; ("E6", "5e-2")
       ])
    ]
    |}];
  ()
;;

let lower_e_flat =
  let t = Lazy.force E12.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3e
      ; string_number = IV
      }
  }
;;

let%expect_test "e_flat_major" =
  let scale = make_major_scale ~from:lower_e_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 30 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("Eb2", "m3e"); ("F2", "4e") ])
    ; (III, [ ("G2", "0"); ("Ab2", "m2e"); ("Bb2", "m3e"); ("C3", "4e") ])
    ; (II,
       [ ("D3", "0")
       ; ("Eb3", "m2e")
       ; ("F3", "m3e")
       ; ("G3", "4e")
       ; ("Ab3", "A4e")
       ])
    ; (I,
       [ ("Bb3", "m2e")
       ; ("C4", "m3e")
       ; ("D4", "4e")
       ; ("Eb4", "A4e")
       ; ("F4", "m6e")
       ; ("G4", "m7e")
       ; ("Ab4", "M7e")
       ; ("Bb4", "m2e-1")
       ; ("C5", "m3e-1")
       ; ("D5", "4e-1")
       ; ("Eb5", "A4e-1")
       ; ("F5", "m6e-1")
       ; ("G5", "m7e-1")
       ; ("Ab5", "M7e-1")
       ; ("Bb5", "m2e-2")
       ; ("C6", "m3e-2")
       ; ("D6", "4e-2")
       ; ("Eb6", "A4e-2")
       ; ("F6", "m6e-2")
       ])
    ]
    |}];
  ()
;;
