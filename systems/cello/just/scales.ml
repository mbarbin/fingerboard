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

let make_major_pythagorean_scale ~from =
  let t = Lazy.force Just.t in
  make_scale t ~characterized_scale:Characterized_scale.major_pythagorean ~from
;;

let make_major_just_scale ~from =
  let t = Lazy.force Just.t in
  make_scale t ~characterized_scale:Characterized_scale.major_just ~from
;;

let lower_c =
  let t = Lazy.force Just.t in
  System.open_string t IV |> Option.get
;;

let%expect_test "c_major_just" =
  let scale = make_major_just_scale ~from:lower_c in
  print_dyn (scale |> Dyn.list Located_note.to_dyn);
  [%expect
    {|
    [ { note = { letter_name = C; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "0"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string = Zero
              }
          ; string_number = IV
          }
      }
    ; { note = { letter_name = D; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M2p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = -3 }; { prime = 3; exponent = 2 } ]
              }
          ; string_number = IV
          }
      }
    ; { note = { letter_name = E; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M3z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = -2 }; { prime = 5; exponent = 1 } ]
              }
          ; string_number = IV
          }
      }
    ; { note = { letter_name = F; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "4p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 2 }; { prime = 3; exponent = -1 } ]
              }
          ; string_number = IV
          }
      }
    ; { note = { letter_name = G; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "0"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string = Zero
              }
          ; string_number = III
          }
      }
    ; { note = { letter_name = A; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M2z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 1 }
                    ; { prime = 3; exponent = -2 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = III
          }
      }
    ; { note = { letter_name = B; symbol = Natural; octave_designation = 2 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M3z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = -2 }; { prime = 5; exponent = 1 } ]
              }
          ; string_number = III
          }
      }
    ; { note = { letter_name = C; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "4p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 2 }; { prime = 3; exponent = -1 } ]
              }
          ; string_number = III
          }
      }
    ; { note = { letter_name = D; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "0"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string = Zero
              }
          ; string_number = II
          }
      }
    ; { note = { letter_name = E; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M2z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 1 }
                    ; { prime = 3; exponent = -2 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = II
          }
      }
    ; { note = { letter_name = F; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m3p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 5 }; { prime = 3; exponent = -3 } ]
              }
          ; string_number = II
          }
      }
    ; { note = { letter_name = G; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "4p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 2 }; { prime = 3; exponent = -1 } ]
              }
          ; string_number = II
          }
      }
    ; { note = { letter_name = A; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "5z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 3 }
                    ; { prime = 3; exponent = -3 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = II
          }
      }
    ; { note = { letter_name = B; symbol = Natural; octave_designation = 3 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M2z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 1 }
                    ; { prime = 3; exponent = -2 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = C; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m3p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 5 }; { prime = 3; exponent = -3 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = D; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "4p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 2 }; { prime = 3; exponent = -1 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = E; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "5z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 3 }
                    ; { prime = 3; exponent = -3 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = F; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m6p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 7 }; { prime = 3; exponent = -4 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = G; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m7p"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 4 }; { prime = 3; exponent = -2 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = A; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "8z"
              ; at_octave = 0
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 5 }
                    ; { prime = 3; exponent = -4 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = B; symbol = Natural; octave_designation = 4 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M2z"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 1 }
                    ; { prime = 3; exponent = -2 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = C; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m3p"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 5 }; { prime = 3; exponent = -3 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = D; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "4p"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 2 }; { prime = 3; exponent = -1 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = E; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "5z"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 3 }
                    ; { prime = 3; exponent = -3 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = F; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m6p"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 7 }; { prime = 3; exponent = -4 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = G; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m7p"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 4 }; { prime = 3; exponent = -2 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = A; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "8z"
              ; at_octave = 1
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 5 }
                    ; { prime = 3; exponent = -4 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = B; symbol = Natural; octave_designation = 5 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "M2z"
              ; at_octave = 2
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 1 }
                    ; { prime = 3; exponent = -2 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = C; symbol = Natural; octave_designation = 6 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "m3p"
              ; at_octave = 2
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 5 }; { prime = 3; exponent = -3 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = D; symbol = Natural; octave_designation = 6 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "4p"
              ; at_octave = 2
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 2 }; { prime = 3; exponent = -1 } ]
              }
          ; string_number = I
          }
      }
    ; { note = { letter_name = E; symbol = Natural; octave_designation = 6 }
      ; fingerboard_location =
          { fingerboard_position =
              { name = "5z"
              ; at_octave = 2
              ; basis_acoustic_interval_to_the_open_string =
                  Reduced_natural_ratio
                    [ { prime = 2; exponent = 3 }
                    ; { prime = 3; exponent = -3 }
                    ; { prime = 5; exponent = 1 }
                    ]
              }
          ; string_number = I
          }
      }
    ]
    |}];
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn
    (scale
     |> Dyn.list (fun n -> n |> Located_note.to_abbrev |> Located_note.Abbrev.to_dyn));
  [%expect
    {|
    [ ("C2", "0", IV)
    ; ("D2", "M2p", IV)
    ; ("E2", "M3z", IV)
    ; ("F2", "4p", IV)
    ; ("G2", "0", III)
    ; ("A2", "M2z", III)
    ; ("B2", "M3z", III)
    ; ("C3", "4p", III)
    ; ("D3", "0", II)
    ; ("E3", "M2z", II)
    ; ("F3", "m3p", II)
    ; ("G3", "4p", II)
    ; ("A3", "5z", II)
    ; ("B3", "M2z", I)
    ; ("C4", "m3p", I)
    ; ("D4", "4p", I)
    ; ("E4", "5z", I)
    ; ("F4", "m6p", I)
    ; ("G4", "m7p", I)
    ; ("A4", "8z", I)
    ; ("B4", "M2z-1", I)
    ; ("C5", "m3p-1", I)
    ; ("D5", "4p-1", I)
    ; ("E5", "5z-1", I)
    ; ("F5", "m6p-1", I)
    ; ("G5", "m7p-1", I)
    ; ("A5", "8z-1", I)
    ; ("B5", "M2z-2", I)
    ; ("C6", "m3p-2", I)
    ; ("D6", "4p-2", I)
    ; ("E6", "5z-2", I)
    ]
    |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("C2", "0"); ("D2", "M2p"); ("E2", "M3z"); ("F2", "4p") ])
    ; (III, [ ("G2", "0"); ("A2", "M2z"); ("B2", "M3z"); ("C3", "4p") ])
    ; (II,
       [ ("D3", "0"); ("E3", "M2z"); ("F3", "m3p"); ("G3", "4p"); ("A3", "5z") ])
    ; (I,
       [ ("B3", "M2z")
       ; ("C4", "m3p")
       ; ("D4", "4p")
       ; ("E4", "5z")
       ; ("F4", "m6p")
       ; ("G4", "m7p")
       ; ("A4", "8z")
       ; ("B4", "M2z-1")
       ; ("C5", "m3p-1")
       ; ("D5", "4p-1")
       ; ("E5", "5z-1")
       ; ("F5", "m6p-1")
       ; ("G5", "m7p-1")
       ; ("A5", "8z-1")
       ; ("B5", "M2z-2")
       ; ("C6", "m3p-2")
       ; ("D6", "4p-2")
       ; ("E6", "5z-2")
       ])
    ]
    |}];
  ()
;;

(* Since [Just] is a superset of [Pythagorean], it supports all of the
   same pythagorean scales. We only test one here, since they all
   already appear in [pythagorean/scales.ml]. *)
let%expect_test "c_major_pythagorean" =
  let scale = make_major_pythagorean_scale ~from:lower_c in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("C2", "0"); ("D2", "M2p"); ("E2", "M3p"); ("F2", "4p") ])
    ; (III, [ ("G2", "0"); ("A2", "M2p"); ("B2", "M3p"); ("C3", "4p") ])
    ; (II, [ ("D3", "0"); ("E3", "M2p"); ("F3", "m3p"); ("G3", "4p") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2p")
       ; ("C4", "m3p")
       ; ("D4", "4p")
       ; ("E4", "5p")
       ; ("F4", "m6p")
       ; ("G4", "m7p")
       ; ("A4", "0-1")
       ; ("B4", "M2p-1")
       ; ("C5", "m3p-1")
       ; ("D5", "4p-1")
       ; ("E5", "5p-1")
       ; ("F5", "m6p-1")
       ; ("G5", "m7p-1")
       ; ("A5", "0-2")
       ; ("B5", "M2p-2")
       ; ("C6", "m3p-2")
       ; ("D6", "4p-2")
       ; ("E6", "5p-2")
       ])
    ]
    |}];
  ()
;;

let lower_dz_flat =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = D; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2z
      ; string_number = IV
      }
  }
;;

let%expect_test "d_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_dz_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("Db2", "m2z"); ("Eb2", "m3z"); ("F2", "4p"); ("Gb2", "d5z") ])
    ; (III, [ ("Ab2", "m2z"); ("Bb2", "m3p"); ("C3", "4p"); ("Db3", "d5z") ])
    ; (II, [ ("Eb3", "m2z"); ("F3", "m3p"); ("Gb3", "d4z"); ("Ab3", "d5z") ])
    ; (I,
       [ ("Bb3", "m2p")
       ; ("C4", "m3p")
       ; ("Db4", "d4z")
       ; ("Eb4", "d5z")
       ; ("F4", "m6p")
       ; ("Gb4", "d7z")
       ; ("Ab4", "d8z")
       ; ("Bb4", "m2p-1")
       ; ("C5", "m3p-1")
       ; ("Db5", "d4z-1")
       ; ("Eb5", "d5z-1")
       ; ("F5", "m6p-1")
       ; ("Gb5", "d7z-1")
       ; ("Ab5", "d8z-1")
       ; ("Bb5", "m2p-2")
       ; ("C6", "m3p-2")
       ; ("Db6", "d4z-2")
       ; ("Eb6", "d5z-2")
       ; ("F6", "m6p-2")
       ])
    ]
    |}];
  ()
;;

let lower_d =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = D; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2p
      ; string_number = IV
      }
  }
;;

let%expect_test "d_major_just" =
  let scale = make_major_just_scale ~from:lower_d in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 30 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("D2", "M2p"); ("E2", "M3p"); ("F#2", "A4z") ])
    ; (III, [ ("G2", "0"); ("A2", "M2p"); ("B2", "M3z"); ("C#3", "A4z") ])
    ; (II, [ ("D3", "0"); ("E3", "M2p"); ("F#3", "M3z"); ("G3", "4p") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2z")
       ; ("C#4", "M3z")
       ; ("D4", "4p")
       ; ("E4", "5p")
       ; ("F#4", "M6z")
       ; ("G4", "m7p")
       ; ("A4", "0-1")
       ; ("B4", "M2z-1")
       ; ("C#5", "M3z-1")
       ; ("D5", "4p-1")
       ; ("E5", "5p-1")
       ; ("F#5", "M6z-1")
       ; ("G5", "m7p-1")
       ; ("A5", "0-2")
       ; ("B5", "M2z-2")
       ; ("C#6", "M3z-2")
       ; ("D6", "4p-2")
       ; ("E6", "5p-2")
       ])
    ]
    |}];
  ()
;;

let lower_ez_flat =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3z
      ; string_number = IV
      }
  }
;;

let%expect_test "e_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_ez_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 30 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("Eb2", "m3z"); ("F2", "4z") ])
    ; (III, [ ("G2", "0"); ("Ab2", "m2z"); ("Bb2", "m3z"); ("C3", "4p") ])
    ; (II,
       [ ("D3", "0")
       ; ("Eb3", "m2z")
       ; ("F3", "m3z")
       ; ("G3", "4p")
       ; ("Ab3", "d5z")
       ])
    ; (I,
       [ ("Bb3", "m2z")
       ; ("C4", "m3p")
       ; ("D4", "4p")
       ; ("Eb4", "d5z")
       ; ("F4", "m6z")
       ; ("G4", "m7p")
       ; ("Ab4", "d8z")
       ; ("Bb4", "m2z-1")
       ; ("C5", "m3p-1")
       ; ("D5", "4p-1")
       ; ("Eb5", "d5z-1")
       ; ("F5", "m6z-1")
       ; ("G5", "m7p-1")
       ; ("Ab5", "d8z-1")
       ; ("Bb5", "m2z-2")
       ; ("C6", "m3p-2")
       ; ("D6", "4p-2")
       ; ("Eb6", "d5z-2")
       ; ("F6", "m6z-2")
       ])
    ]
    |}];
  ()
;;

let lower_e =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = E; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3p
      ; string_number = IV
      }
  }
;;

let%expect_test "e_major_just" =
  let scale = make_major_just_scale ~from:lower_e in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 29 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("E2", "M3p"); ("F#2", "A4p") ])
    ; (III, [ ("G#2", "A1z"); ("A2", "M2p"); ("B2", "M3p"); ("C#3", "A4z") ])
    ; (II, [ ("D#3", "A1z"); ("E3", "M2p"); ("F#3", "M3p"); ("G#3", "A4z") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2p")
       ; ("C#4", "M3z")
       ; ("D#4", "A4z")
       ; ("E4", "5p")
       ; ("F#4", "M6p")
       ; ("G#4", "M7z")
       ; ("A4", "0-1")
       ; ("B4", "M2p-1")
       ; ("C#5", "M3z-1")
       ; ("D#5", "A4z-1")
       ; ("E5", "5p-1")
       ; ("F#5", "M6p-1")
       ; ("G#5", "M7z-1")
       ; ("A5", "0-2")
       ; ("B5", "M2p-2")
       ; ("C#6", "M3z-2")
       ; ("D#6", "A4z-2")
       ; ("E6", "5p-2")
       ])
    ]
    |}];
  ()
;;

let lower_fp =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4p
      ; string_number = IV
      }
  }
;;

let%expect_test "fp_major_just" =
  let scale = make_major_just_scale ~from:lower_fp in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 12 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("F2", "4p") ])
    ; (III,
       [ ("G2", "0"); ("A2", "M2z"); ("Bb2", "m3p"); ("C3", "4p"); ("D3", "5z") ])
    ; (II, [ ("E3", "M2z"); ("F3", "m3p"); ("G3", "4p"); ("A3", "5z") ])
    ; (I, [ ("Bb3", "m2p"); ("C4", "m3p") ])
    ]
    |}];
  ()
;;

let lower_fz =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4z
      ; string_number = IV
      }
  }
;;

let%expect_test "fz_major_just" =
  let scale = make_major_just_scale ~from:lower_fz in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 1 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect {| [ (IV, [ ("F2", "4z") ]) ] |}];
  ()
;;

let lower_fp_sharp =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = F; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A4p
      ; string_number = IV
      }
  }
;;

let%expect_test "f_sharp_major_just" =
  let scale = make_major_just_scale ~from:lower_fp_sharp in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 2 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect {| [ (IV, [ ("F#2", "A4p") ]); (III, [ ("G#2", "A1p") ]) ] |}];
  ()
;;

let lower_gz_flat =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = G; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `d5z
      ; string_number = IV
      }
  }
;;

let%expect_test "g_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_gz_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 28 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("Gb2", "d5z") ])
    ; (III, [ ("Ab2", "m2z"); ("Bb2", "m3p"); ("Cb3", "d4z"); ("Db3", "d5z") ])
    ; (II, [ ("Eb3", "m2p"); ("F3", "m3p"); ("Gb3", "d4z"); ("Ab3", "d5z") ])
    ; (I,
       [ ("Bb3", "m2p")
       ; ("Cb4", "d3z")
       ; ("Db4", "d4z")
       ; ("Eb4", "d5p")
       ; ("F4", "m6p")
       ; ("Gb4", "d7z")
       ; ("Ab4", "d8z")
       ; ("Bb4", "m2p-1")
       ; ("Cb5", "d3z-1")
       ; ("Db5", "d4z-1")
       ; ("Eb5", "d5p-1")
       ; ("F5", "m6p-1")
       ; ("Gb5", "d7z-1")
       ; ("Ab5", "d8z-1")
       ; ("Bb5", "m2p-2")
       ; ("Cb6", "d3z-2")
       ; ("Db6", "d4z-2")
       ; ("Eb6", "d5p-2")
       ; ("F6", "m6p-2")
       ])
    ]
    |}];
  ()
;;

let lower_g =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = G; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `open_string
      ; string_number = III
      }
  }
;;

let%expect_test "g_major_just" =
  let scale = make_major_just_scale ~from:lower_g in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 27 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("G2", "0"); ("A2", "M2p"); ("B2", "M3z"); ("C3", "4p") ])
    ; (II, [ ("D3", "0"); ("E3", "M2z"); ("F#3", "M3z"); ("G3", "4p") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2z")
       ; ("C4", "m3p")
       ; ("D4", "4p")
       ; ("E4", "5z")
       ; ("F#4", "M6z")
       ; ("G4", "m7p")
       ; ("A4", "0-1")
       ; ("B4", "M2z-1")
       ; ("C5", "m3p-1")
       ; ("D5", "4p-1")
       ; ("E5", "5z-1")
       ; ("F#5", "M6z-1")
       ; ("G5", "m7p-1")
       ; ("A5", "0-2")
       ; ("B5", "M2z-2")
       ; ("C6", "m3p-2")
       ; ("D6", "4p-2")
       ; ("E6", "5z-2")
       ])
    ]
    |}];
  ()
;;

let lower_az_flat =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = A; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2z
      ; string_number = III
      }
  }
;;

let%expect_test "a_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_az_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 27 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("Ab2", "m2z"); ("Bb2", "m3z"); ("C3", "4p"); ("Db3", "d5z") ])
    ; (II, [ ("Eb3", "m2z"); ("F3", "m3p"); ("G3", "4p"); ("Ab3", "d5z") ])
    ; (I,
       [ ("Bb3", "m2z")
       ; ("C4", "m3p")
       ; ("Db4", "d4z")
       ; ("Eb4", "d5z")
       ; ("F4", "m6p")
       ; ("G4", "m7p")
       ; ("Ab4", "d8z")
       ; ("Bb4", "m2z-1")
       ; ("C5", "m3p-1")
       ; ("Db5", "d4z-1")
       ; ("Eb5", "d5z-1")
       ; ("F5", "m6p-1")
       ; ("G5", "m7p-1")
       ; ("Ab5", "d8z-1")
       ; ("Bb5", "m2z-2")
       ; ("C6", "m3p-2")
       ; ("Db6", "d4z-2")
       ; ("Eb6", "d5z-2")
       ; ("F6", "m6p-2")
       ])
    ]
    |}];
  ()
;;

let lower_a =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = A; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2p
      ; string_number = III
      }
  }
;;

let%expect_test "a_major_just" =
  let scale = make_major_just_scale ~from:lower_a in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 26 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("A2", "M2p"); ("B2", "M3p"); ("C#3", "A4z") ])
    ; (II, [ ("D3", "0"); ("E3", "M2p"); ("F#3", "M3z"); ("G#3", "A4z") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2p")
       ; ("C#4", "M3z")
       ; ("D4", "4p")
       ; ("E4", "5p")
       ; ("F#4", "M6z")
       ; ("G#4", "M7z")
       ; ("A4", "0-1")
       ; ("B4", "M2p-1")
       ; ("C#5", "M3z-1")
       ; ("D5", "4p-1")
       ; ("E5", "5p-1")
       ; ("F#5", "M6z-1")
       ; ("G#5", "M7z-1")
       ; ("A5", "0-2")
       ; ("B5", "M2p-2")
       ; ("C#6", "M3z-2")
       ; ("D6", "4p-2")
       ; ("E6", "5p-2")
       ])
    ]
    |}];
  ()
;;

let lower_bz_flat =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = B; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3z
      ; string_number = III
      }
  }
;;

let%expect_test "b_flat_major_just" =
  let scale = make_major_just_scale ~from:lower_bz_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 26 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("Bb2", "m3z"); ("C3", "4z") ])
    ; (II, [ ("D3", "0"); ("Eb3", "m2z"); ("F3", "m3z"); ("G3", "4p") ])
    ; (I,
       [ ("A3", "0")
       ; ("Bb3", "m2z")
       ; ("C4", "m3z")
       ; ("D4", "4p")
       ; ("Eb4", "d5z")
       ; ("F4", "m6z")
       ; ("G4", "m7p")
       ; ("A4", "0-1")
       ; ("Bb4", "m2z-1")
       ; ("C5", "m3z-1")
       ; ("D5", "4p-1")
       ; ("Eb5", "d5z-1")
       ; ("F5", "m6z-1")
       ; ("G5", "m7p-1")
       ; ("A5", "0-2")
       ; ("Bb5", "m2z-2")
       ; ("C6", "m3z-2")
       ; ("D6", "4p-2")
       ; ("Eb6", "d5z-2")
       ; ("F6", "m6z-2")
       ])
    ]
    |}];
  ()
;;

let lower_bp =
  let t = Lazy.force Just.t in
  { Located_note.note = { letter_name = B; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3p
      ; string_number = III
      }
  }
;;

let%expect_test "b_major_just" =
  let scale = make_major_just_scale ~from:lower_bp in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 25 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("B2", "M3p"); ("C#3", "A4p") ])
    ; (II, [ ("D#3", "A1z"); ("E3", "M2p"); ("F#3", "M3p"); ("G#3", "A4z") ])
    ; (I,
       [ ("A#3", "A1z")
       ; ("B3", "M2p")
       ; ("C#4", "M3p")
       ; ("D#4", "A4z")
       ; ("E4", "5p")
       ; ("F#4", "M6p")
       ; ("G#4", "M7z")
       ; ("A#4", "A1z-1")
       ; ("B4", "M2p-1")
       ; ("C#5", "M3p-1")
       ; ("D#5", "A4z-1")
       ; ("E5", "5p-1")
       ; ("F#5", "M6p-1")
       ; ("G#5", "M7z-1")
       ; ("A#5", "A1z-2")
       ; ("B5", "M2p-2")
       ; ("C#6", "M3p-2")
       ; ("D#6", "A4z-2")
       ; ("E6", "5p-2")
       ])
    ]
    |}];
  ()
;;
