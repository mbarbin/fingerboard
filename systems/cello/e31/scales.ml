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
  let t = Lazy.force E31.t in
  make_scale t ~characterized_scale:Characterized_scale.major_e31 ~from
;;

let lower_c =
  let t = Lazy.force E31.t in
  System.open_string t IV |> Option.get
;;

let%expect_test "c_major" =
  let scale = make_major_scale ~from:lower_c in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("C2", "0"); ("D2", "M2-e31"); ("E2", "M3-e31"); ("F2", "4-e31") ])
    ; (III, [ ("G2", "0"); ("A2", "M2-e31"); ("B2", "M3-e31"); ("C3", "4-e31") ])
    ; (II, [ ("D3", "0"); ("E3", "M2-e31"); ("F3", "m3-e31"); ("G3", "4-e31") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2-e31")
       ; ("C4", "m3-e31")
       ; ("D4", "4-e31")
       ; ("E4", "5-e31")
       ; ("F4", "m6-e31")
       ; ("G4", "m7-e31")
       ; ("A4", "0-1")
       ; ("B4", "M2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("A5", "0-2")
       ; ("B5", "M2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_c_sharp =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = C; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A1_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "c_sharp_major" =
  let scale = make_major_scale ~from:lower_c_sharp in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV,
       [ ("C#2", "A1-e31")
       ; ("D#2", "A2-e31")
       ; ("E#2", "A3-e31")
       ; ("F#2", "A4-e31")
       ])
    ; (III,
       [ ("G#2", "A1-e31")
       ; ("A#2", "A2-e31")
       ; ("B#2", "A3-e31")
       ; ("C#3", "A4-e31")
       ])
    ; (II,
       [ ("D#3", "A1-e31")
       ; ("E#3", "A2-e31")
       ; ("F#3", "M3-e31")
       ; ("G#3", "A4-e31")
       ])
    ; (I,
       [ ("A#3", "A1-e31")
       ; ("B#3", "A2-e31")
       ; ("C#4", "M3-e31")
       ; ("D#4", "A4-e31")
       ; ("E#4", "A5-e31")
       ; ("F#4", "M6-e31")
       ; ("G#4", "M7-e31")
       ; ("A#4", "A1-e31-1")
       ; ("B#4", "A2-e31-1")
       ; ("C#5", "M3-e31-1")
       ; ("D#5", "A4-e31-1")
       ; ("E#5", "A5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G#5", "M7-e31-1")
       ; ("A#5", "A1-e31-2")
       ; ("B#5", "A2-e31-2")
       ; ("C#6", "M3-e31-2")
       ; ("D#6", "A4-e31-2")
       ; ("E#6", "A5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_d_flat =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = D; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "d_flat_major" =
  let scale = make_major_scale ~from:lower_d_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 31 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV,
       [ ("Db2", "m2-e31")
       ; ("Eb2", "m3-e31")
       ; ("F2", "4-e31")
       ; ("Gb2", "d5-e31")
       ])
    ; (III,
       [ ("Ab2", "m2-e31")
       ; ("Bb2", "m3-e31")
       ; ("C3", "4-e31")
       ; ("Db3", "d5-e31")
       ])
    ; (II,
       [ ("Eb3", "m2-e31")
       ; ("F3", "m3-e31")
       ; ("Gb3", "d4-e31")
       ; ("Ab3", "d5-e31")
       ])
    ; (I,
       [ ("Bb3", "m2-e31")
       ; ("C4", "m3-e31")
       ; ("Db4", "d4-e31")
       ; ("Eb4", "d5-e31")
       ; ("F4", "m6-e31")
       ; ("Gb4", "d7-e31")
       ; ("Ab4", "d8-e31")
       ; ("Bb4", "m2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("Db5", "d4-e31-1")
       ; ("Eb5", "d5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("Gb5", "d7-e31-1")
       ; ("Ab5", "d8-e31-1")
       ; ("Bb5", "m2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("Db6", "d4-e31-2")
       ; ("Eb6", "d5-e31-2")
       ; ("F6", "m6-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_d =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = D; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "d_major" =
  let scale = make_major_scale ~from:lower_d in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 30 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("D2", "M2-e31"); ("E2", "M3-e31"); ("F#2", "A4-e31") ])
    ; (III,
       [ ("G2", "0"); ("A2", "M2-e31"); ("B2", "M3-e31"); ("C#3", "A4-e31") ])
    ; (II, [ ("D3", "0"); ("E3", "M2-e31"); ("F#3", "M3-e31"); ("G3", "4-e31") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2-e31")
       ; ("C#4", "M3-e31")
       ; ("D4", "4-e31")
       ; ("E4", "5-e31")
       ; ("F#4", "M6-e31")
       ; ("G4", "m7-e31")
       ; ("A4", "0-1")
       ; ("B4", "M2-e31-1")
       ; ("C#5", "M3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("A5", "0-2")
       ; ("B5", "M2-e31-2")
       ; ("C#6", "M3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_e_flat =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = E; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3_e31
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
    [ (IV, [ ("Eb2", "m3-e31"); ("F2", "4-e31") ])
    ; (III,
       [ ("G2", "0"); ("Ab2", "m2-e31"); ("Bb2", "m3-e31"); ("C3", "4-e31") ])
    ; (II,
       [ ("D3", "0")
       ; ("Eb3", "m2-e31")
       ; ("F3", "m3-e31")
       ; ("G3", "4-e31")
       ; ("Ab3", "d5-e31")
       ])
    ; (I,
       [ ("Bb3", "m2-e31")
       ; ("C4", "m3-e31")
       ; ("D4", "4-e31")
       ; ("Eb4", "d5-e31")
       ; ("F4", "m6-e31")
       ; ("G4", "m7-e31")
       ; ("Ab4", "d8-e31")
       ; ("Bb4", "m2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("Eb5", "d5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("Ab5", "d8-e31-1")
       ; ("Bb5", "m2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("Eb6", "d5-e31-2")
       ; ("F6", "m6-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_e =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = E; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "e_major" =
  let scale = make_major_scale ~from:lower_e in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 29 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("E2", "M3-e31"); ("F#2", "A4-e31") ])
    ; (III,
       [ ("G#2", "A1-e31")
       ; ("A2", "M2-e31")
       ; ("B2", "M3-e31")
       ; ("C#3", "A4-e31")
       ])
    ; (II,
       [ ("D#3", "A1-e31")
       ; ("E3", "M2-e31")
       ; ("F#3", "M3-e31")
       ; ("G#3", "A4-e31")
       ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2-e31")
       ; ("C#4", "M3-e31")
       ; ("D#4", "A4-e31")
       ; ("E4", "5-e31")
       ; ("F#4", "M6-e31")
       ; ("G#4", "M7-e31")
       ; ("A4", "0-1")
       ; ("B4", "M2-e31-1")
       ; ("C#5", "M3-e31-1")
       ; ("D#5", "A4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G#5", "M7-e31-1")
       ; ("A5", "0-2")
       ; ("B5", "M2-e31-2")
       ; ("C#6", "M3-e31-2")
       ; ("D#6", "A4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_f =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = F; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `P4_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "f_major" =
  let scale = make_major_scale ~from:lower_f in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 28 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("F2", "4-e31") ])
    ; (III, [ ("G2", "0"); ("A2", "M2-e31"); ("Bb2", "m3-e31"); ("C3", "4-e31") ])
    ; (II, [ ("D3", "0"); ("E3", "M2-e31"); ("F3", "m3-e31"); ("G3", "4-e31") ])
    ; (I,
       [ ("A3", "0")
       ; ("Bb3", "m2-e31")
       ; ("C4", "m3-e31")
       ; ("D4", "4-e31")
       ; ("E4", "5-e31")
       ; ("F4", "m6-e31")
       ; ("G4", "m7-e31")
       ; ("A4", "0-1")
       ; ("Bb4", "m2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("A5", "0-2")
       ; ("Bb5", "m2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_f_sharp =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = F; symbol = Sharp; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `A4_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "f_sharp_major" =
  let scale = make_major_scale ~from:lower_f_sharp in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 28 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("F#2", "A4-e31") ])
    ; (III,
       [ ("G#2", "A1-e31")
       ; ("A#2", "A2-e31")
       ; ("B2", "M3-e31")
       ; ("C#3", "A4-e31")
       ])
    ; (II,
       [ ("D#3", "A1-e31")
       ; ("E#3", "A2-e31")
       ; ("F#3", "M3-e31")
       ; ("G#3", "A4-e31")
       ])
    ; (I,
       [ ("A#3", "A1-e31")
       ; ("B3", "M2-e31")
       ; ("C#4", "M3-e31")
       ; ("D#4", "A4-e31")
       ; ("E#4", "A5-e31")
       ; ("F#4", "M6-e31")
       ; ("G#4", "M7-e31")
       ; ("A#4", "A1-e31-1")
       ; ("B4", "M2-e31-1")
       ; ("C#5", "M3-e31-1")
       ; ("D#5", "A4-e31-1")
       ; ("E#5", "A5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G#5", "M7-e31-1")
       ; ("A#5", "A1-e31-2")
       ; ("B5", "M2-e31-2")
       ; ("C#6", "M3-e31-2")
       ; ("D#6", "A4-e31-2")
       ; ("E#6", "A5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_g_flat =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = G; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `d5_e31
      ; string_number = IV
      }
  }
;;

let%expect_test "g_flat_major" =
  let scale = make_major_scale ~from:lower_g_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 28 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (IV, [ ("Gb2", "d5-e31") ])
    ; (III,
       [ ("Ab2", "m2-e31")
       ; ("Bb2", "m3-e31")
       ; ("Cb3", "d4-e31")
       ; ("Db3", "d5-e31")
       ])
    ; (II,
       [ ("Eb3", "m2-e31")
       ; ("F3", "m3-e31")
       ; ("Gb3", "d4-e31")
       ; ("Ab3", "d5-e31")
       ])
    ; (I,
       [ ("Bb3", "m2-e31")
       ; ("Cb4", "d3-e31")
       ; ("Db4", "d4-e31")
       ; ("Eb4", "d5-e31")
       ; ("F4", "m6-e31")
       ; ("Gb4", "d7-e31")
       ; ("Ab4", "d8-e31")
       ; ("Bb4", "m2-e31-1")
       ; ("Cb5", "d3-e31-1")
       ; ("Db5", "d4-e31-1")
       ; ("Eb5", "d5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("Gb5", "d7-e31-1")
       ; ("Ab5", "d8-e31-1")
       ; ("Bb5", "m2-e31-2")
       ; ("Cb6", "d3-e31-2")
       ; ("Db6", "d4-e31-2")
       ; ("Eb6", "d5-e31-2")
       ; ("F6", "m6-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_g =
  let t = Lazy.force E31.t in
  System.open_string t III |> Option.get
;;

let%expect_test "g_major" =
  let scale = make_major_scale ~from:lower_g in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 27 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("G2", "0"); ("A2", "M2-e31"); ("B2", "M3-e31"); ("C3", "4-e31") ])
    ; (II, [ ("D3", "0"); ("E3", "M2-e31"); ("F#3", "M3-e31"); ("G3", "4-e31") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2-e31")
       ; ("C4", "m3-e31")
       ; ("D4", "4-e31")
       ; ("E4", "5-e31")
       ; ("F#4", "M6-e31")
       ; ("G4", "m7-e31")
       ; ("A4", "0-1")
       ; ("B4", "M2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("A5", "0-2")
       ; ("B5", "M2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_a_flat =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = A; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m2_e31
      ; string_number = III
      }
  }
;;

let%expect_test "a_flat_major" =
  let scale = make_major_scale ~from:lower_a_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 27 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III,
       [ ("Ab2", "m2-e31")
       ; ("Bb2", "m3-e31")
       ; ("C3", "4-e31")
       ; ("Db3", "d5-e31")
       ])
    ; (II,
       [ ("Eb3", "m2-e31")
       ; ("F3", "m3-e31")
       ; ("G3", "4-e31")
       ; ("Ab3", "d5-e31")
       ])
    ; (I,
       [ ("Bb3", "m2-e31")
       ; ("C4", "m3-e31")
       ; ("Db4", "d4-e31")
       ; ("Eb4", "d5-e31")
       ; ("F4", "m6-e31")
       ; ("G4", "m7-e31")
       ; ("Ab4", "d8-e31")
       ; ("Bb4", "m2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("Db5", "d4-e31-1")
       ; ("Eb5", "d5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("Ab5", "d8-e31-1")
       ; ("Bb5", "m2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("Db6", "d4-e31-2")
       ; ("Eb6", "d5-e31-2")
       ; ("F6", "m6-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_a =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = A; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M2_e31
      ; string_number = III
      }
  }
;;

let%expect_test "a_major" =
  let scale = make_major_scale ~from:lower_a in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 26 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("A2", "M2-e31"); ("B2", "M3-e31"); ("C#3", "A4-e31") ])
    ; (II,
       [ ("D3", "0"); ("E3", "M2-e31"); ("F#3", "M3-e31"); ("G#3", "A4-e31") ])
    ; (I,
       [ ("A3", "0")
       ; ("B3", "M2-e31")
       ; ("C#4", "M3-e31")
       ; ("D4", "4-e31")
       ; ("E4", "5-e31")
       ; ("F#4", "M6-e31")
       ; ("G#4", "M7-e31")
       ; ("A4", "0-1")
       ; ("B4", "M2-e31-1")
       ; ("C#5", "M3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G#5", "M7-e31-1")
       ; ("A5", "0-2")
       ; ("B5", "M2-e31-2")
       ; ("C#6", "M3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_b_flat =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = B; symbol = Flat; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `m3_e31
      ; string_number = III
      }
  }
;;

let%expect_test "b_flat_major" =
  let scale = make_major_scale ~from:lower_b_flat in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 26 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("Bb2", "m3-e31"); ("C3", "4-e31") ])
    ; (II, [ ("D3", "0"); ("Eb3", "m2-e31"); ("F3", "m3-e31"); ("G3", "4-e31") ])
    ; (I,
       [ ("A3", "0")
       ; ("Bb3", "m2-e31")
       ; ("C4", "m3-e31")
       ; ("D4", "4-e31")
       ; ("Eb4", "d5-e31")
       ; ("F4", "m6-e31")
       ; ("G4", "m7-e31")
       ; ("A4", "0-1")
       ; ("Bb4", "m2-e31-1")
       ; ("C5", "m3-e31-1")
       ; ("D5", "4-e31-1")
       ; ("Eb5", "d5-e31-1")
       ; ("F5", "m6-e31-1")
       ; ("G5", "m7-e31-1")
       ; ("A5", "0-2")
       ; ("Bb5", "m2-e31-2")
       ; ("C6", "m3-e31-2")
       ; ("D6", "4-e31-2")
       ; ("Eb6", "d5-e31-2")
       ; ("F6", "m6-e31-2")
       ])
    ]
    |}];
  ()
;;

let lower_b =
  let t = Lazy.force E31.t in
  { Located_note.note = { letter_name = B; symbol = Natural; octave_designation = 2 }
  ; fingerboard_location =
      { fingerboard_position = Cello.find_fingerboard_position_exn t `M3_e31
      ; string_number = III
      }
  }
;;

let%expect_test "b_major" =
  let scale = make_major_scale ~from:lower_b in
  print_dyn (List.length scale |> Dyn.int);
  [%expect {| 25 |}];
  print_dyn (scale |> Located_note.to_scale_abbrev |> Located_note.Scale_abbrev.to_dyn);
  [%expect
    {|
    [ (III, [ ("B2", "M3-e31"); ("C#3", "A4-e31") ])
    ; (II,
       [ ("D#3", "A1-e31")
       ; ("E3", "M2-e31")
       ; ("F#3", "M3-e31")
       ; ("G#3", "A4-e31")
       ])
    ; (I,
       [ ("A#3", "A1-e31")
       ; ("B3", "M2-e31")
       ; ("C#4", "M3-e31")
       ; ("D#4", "A4-e31")
       ; ("E4", "5-e31")
       ; ("F#4", "M6-e31")
       ; ("G#4", "M7-e31")
       ; ("A#4", "A1-e31-1")
       ; ("B4", "M2-e31-1")
       ; ("C#5", "M3-e31-1")
       ; ("D#5", "A4-e31-1")
       ; ("E5", "5-e31-1")
       ; ("F#5", "M6-e31-1")
       ; ("G#5", "M7-e31-1")
       ; ("A#5", "A1-e31-2")
       ; ("B5", "M2-e31-2")
       ; ("C#6", "M3-e31-2")
       ; ("D#6", "A4-e31-2")
       ; ("E6", "5-e31-2")
       ])
    ]
    |}];
  ()
;;
