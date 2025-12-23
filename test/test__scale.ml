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

let%expect_test "major and minor" =
  let major = Lazy.force Scale.major in
  let minor = Lazy.force Scale.minor in
  let tonics =
    let results = ref [] in
    let octave_designation = 1 in
    let rec aux letter_name =
      if (not (Note.Letter_name.equal letter_name C)) || List.is_empty !results
      then (
        let add symbol =
          results := { Note.letter_name; symbol; octave_designation } :: !results
        in
        add Flat;
        add Natural;
        add Sharp;
        aux (Note.Letter_name.succ letter_name))
    in
    aux Note.Letter_name.C;
    List.rev !results
  in
  let str arr = Array.map arr ~f:Note.to_string in
  List.iter tonics ~f:(fun tonic ->
    print_dyn (Scale.ascending major ~from:tonic |> str |> Dyn.array Dyn.string));
  [%expect
    {|
    [| "Cb1";  "Db1";  "Eb1";  "Fb1";  "Gb1";  "Ab1";  "Bb1";  "Cb2" |]
    [| "C1";  "D1";  "E1";  "F1";  "G1";  "A1";  "B1";  "C2" |]
    [| "C#1";  "D#1";  "E#1";  "F#1";  "G#1";  "A#1";  "B#1";  "C#2" |]
    [| "Db1";  "Eb1";  "F1";  "Gb1";  "Ab1";  "Bb1";  "C2";  "Db2" |]
    [| "D1";  "E1";  "F#1";  "G1";  "A1";  "B1";  "C#2";  "D2" |]
    [| "D#1";  "E#1";  "F##1";  "G#1";  "A#1";  "B#1";  "C##2";  "D#2" |]
    [| "Eb1";  "F1";  "G1";  "Ab1";  "Bb1";  "C2";  "D2";  "Eb2" |]
    [| "E1";  "F#1";  "G#1";  "A1";  "B1";  "C#2";  "D#2";  "E2" |]
    [| "E#1";  "F##1";  "G##1";  "A#1";  "B#1";  "C##2";  "D##2";  "E#2" |]
    [| "Fb1";  "Gb1";  "Ab1";  "Bbb1";  "Cb2";  "Db2";  "Eb2";  "Fb2" |]
    [| "F1";  "G1";  "A1";  "Bb1";  "C2";  "D2";  "E2";  "F2" |]
    [| "F#1";  "G#1";  "A#1";  "B1";  "C#2";  "D#2";  "E#2";  "F#2" |]
    [| "Gb1";  "Ab1";  "Bb1";  "Cb2";  "Db2";  "Eb2";  "F2";  "Gb2" |]
    [| "G1";  "A1";  "B1";  "C2";  "D2";  "E2";  "F#2";  "G2" |]
    [| "G#1";  "A#1";  "B#1";  "C#2";  "D#2";  "E#2";  "F##2";  "G#2" |]
    [| "Ab1";  "Bb1";  "C2";  "Db2";  "Eb2";  "F2";  "G2";  "Ab2" |]
    [| "A1";  "B1";  "C#2";  "D2";  "E2";  "F#2";  "G#2";  "A2" |]
    [| "A#1";  "B#1";  "C##2";  "D#2";  "E#2";  "F##2";  "G##2";  "A#2" |]
    [| "Bb1";  "C2";  "D2";  "Eb2";  "F2";  "G2";  "A2";  "Bb2" |]
    [| "B1";  "C#2";  "D#2";  "E2";  "F#2";  "G#2";  "A#2";  "B2" |]
    [| "B#1";  "C##2";  "D##2";  "E#2";  "F##2";  "G##2";  "A##2";  "B#2" |]
    |}];
  List.iter tonics ~f:(fun tonic ->
    print_dyn (Scale.descending major ~from:tonic |> str |> Dyn.array Dyn.string));
  [%expect
    {|
    [| "Cb1";  "Bb0";  "Ab0";  "Gb0";  "Fb0";  "Eb0";  "Db0";  "Cb0" |]
    [| "C1";  "B0";  "A0";  "G0";  "F0";  "E0";  "D0";  "C0" |]
    [| "C#1";  "B#0";  "A#0";  "G#0";  "F#0";  "E#0";  "D#0";  "C#0" |]
    [| "Db1";  "C1";  "Bb0";  "Ab0";  "Gb0";  "F0";  "Eb0";  "Db0" |]
    [| "D1";  "C#1";  "B0";  "A0";  "G0";  "F#0";  "E0";  "D0" |]
    [| "D#1";  "C##1";  "B#0";  "A#0";  "G#0";  "F##0";  "E#0";  "D#0" |]
    [| "Eb1";  "D1";  "C1";  "Bb0";  "Ab0";  "G0";  "F0";  "Eb0" |]
    [| "E1";  "D#1";  "C#1";  "B0";  "A0";  "G#0";  "F#0";  "E0" |]
    [| "E#1";  "D##1";  "C##1";  "B#0";  "A#0";  "G##0";  "F##0";  "E#0" |]
    [| "Fb1";  "Eb1";  "Db1";  "Cb1";  "Bbb0";  "Ab0";  "Gb0";  "Fb0" |]
    [| "F1";  "E1";  "D1";  "C1";  "Bb0";  "A0";  "G0";  "F0" |]
    [| "F#1";  "E#1";  "D#1";  "C#1";  "B0";  "A#0";  "G#0";  "F#0" |]
    [| "Gb1";  "F1";  "Eb1";  "Db1";  "Cb1";  "Bb0";  "Ab0";  "Gb0" |]
    [| "G1";  "F#1";  "E1";  "D1";  "C1";  "B0";  "A0";  "G0" |]
    [| "G#1";  "F##1";  "E#1";  "D#1";  "C#1";  "B#0";  "A#0";  "G#0" |]
    [| "Ab1";  "G1";  "F1";  "Eb1";  "Db1";  "C1";  "Bb0";  "Ab0" |]
    [| "A1";  "G#1";  "F#1";  "E1";  "D1";  "C#1";  "B0";  "A0" |]
    [| "A#1";  "G##1";  "F##1";  "E#1";  "D#1";  "C##1";  "B#0";  "A#0" |]
    [| "Bb1";  "A1";  "G1";  "F1";  "Eb1";  "D1";  "C1";  "Bb0" |]
    [| "B1";  "A#1";  "G#1";  "F#1";  "E1";  "D#1";  "C#1";  "B0" |]
    [| "B#1";  "A##1";  "G##1";  "F##1";  "E#1";  "D##1";  "C##1";  "B#0" |]
    |}];
  List.iter tonics ~f:(fun tonic ->
    print_dyn (Scale.ascending minor ~from:tonic |> str |> Dyn.array Dyn.string));
  [%expect
    {|
    [| "Cb1";  "Db1";  "Ebb1";  "Fb1";  "Gb1";  "Abb1";  "Bbb1";  "Cb2" |]
    [| "C1";  "D1";  "Eb1";  "F1";  "G1";  "Ab1";  "Bb1";  "C2" |]
    [| "C#1";  "D#1";  "E1";  "F#1";  "G#1";  "A1";  "B1";  "C#2" |]
    [| "Db1";  "Eb1";  "Fb1";  "Gb1";  "Ab1";  "Bbb1";  "Cb2";  "Db2" |]
    [| "D1";  "E1";  "F1";  "G1";  "A1";  "Bb1";  "C2";  "D2" |]
    [| "D#1";  "E#1";  "F#1";  "G#1";  "A#1";  "B1";  "C#2";  "D#2" |]
    [| "Eb1";  "F1";  "Gb1";  "Ab1";  "Bb1";  "Cb2";  "Db2";  "Eb2" |]
    [| "E1";  "F#1";  "G1";  "A1";  "B1";  "C2";  "D2";  "E2" |]
    [| "E#1";  "F##1";  "G#1";  "A#1";  "B#1";  "C#2";  "D#2";  "E#2" |]
    [| "Fb1";  "Gb1";  "Abb1";  "Bbb1";  "Cb2";  "Dbb2";  "Ebb2";  "Fb2" |]
    [| "F1";  "G1";  "Ab1";  "Bb1";  "C2";  "Db2";  "Eb2";  "F2" |]
    [| "F#1";  "G#1";  "A1";  "B1";  "C#2";  "D2";  "E2";  "F#2" |]
    [| "Gb1";  "Ab1";  "Bbb1";  "Cb2";  "Db2";  "Ebb2";  "Fb2";  "Gb2" |]
    [| "G1";  "A1";  "Bb1";  "C2";  "D2";  "Eb2";  "F2";  "G2" |]
    [| "G#1";  "A#1";  "B1";  "C#2";  "D#2";  "E2";  "F#2";  "G#2" |]
    [| "Ab1";  "Bb1";  "Cb2";  "Db2";  "Eb2";  "Fb2";  "Gb2";  "Ab2" |]
    [| "A1";  "B1";  "C2";  "D2";  "E2";  "F2";  "G2";  "A2" |]
    [| "A#1";  "B#1";  "C#2";  "D#2";  "E#2";  "F#2";  "G#2";  "A#2" |]
    [| "Bb1";  "C2";  "Db2";  "Eb2";  "F2";  "Gb2";  "Ab2";  "Bb2" |]
    [| "B1";  "C#2";  "D2";  "E2";  "F#2";  "G2";  "A2";  "B2" |]
    [| "B#1";  "C##2";  "D#2";  "E#2";  "F##2";  "G#2";  "A#2";  "B#2" |]
    |}];
  List.iter tonics ~f:(fun tonic ->
    print_dyn (Scale.descending minor ~from:tonic |> str |> Dyn.array Dyn.string));
  [%expect
    {|
    [| "Cb1";  "Bbb0";  "Abb0";  "Gb0";  "Fb0";  "Ebb0";  "Db0";  "Cb0" |]
    [| "C1";  "Bb0";  "Ab0";  "G0";  "F0";  "Eb0";  "D0";  "C0" |]
    [| "C#1";  "B0";  "A0";  "G#0";  "F#0";  "E0";  "D#0";  "C#0" |]
    [| "Db1";  "Cb1";  "Bbb0";  "Ab0";  "Gb0";  "Fb0";  "Eb0";  "Db0" |]
    [| "D1";  "C1";  "Bb0";  "A0";  "G0";  "F0";  "E0";  "D0" |]
    [| "D#1";  "C#1";  "B0";  "A#0";  "G#0";  "F#0";  "E#0";  "D#0" |]
    [| "Eb1";  "Db1";  "Cb1";  "Bb0";  "Ab0";  "Gb0";  "F0";  "Eb0" |]
    [| "E1";  "D1";  "C1";  "B0";  "A0";  "G0";  "F#0";  "E0" |]
    [| "E#1";  "D#1";  "C#1";  "B#0";  "A#0";  "G#0";  "F##0";  "E#0" |]
    [| "Fb1";  "Ebb1";  "Dbb1";  "Cb1";  "Bbb0";  "Abb0";  "Gb0";  "Fb0" |]
    [| "F1";  "Eb1";  "Db1";  "C1";  "Bb0";  "Ab0";  "G0";  "F0" |]
    [| "F#1";  "E1";  "D1";  "C#1";  "B0";  "A0";  "G#0";  "F#0" |]
    [| "Gb1";  "Fb1";  "Ebb1";  "Db1";  "Cb1";  "Bbb0";  "Ab0";  "Gb0" |]
    [| "G1";  "F1";  "Eb1";  "D1";  "C1";  "Bb0";  "A0";  "G0" |]
    [| "G#1";  "F#1";  "E1";  "D#1";  "C#1";  "B0";  "A#0";  "G#0" |]
    [| "Ab1";  "Gb1";  "Fb1";  "Eb1";  "Db1";  "Cb1";  "Bb0";  "Ab0" |]
    [| "A1";  "G1";  "F1";  "E1";  "D1";  "C1";  "B0";  "A0" |]
    [| "A#1";  "G#1";  "F#1";  "E#1";  "D#1";  "C#1";  "B#0";  "A#0" |]
    [| "Bb1";  "Ab1";  "Gb1";  "F1";  "Eb1";  "Db1";  "C1";  "Bb0" |]
    [| "B1";  "A1";  "G1";  "F#1";  "E1";  "D1";  "C#1";  "B0" |]
    [| "B#1";  "A#1";  "G#1";  "F##1";  "E#1";  "D#1";  "C##1";  "B#0" |]
    |}];
  ()
;;
