open! Core
open! Cemper

let qualities number ~doubly_augmented =
  let open Interval.Quality in
  List.concat
    [ (if Interval.Number.accepts_minor_major_quality number
      then [ Minor; Major ]
      else [ Perfect ])
    ; [ Augmented; Diminished ]
    ; (if doubly_augmented then [ Doubly_augmented; Doubly_diminished ] else [])
    ]
  |> List.sort ~compare:Interval.Quality.compare
;;

let ts =
  lazy
    (List.bind Interval.Number.all ~f:(fun number ->
       List.map (qualities number ~doubly_augmented:true) ~f:(fun quality ->
         Interval.{ number; quality; additional_octaves = 0 })))
;;

let%expect_test "sort" =
  let ts = force ts in
  let ts =
    let table = Hashtbl.create (module Int) in
    List.iter ts ~f:(fun t ->
      let number_of_semitons = Interval.number_of_semitons t in
      if 0 <= number_of_semitons && number_of_semitons <= 24
      then (
        let queue =
          Hashtbl.find_or_add table number_of_semitons ~default:(fun () ->
            Queue.create ())
        in
        Queue.enqueue queue t));
    Hashtbl.to_alist table
    |> List.sort ~compare:(fun (i, _) (j, _) -> Int.compare i j)
    |> List.map ~f:(fun (i, queue) ->
         let intervals = Queue.to_list queue |> List.sort ~compare:Interval.compare in
         let canonical_interval =
           List.map intervals ~f:(fun t ->
             let priority =
               match t.quality with
               | Perfect | Minor | Major -> 0
               | Augmented -> 1
               | Diminished -> 2
               | Doubly_augmented -> 3
               | Doubly_diminished -> 4
             in
             priority, t)
           |> List.sort ~compare:(fun (i, _) (j, _) -> Int.compare i j)
           |> List.map ~f:snd
           |> List.hd_exn
         in
         i, canonical_interval, intervals)
  in
  List.iter ts ~f:(fun (i, canonical_interval, intervals) ->
    let intervals = List.map intervals ~f:Interval.to_string in
    let canonical_interval = Interval.name canonical_interval in
    print_s [%sexp (i : int), (canonical_interval : string), (intervals : string list)]);
  [%expect
    {|
    (0 unison (P1 d2))
    (1 "minor second" (A1 m2 dd3))
    (2 "major second" (AA1 M2 d3))
    (3 "minor third" (A2 m3 dd4))
    (4 "major third" (AA2 M3 d4))
    (5 fourth (A3 P4 dd5))
    (6 "augmented fourth" (AA3 A4 d5 dd6))
    (7 fifth (AA4 P5 d6))
    (8 "minor sixth" (A5 m6 dd7))
    (9 "major sixth" (AA5 M6 d7))
    (10 "minor seventh" (A6 m7 dd8))
    (11 "major seventh" (AA6 M7 d8))
    (12 octave (A7 P8))
    (13 "augmented octave" (AA7 A8))
    (14 "doubly augmented octave" (AA8)) |}]
;;

let%expect_test "compute" =
  let inputs =
    let open List.Let_syntax in
    let%bind l1 = Note.Letter_name.all in
    let%bind l2 = Note.Letter_name.all in
    let%bind s1 = Note.Symbol.all in
    let%bind s2 = Note.Symbol.all in
    let%bind od1 = [ 1; 2; 3; 4 ] in
    let%bind od2 = [ 1; 2; 3; 4 ] in
    return
      ( { Note.letter_name = l1; symbol = s1; octave_designation = od1 }
      , { Note.letter_name = l2; symbol = s2; octave_designation = od2 } )
  in
  let table = Hashtbl.create (module Interval) in
  List.iter inputs ~f:(fun (from, to_) ->
    match Interval.compute ~from ~to_ () with
    | None -> ()
    | Some interval ->
      let queue =
        Hashtbl.find_or_add table interval ~default:(fun () -> Queue.create ())
      in
      Queue.enqueue queue (from, to_));
  let results =
    Hashtbl.to_alist table
    |> List.map ~f:(fun (interval, queue) ->
         let number_of_semitons = Interval.number_of_semitons interval in
         number_of_semitons, interval, Queue.to_list queue)
    |> List.sort ~compare:(fun (i, a, _) (j, b, _) ->
         let c = Int.compare i j in
         if c <> 0 then c else Interval.compare a b)
  in
  List.iter results ~f:(fun (number_of_semitons, interval, intervals) ->
    let intervals =
      List.map intervals ~f:(fun (from, to_) ->
        Note.to_string from ^ "-" ^ Note.to_string to_)
    in
    print_s
      [%sexp
        (number_of_semitons : int)
        , (Interval.to_string interval : string)
        , (Interval.name interval : string)
        , (interval : Interval.t)
        , (intervals : string list)]);
  [%expect
    {|
    (0 P1 unison ((number Unison) (quality Perfect) (additional_octaves 0))
     (Abbb1-Abbb1 Abbb2-Abbb2 Abbb3-Abbb3 Abbb4-Abbb4 Abb1-Abb1 Abb2-Abb2
      Abb3-Abb3 Abb4-Abb4 Ab1-Ab1 Ab2-Ab2 Ab3-Ab3 Ab4-Ab4 A1-A1 A2-A2 A3-A3 A4-A4
      A#1-A#1 A#2-A#2 A#3-A#3 A#4-A#4 A##1-A##1 A##2-A##2 A##3-A##3 A##4-A##4
      A###1-A###1 A###2-A###2 A###3-A###3 A###4-A###4 Bbbb1-Bbbb1 Bbbb2-Bbbb2
      Bbbb3-Bbbb3 Bbbb4-Bbbb4 Bbb1-Bbb1 Bbb2-Bbb2 Bbb3-Bbb3 Bbb4-Bbb4 Bb1-Bb1
      Bb2-Bb2 Bb3-Bb3 Bb4-Bb4 B1-B1 B2-B2 B3-B3 B4-B4 B#1-B#1 B#2-B#2 B#3-B#3
      B#4-B#4 B##1-B##1 B##2-B##2 B##3-B##3 B##4-B##4 B###1-B###1 B###2-B###2
      B###3-B###3 B###4-B###4 Cbbb1-Cbbb1 Cbbb2-Cbbb2 Cbbb3-Cbbb3 Cbbb4-Cbbb4
      Cbb1-Cbb1 Cbb2-Cbb2 Cbb3-Cbb3 Cbb4-Cbb4 Cb1-Cb1 Cb2-Cb2 Cb3-Cb3 Cb4-Cb4
      C1-C1 C2-C2 C3-C3 C4-C4 C#1-C#1 C#2-C#2 C#3-C#3 C#4-C#4 C##1-C##1 C##2-C##2
      C##3-C##3 C##4-C##4 C###1-C###1 C###2-C###2 C###3-C###3 C###4-C###4
      Dbbb1-Dbbb1 Dbbb2-Dbbb2 Dbbb3-Dbbb3 Dbbb4-Dbbb4 Dbb1-Dbb1 Dbb2-Dbb2
      Dbb3-Dbb3 Dbb4-Dbb4 Db1-Db1 Db2-Db2 Db3-Db3 Db4-Db4 D1-D1 D2-D2 D3-D3 D4-D4
      D#1-D#1 D#2-D#2 D#3-D#3 D#4-D#4 D##1-D##1 D##2-D##2 D##3-D##3 D##4-D##4
      D###1-D###1 D###2-D###2 D###3-D###3 D###4-D###4 Ebbb1-Ebbb1 Ebbb2-Ebbb2
      Ebbb3-Ebbb3 Ebbb4-Ebbb4 Ebb1-Ebb1 Ebb2-Ebb2 Ebb3-Ebb3 Ebb4-Ebb4 Eb1-Eb1
      Eb2-Eb2 Eb3-Eb3 Eb4-Eb4 E1-E1 E2-E2 E3-E3 E4-E4 E#1-E#1 E#2-E#2 E#3-E#3
      E#4-E#4 E##1-E##1 E##2-E##2 E##3-E##3 E##4-E##4 E###1-E###1 E###2-E###2
      E###3-E###3 E###4-E###4 Fbbb1-Fbbb1 Fbbb2-Fbbb2 Fbbb3-Fbbb3 Fbbb4-Fbbb4
      Fbb1-Fbb1 Fbb2-Fbb2 Fbb3-Fbb3 Fbb4-Fbb4 Fb1-Fb1 Fb2-Fb2 Fb3-Fb3 Fb4-Fb4
      F1-F1 F2-F2 F3-F3 F4-F4 F#1-F#1 F#2-F#2 F#3-F#3 F#4-F#4 F##1-F##1 F##2-F##2
      F##3-F##3 F##4-F##4 F###1-F###1 F###2-F###2 F###3-F###3 F###4-F###4
      Gbbb1-Gbbb1 Gbbb2-Gbbb2 Gbbb3-Gbbb3 Gbbb4-Gbbb4 Gbb1-Gbb1 Gbb2-Gbb2
      Gbb3-Gbb3 Gbb4-Gbb4 Gb1-Gb1 Gb2-Gb2 Gb3-Gb3 Gb4-Gb4 G1-G1 G2-G2 G3-G3 G4-G4
      G#1-G#1 G#2-G#2 G#3-G#3 G#4-G#4 G##1-G##1 G##2-G##2 G##3-G##3 G##4-G##4
      G###1-G###1 G###2-G###2 G###3-G###3 G###4-G###4))
    (0 d2 "diminished second"
     ((number Second) (quality Diminished) (additional_octaves 0))
     (Ab1-Bbbb1 Ab2-Bbbb2 Ab3-Bbbb3 Ab4-Bbbb4 A1-Bbb1 A2-Bbb2 A3-Bbb3 A4-Bbb4
      A#1-Bb1 A#2-Bb2 A#3-Bb3 A#4-Bb4 A##1-B1 A##2-B2 A##3-B3 A##4-B4 A###1-B#1
      A###2-B#2 A###3-B#3 A###4-B#4 Bbb1-Cbbb2 Bbb2-Cbbb3 Bbb3-Cbbb4 Bb1-Cbb2
      Bb2-Cbb3 Bb3-Cbb4 B1-Cb2 B2-Cb3 B3-Cb4 B#1-C2 B#2-C3 B#3-C4 B##1-C#2
      B##2-C#3 B##3-C#4 B###1-C##2 B###2-C##3 B###3-C##4 Cb1-Dbbb1 Cb2-Dbbb2
      Cb3-Dbbb3 Cb4-Dbbb4 C1-Dbb1 C2-Dbb2 C3-Dbb3 C4-Dbb4 C#1-Db1 C#2-Db2 C#3-Db3
      C#4-Db4 C##1-D1 C##2-D2 C##3-D3 C##4-D4 C###1-D#1 C###2-D#2 C###3-D#3
      C###4-D#4 Db1-Ebbb1 Db2-Ebbb2 Db3-Ebbb3 Db4-Ebbb4 D1-Ebb1 D2-Ebb2 D3-Ebb3
      D4-Ebb4 D#1-Eb1 D#2-Eb2 D#3-Eb3 D#4-Eb4 D##1-E1 D##2-E2 D##3-E3 D##4-E4
      D###1-E#1 D###2-E#2 D###3-E#3 D###4-E#4 Ebb1-Fbbb1 Ebb2-Fbbb2 Ebb3-Fbbb3
      Ebb4-Fbbb4 Eb1-Fbb1 Eb2-Fbb2 Eb3-Fbb3 Eb4-Fbb4 E1-Fb1 E2-Fb2 E3-Fb3 E4-Fb4
      E#1-F1 E#2-F2 E#3-F3 E#4-F4 E##1-F#1 E##2-F#2 E##3-F#3 E##4-F#4 E###1-F##1
      E###2-F##2 E###3-F##3 E###4-F##4 Fb1-Gbbb1 Fb2-Gbbb2 Fb3-Gbbb3 Fb4-Gbbb4
      F1-Gbb1 F2-Gbb2 F3-Gbb3 F4-Gbb4 F#1-Gb1 F#2-Gb2 F#3-Gb3 F#4-Gb4 F##1-G1
      F##2-G2 F##3-G3 F##4-G4 F###1-G#1 F###2-G#2 F###3-G#3 F###4-G#4 Gb1-Abbb1
      Gb2-Abbb2 Gb3-Abbb3 Gb4-Abbb4 G1-Abb1 G2-Abb2 G3-Abb3 G4-Abb4 G#1-Ab1
      G#2-Ab2 G#3-Ab3 G#4-Ab4 G##1-A1 G##2-A2 G##3-A3 G##4-A4 G###1-A#1 G###2-A#2
      G###3-A#3 G###4-A#4))
    (1 A1 "augmented unison"
     ((number Unison) (quality Augmented) (additional_octaves 0))
     (Abbb1-Abb1 Abbb2-Abb2 Abbb3-Abb3 Abbb4-Abb4 Abb1-Ab1 Abb2-Ab2 Abb3-Ab3
      Abb4-Ab4 Ab1-A1 Ab2-A2 Ab3-A3 Ab4-A4 A1-A#1 A2-A#2 A3-A#3 A4-A#4 A#1-A##1
      A#2-A##2 A#3-A##3 A#4-A##4 A##1-A###1 A##2-A###2 A##3-A###3 A##4-A###4
      Bbbb1-Bbb1 Bbbb2-Bbb2 Bbbb3-Bbb3 Bbbb4-Bbb4 Bbb1-Bb1 Bbb2-Bb2 Bbb3-Bb3
      Bbb4-Bb4 Bb1-B1 Bb2-B2 Bb3-B3 Bb4-B4 B1-B#1 B2-B#2 B3-B#3 B4-B#4 B#1-B##1
      B#2-B##2 B#3-B##3 B#4-B##4 B##1-B###1 B##2-B###2 B##3-B###3 B##4-B###4
      Cbbb1-Cbb1 Cbbb2-Cbb2 Cbbb3-Cbb3 Cbbb4-Cbb4 Cbb1-Cb1 Cbb2-Cb2 Cbb3-Cb3
      Cbb4-Cb4 Cb1-C1 Cb2-C2 Cb3-C3 Cb4-C4 C1-C#1 C2-C#2 C3-C#3 C4-C#4 C#1-C##1
      C#2-C##2 C#3-C##3 C#4-C##4 C##1-C###1 C##2-C###2 C##3-C###3 C##4-C###4
      Dbbb1-Dbb1 Dbbb2-Dbb2 Dbbb3-Dbb3 Dbbb4-Dbb4 Dbb1-Db1 Dbb2-Db2 Dbb3-Db3
      Dbb4-Db4 Db1-D1 Db2-D2 Db3-D3 Db4-D4 D1-D#1 D2-D#2 D3-D#3 D4-D#4 D#1-D##1
      D#2-D##2 D#3-D##3 D#4-D##4 D##1-D###1 D##2-D###2 D##3-D###3 D##4-D###4
      Ebbb1-Ebb1 Ebbb2-Ebb2 Ebbb3-Ebb3 Ebbb4-Ebb4 Ebb1-Eb1 Ebb2-Eb2 Ebb3-Eb3
      Ebb4-Eb4 Eb1-E1 Eb2-E2 Eb3-E3 Eb4-E4 E1-E#1 E2-E#2 E3-E#3 E4-E#4 E#1-E##1
      E#2-E##2 E#3-E##3 E#4-E##4 E##1-E###1 E##2-E###2 E##3-E###3 E##4-E###4
      Fbbb1-Fbb1 Fbbb2-Fbb2 Fbbb3-Fbb3 Fbbb4-Fbb4 Fbb1-Fb1 Fbb2-Fb2 Fbb3-Fb3
      Fbb4-Fb4 Fb1-F1 Fb2-F2 Fb3-F3 Fb4-F4 F1-F#1 F2-F#2 F3-F#3 F4-F#4 F#1-F##1
      F#2-F##2 F#3-F##3 F#4-F##4 F##1-F###1 F##2-F###2 F##3-F###3 F##4-F###4
      Gbbb1-Gbb1 Gbbb2-Gbb2 Gbbb3-Gbb3 Gbbb4-Gbb4 Gbb1-Gb1 Gbb2-Gb2 Gbb3-Gb3
      Gbb4-Gb4 Gb1-G1 Gb2-G2 Gb3-G3 Gb4-G4 G1-G#1 G2-G#2 G3-G#3 G4-G#4 G#1-G##1
      G#2-G##2 G#3-G##3 G#4-G##4 G##1-G###1 G##2-G###2 G##3-G###3 G##4-G###4))
    (1 m2 "minor second" ((number Second) (quality Minor) (additional_octaves 0))
     (Abb1-Bbbb1 Abb2-Bbbb2 Abb3-Bbbb3 Abb4-Bbbb4 Ab1-Bbb1 Ab2-Bbb2 Ab3-Bbb3
      Ab4-Bbb4 A1-Bb1 A2-Bb2 A3-Bb3 A4-Bb4 A#1-B1 A#2-B2 A#3-B3 A#4-B4 A##1-B#1
      A##2-B#2 A##3-B#3 A##4-B#4 A###1-B##1 A###2-B##2 A###3-B##3 A###4-B##4
      Bbbb1-Cbbb2 Bbbb2-Cbbb3 Bbbb3-Cbbb4 Bbb1-Cbb2 Bbb2-Cbb3 Bbb3-Cbb4 Bb1-Cb2
      Bb2-Cb3 Bb3-Cb4 B1-C2 B2-C3 B3-C4 B#1-C#2 B#2-C#3 B#3-C#4 B##1-C##2
      B##2-C##3 B##3-C##4 B###1-C###2 B###2-C###3 B###3-C###4 Cbb1-Dbbb1
      Cbb2-Dbbb2 Cbb3-Dbbb3 Cbb4-Dbbb4 Cb1-Dbb1 Cb2-Dbb2 Cb3-Dbb3 Cb4-Dbb4 C1-Db1
      C2-Db2 C3-Db3 C4-Db4 C#1-D1 C#2-D2 C#3-D3 C#4-D4 C##1-D#1 C##2-D#2 C##3-D#3
      C##4-D#4 C###1-D##1 C###2-D##2 C###3-D##3 C###4-D##4 Dbb1-Ebbb1 Dbb2-Ebbb2
      Dbb3-Ebbb3 Dbb4-Ebbb4 Db1-Ebb1 Db2-Ebb2 Db3-Ebb3 Db4-Ebb4 D1-Eb1 D2-Eb2
      D3-Eb3 D4-Eb4 D#1-E1 D#2-E2 D#3-E3 D#4-E4 D##1-E#1 D##2-E#2 D##3-E#3
      D##4-E#4 D###1-E##1 D###2-E##2 D###3-E##3 D###4-E##4 Ebbb1-Fbbb1
      Ebbb2-Fbbb2 Ebbb3-Fbbb3 Ebbb4-Fbbb4 Ebb1-Fbb1 Ebb2-Fbb2 Ebb3-Fbb3 Ebb4-Fbb4
      Eb1-Fb1 Eb2-Fb2 Eb3-Fb3 Eb4-Fb4 E1-F1 E2-F2 E3-F3 E4-F4 E#1-F#1 E#2-F#2
      E#3-F#3 E#4-F#4 E##1-F##1 E##2-F##2 E##3-F##3 E##4-F##4 E###1-F###1
      E###2-F###2 E###3-F###3 E###4-F###4 Fbb1-Gbbb1 Fbb2-Gbbb2 Fbb3-Gbbb3
      Fbb4-Gbbb4 Fb1-Gbb1 Fb2-Gbb2 Fb3-Gbb3 Fb4-Gbb4 F1-Gb1 F2-Gb2 F3-Gb3 F4-Gb4
      F#1-G1 F#2-G2 F#3-G3 F#4-G4 F##1-G#1 F##2-G#2 F##3-G#3 F##4-G#4 F###1-G##1
      F###2-G##2 F###3-G##3 F###4-G##4 Gbb1-Abbb1 Gbb2-Abbb2 Gbb3-Abbb3
      Gbb4-Abbb4 Gb1-Abb1 Gb2-Abb2 Gb3-Abb3 Gb4-Abb4 G1-Ab1 G2-Ab2 G3-Ab3 G4-Ab4
      G#1-A1 G#2-A2 G#3-A3 G#4-A4 G##1-A#1 G##2-A#2 G##3-A#3 G##4-A#4 G###1-A##1
      G###2-A##2 G###3-A##3 G###4-A##4))
    (1 dd3 "doubly diminished third"
     ((number Third) (quality Doubly_diminished) (additional_octaves 0))
     (Ab1-Cbbb2 Ab2-Cbbb3 Ab3-Cbbb4 A1-Cbb2 A2-Cbb3 A3-Cbb4 A#1-Cb2 A#2-Cb3
      A#3-Cb4 A##1-C2 A##2-C3 A##3-C4 A###1-C#2 A###2-C#3 A###3-C#4 Bb1-Dbbb2
      Bb2-Dbbb3 Bb3-Dbbb4 B1-Dbb2 B2-Dbb3 B3-Dbb4 B#1-Db2 B#2-Db3 B#3-Db4 B##1-D2
      B##2-D3 B##3-D4 B###1-D#2 B###2-D#3 B###3-D#4 C1-Ebbb1 C2-Ebbb2 C3-Ebbb3
      C4-Ebbb4 C#1-Ebb1 C#2-Ebb2 C#3-Ebb3 C#4-Ebb4 C##1-Eb1 C##2-Eb2 C##3-Eb3
      C##4-Eb4 C###1-E1 C###2-E2 C###3-E3 C###4-E4 Db1-Fbbb1 Db2-Fbbb2 Db3-Fbbb3
      Db4-Fbbb4 D1-Fbb1 D2-Fbb2 D3-Fbb3 D4-Fbb4 D#1-Fb1 D#2-Fb2 D#3-Fb3 D#4-Fb4
      D##1-F1 D##2-F2 D##3-F3 D##4-F4 D###1-F#1 D###2-F#2 D###3-F#3 D###4-F#4
      Eb1-Gbbb1 Eb2-Gbbb2 Eb3-Gbbb3 Eb4-Gbbb4 E1-Gbb1 E2-Gbb2 E3-Gbb3 E4-Gbb4
      E#1-Gb1 E#2-Gb2 E#3-Gb3 E#4-Gb4 E##1-G1 E##2-G2 E##3-G3 E##4-G4 E###1-G#1
      E###2-G#2 E###3-G#3 E###4-G#4 F1-Abbb1 F2-Abbb2 F3-Abbb3 F4-Abbb4 F#1-Abb1
      F#2-Abb2 F#3-Abb3 F#4-Abb4 F##1-Ab1 F##2-Ab2 F##3-Ab3 F##4-Ab4 F###1-A1
      F###2-A2 F###3-A3 F###4-A4 G1-Bbbb1 G2-Bbbb2 G3-Bbbb3 G4-Bbbb4 G#1-Bbb1
      G#2-Bbb2 G#3-Bbb3 G#4-Bbb4 G##1-Bb1 G##2-Bb2 G##3-Bb3 G##4-Bb4 G###1-B1
      G###2-B2 G###3-B3 G###4-B4))
    (2 AA1 "doubly augmented unison"
     ((number Unison) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-Ab1 Abbb2-Ab2 Abbb3-Ab3 Abbb4-Ab4 Abb1-A1 Abb2-A2 Abb3-A3 Abb4-A4
      Ab1-A#1 Ab2-A#2 Ab3-A#3 Ab4-A#4 A1-A##1 A2-A##2 A3-A##3 A4-A##4 A#1-A###1
      A#2-A###2 A#3-A###3 A#4-A###4 Bbbb1-Bb1 Bbbb2-Bb2 Bbbb3-Bb3 Bbbb4-Bb4
      Bbb1-B1 Bbb2-B2 Bbb3-B3 Bbb4-B4 Bb1-B#1 Bb2-B#2 Bb3-B#3 Bb4-B#4 B1-B##1
      B2-B##2 B3-B##3 B4-B##4 B#1-B###1 B#2-B###2 B#3-B###3 B#4-B###4 Cbbb1-Cb1
      Cbbb2-Cb2 Cbbb3-Cb3 Cbbb4-Cb4 Cbb1-C1 Cbb2-C2 Cbb3-C3 Cbb4-C4 Cb1-C#1
      Cb2-C#2 Cb3-C#3 Cb4-C#4 C1-C##1 C2-C##2 C3-C##3 C4-C##4 C#1-C###1 C#2-C###2
      C#3-C###3 C#4-C###4 Dbbb1-Db1 Dbbb2-Db2 Dbbb3-Db3 Dbbb4-Db4 Dbb1-D1 Dbb2-D2
      Dbb3-D3 Dbb4-D4 Db1-D#1 Db2-D#2 Db3-D#3 Db4-D#4 D1-D##1 D2-D##2 D3-D##3
      D4-D##4 D#1-D###1 D#2-D###2 D#3-D###3 D#4-D###4 Ebbb1-Eb1 Ebbb2-Eb2
      Ebbb3-Eb3 Ebbb4-Eb4 Ebb1-E1 Ebb2-E2 Ebb3-E3 Ebb4-E4 Eb1-E#1 Eb2-E#2 Eb3-E#3
      Eb4-E#4 E1-E##1 E2-E##2 E3-E##3 E4-E##4 E#1-E###1 E#2-E###2 E#3-E###3
      E#4-E###4 Fbbb1-Fb1 Fbbb2-Fb2 Fbbb3-Fb3 Fbbb4-Fb4 Fbb1-F1 Fbb2-F2 Fbb3-F3
      Fbb4-F4 Fb1-F#1 Fb2-F#2 Fb3-F#3 Fb4-F#4 F1-F##1 F2-F##2 F3-F##3 F4-F##4
      F#1-F###1 F#2-F###2 F#3-F###3 F#4-F###4 Gbbb1-Gb1 Gbbb2-Gb2 Gbbb3-Gb3
      Gbbb4-Gb4 Gbb1-G1 Gbb2-G2 Gbb3-G3 Gbb4-G4 Gb1-G#1 Gb2-G#2 Gb3-G#3 Gb4-G#4
      G1-G##1 G2-G##2 G3-G##3 G4-G##4 G#1-G###1 G#2-G###2 G#3-G###3 G#4-G###4))
    (2 M2 "major second" ((number Second) (quality Major) (additional_octaves 0))
     (Abbb1-Bbbb1 Abbb2-Bbbb2 Abbb3-Bbbb3 Abbb4-Bbbb4 Abb1-Bbb1 Abb2-Bbb2
      Abb3-Bbb3 Abb4-Bbb4 Ab1-Bb1 Ab2-Bb2 Ab3-Bb3 Ab4-Bb4 A1-B1 A2-B2 A3-B3 A4-B4
      A#1-B#1 A#2-B#2 A#3-B#3 A#4-B#4 A##1-B##1 A##2-B##2 A##3-B##3 A##4-B##4
      A###1-B###1 A###2-B###2 A###3-B###3 A###4-B###4 Bbbb1-Cbb2 Bbbb2-Cbb3
      Bbbb3-Cbb4 Bbb1-Cb2 Bbb2-Cb3 Bbb3-Cb4 Bb1-C2 Bb2-C3 Bb3-C4 B1-C#2 B2-C#3
      B3-C#4 B#1-C##2 B#2-C##3 B#3-C##4 B##1-C###2 B##2-C###3 B##3-C###4
      Cbbb1-Dbbb1 Cbbb2-Dbbb2 Cbbb3-Dbbb3 Cbbb4-Dbbb4 Cbb1-Dbb1 Cbb2-Dbb2
      Cbb3-Dbb3 Cbb4-Dbb4 Cb1-Db1 Cb2-Db2 Cb3-Db3 Cb4-Db4 C1-D1 C2-D2 C3-D3 C4-D4
      C#1-D#1 C#2-D#2 C#3-D#3 C#4-D#4 C##1-D##1 C##2-D##2 C##3-D##3 C##4-D##4
      C###1-D###1 C###2-D###2 C###3-D###3 C###4-D###4 Dbbb1-Ebbb1 Dbbb2-Ebbb2
      Dbbb3-Ebbb3 Dbbb4-Ebbb4 Dbb1-Ebb1 Dbb2-Ebb2 Dbb3-Ebb3 Dbb4-Ebb4 Db1-Eb1
      Db2-Eb2 Db3-Eb3 Db4-Eb4 D1-E1 D2-E2 D3-E3 D4-E4 D#1-E#1 D#2-E#2 D#3-E#3
      D#4-E#4 D##1-E##1 D##2-E##2 D##3-E##3 D##4-E##4 D###1-E###1 D###2-E###2
      D###3-E###3 D###4-E###4 Ebbb1-Fbb1 Ebbb2-Fbb2 Ebbb3-Fbb3 Ebbb4-Fbb4
      Ebb1-Fb1 Ebb2-Fb2 Ebb3-Fb3 Ebb4-Fb4 Eb1-F1 Eb2-F2 Eb3-F3 Eb4-F4 E1-F#1
      E2-F#2 E3-F#3 E4-F#4 E#1-F##1 E#2-F##2 E#3-F##3 E#4-F##4 E##1-F###1
      E##2-F###2 E##3-F###3 E##4-F###4 Fbbb1-Gbbb1 Fbbb2-Gbbb2 Fbbb3-Gbbb3
      Fbbb4-Gbbb4 Fbb1-Gbb1 Fbb2-Gbb2 Fbb3-Gbb3 Fbb4-Gbb4 Fb1-Gb1 Fb2-Gb2 Fb3-Gb3
      Fb4-Gb4 F1-G1 F2-G2 F3-G3 F4-G4 F#1-G#1 F#2-G#2 F#3-G#3 F#4-G#4 F##1-G##1
      F##2-G##2 F##3-G##3 F##4-G##4 F###1-G###1 F###2-G###2 F###3-G###3
      F###4-G###4 Gbbb1-Abbb1 Gbbb2-Abbb2 Gbbb3-Abbb3 Gbbb4-Abbb4 Gbb1-Abb1
      Gbb2-Abb2 Gbb3-Abb3 Gbb4-Abb4 Gb1-Ab1 Gb2-Ab2 Gb3-Ab3 Gb4-Ab4 G1-A1 G2-A2
      G3-A3 G4-A4 G#1-A#1 G#2-A#2 G#3-A#3 G#4-A#4 G##1-A##1 G##2-A##2 G##3-A##3
      G##4-A##4 G###1-A###1 G###2-A###2 G###3-A###3 G###4-A###4))
    (2 d3 "diminished third"
     ((number Third) (quality Diminished) (additional_octaves 0))
     (Abb1-Cbbb2 Abb2-Cbbb3 Abb3-Cbbb4 Ab1-Cbb2 Ab2-Cbb3 Ab3-Cbb4 A1-Cb2 A2-Cb3
      A3-Cb4 A#1-C2 A#2-C3 A#3-C4 A##1-C#2 A##2-C#3 A##3-C#4 A###1-C##2
      A###2-C##3 A###3-C##4 Bbb1-Dbbb2 Bbb2-Dbbb3 Bbb3-Dbbb4 Bb1-Dbb2 Bb2-Dbb3
      Bb3-Dbb4 B1-Db2 B2-Db3 B3-Db4 B#1-D2 B#2-D3 B#3-D4 B##1-D#2 B##2-D#3
      B##3-D#4 B###1-D##2 B###2-D##3 B###3-D##4 Cb1-Ebbb1 Cb2-Ebbb2 Cb3-Ebbb3
      Cb4-Ebbb4 C1-Ebb1 C2-Ebb2 C3-Ebb3 C4-Ebb4 C#1-Eb1 C#2-Eb2 C#3-Eb3 C#4-Eb4
      C##1-E1 C##2-E2 C##3-E3 C##4-E4 C###1-E#1 C###2-E#2 C###3-E#3 C###4-E#4
      Dbb1-Fbbb1 Dbb2-Fbbb2 Dbb3-Fbbb3 Dbb4-Fbbb4 Db1-Fbb1 Db2-Fbb2 Db3-Fbb3
      Db4-Fbb4 D1-Fb1 D2-Fb2 D3-Fb3 D4-Fb4 D#1-F1 D#2-F2 D#3-F3 D#4-F4 D##1-F#1
      D##2-F#2 D##3-F#3 D##4-F#4 D###1-F##1 D###2-F##2 D###3-F##3 D###4-F##4
      Ebb1-Gbbb1 Ebb2-Gbbb2 Ebb3-Gbbb3 Ebb4-Gbbb4 Eb1-Gbb1 Eb2-Gbb2 Eb3-Gbb3
      Eb4-Gbb4 E1-Gb1 E2-Gb2 E3-Gb3 E4-Gb4 E#1-G1 E#2-G2 E#3-G3 E#4-G4 E##1-G#1
      E##2-G#2 E##3-G#3 E##4-G#4 E###1-G##1 E###2-G##2 E###3-G##3 E###4-G##4
      Fb1-Abbb1 Fb2-Abbb2 Fb3-Abbb3 Fb4-Abbb4 F1-Abb1 F2-Abb2 F3-Abb3 F4-Abb4
      F#1-Ab1 F#2-Ab2 F#3-Ab3 F#4-Ab4 F##1-A1 F##2-A2 F##3-A3 F##4-A4 F###1-A#1
      F###2-A#2 F###3-A#3 F###4-A#4 Gb1-Bbbb1 Gb2-Bbbb2 Gb3-Bbbb3 Gb4-Bbbb4
      G1-Bbb1 G2-Bbb2 G3-Bbb3 G4-Bbb4 G#1-Bb1 G#2-Bb2 G#3-Bb3 G#4-Bb4 G##1-B1
      G##2-B2 G##3-B3 G##4-B4 G###1-B#1 G###2-B#2 G###3-B#3 G###4-B#4))
    (3 A2 "augmented second"
     ((number Second) (quality Augmented) (additional_octaves 0))
     (Abbb1-Bbb1 Abbb2-Bbb2 Abbb3-Bbb3 Abbb4-Bbb4 Abb1-Bb1 Abb2-Bb2 Abb3-Bb3
      Abb4-Bb4 Ab1-B1 Ab2-B2 Ab3-B3 Ab4-B4 A1-B#1 A2-B#2 A3-B#3 A4-B#4 A#1-B##1
      A#2-B##2 A#3-B##3 A#4-B##4 A##1-B###1 A##2-B###2 A##3-B###3 A##4-B###4
      Bbbb1-Cb2 Bbbb2-Cb3 Bbbb3-Cb4 Bbb1-C2 Bbb2-C3 Bbb3-C4 Bb1-C#2 Bb2-C#3
      Bb3-C#4 B1-C##2 B2-C##3 B3-C##4 B#1-C###2 B#2-C###3 B#3-C###4 Cbbb1-Dbb1
      Cbbb2-Dbb2 Cbbb3-Dbb3 Cbbb4-Dbb4 Cbb1-Db1 Cbb2-Db2 Cbb3-Db3 Cbb4-Db4 Cb1-D1
      Cb2-D2 Cb3-D3 Cb4-D4 C1-D#1 C2-D#2 C3-D#3 C4-D#4 C#1-D##1 C#2-D##2 C#3-D##3
      C#4-D##4 C##1-D###1 C##2-D###2 C##3-D###3 C##4-D###4 Dbbb1-Ebb1 Dbbb2-Ebb2
      Dbbb3-Ebb3 Dbbb4-Ebb4 Dbb1-Eb1 Dbb2-Eb2 Dbb3-Eb3 Dbb4-Eb4 Db1-E1 Db2-E2
      Db3-E3 Db4-E4 D1-E#1 D2-E#2 D3-E#3 D4-E#4 D#1-E##1 D#2-E##2 D#3-E##3
      D#4-E##4 D##1-E###1 D##2-E###2 D##3-E###3 D##4-E###4 Ebbb1-Fb1 Ebbb2-Fb2
      Ebbb3-Fb3 Ebbb4-Fb4 Ebb1-F1 Ebb2-F2 Ebb3-F3 Ebb4-F4 Eb1-F#1 Eb2-F#2 Eb3-F#3
      Eb4-F#4 E1-F##1 E2-F##2 E3-F##3 E4-F##4 E#1-F###1 E#2-F###2 E#3-F###3
      E#4-F###4 Fbbb1-Gbb1 Fbbb2-Gbb2 Fbbb3-Gbb3 Fbbb4-Gbb4 Fbb1-Gb1 Fbb2-Gb2
      Fbb3-Gb3 Fbb4-Gb4 Fb1-G1 Fb2-G2 Fb3-G3 Fb4-G4 F1-G#1 F2-G#2 F3-G#3 F4-G#4
      F#1-G##1 F#2-G##2 F#3-G##3 F#4-G##4 F##1-G###1 F##2-G###2 F##3-G###3
      F##4-G###4 Gbbb1-Abb1 Gbbb2-Abb2 Gbbb3-Abb3 Gbbb4-Abb4 Gbb1-Ab1 Gbb2-Ab2
      Gbb3-Ab3 Gbb4-Ab4 Gb1-A1 Gb2-A2 Gb3-A3 Gb4-A4 G1-A#1 G2-A#2 G3-A#3 G4-A#4
      G#1-A##1 G#2-A##2 G#3-A##3 G#4-A##4 G##1-A###1 G##2-A###2 G##3-A###3
      G##4-A###4))
    (3 m3 "minor third" ((number Third) (quality Minor) (additional_octaves 0))
     (Abbb1-Cbbb2 Abbb2-Cbbb3 Abbb3-Cbbb4 Abb1-Cbb2 Abb2-Cbb3 Abb3-Cbb4 Ab1-Cb2
      Ab2-Cb3 Ab3-Cb4 A1-C2 A2-C3 A3-C4 A#1-C#2 A#2-C#3 A#3-C#4 A##1-C##2
      A##2-C##3 A##3-C##4 A###1-C###2 A###2-C###3 A###3-C###4 Bbbb1-Dbbb2
      Bbbb2-Dbbb3 Bbbb3-Dbbb4 Bbb1-Dbb2 Bbb2-Dbb3 Bbb3-Dbb4 Bb1-Db2 Bb2-Db3
      Bb3-Db4 B1-D2 B2-D3 B3-D4 B#1-D#2 B#2-D#3 B#3-D#4 B##1-D##2 B##2-D##3
      B##3-D##4 B###1-D###2 B###2-D###3 B###3-D###4 Cbb1-Ebbb1 Cbb2-Ebbb2
      Cbb3-Ebbb3 Cbb4-Ebbb4 Cb1-Ebb1 Cb2-Ebb2 Cb3-Ebb3 Cb4-Ebb4 C1-Eb1 C2-Eb2
      C3-Eb3 C4-Eb4 C#1-E1 C#2-E2 C#3-E3 C#4-E4 C##1-E#1 C##2-E#2 C##3-E#3
      C##4-E#4 C###1-E##1 C###2-E##2 C###3-E##3 C###4-E##4 Dbbb1-Fbbb1
      Dbbb2-Fbbb2 Dbbb3-Fbbb3 Dbbb4-Fbbb4 Dbb1-Fbb1 Dbb2-Fbb2 Dbb3-Fbb3 Dbb4-Fbb4
      Db1-Fb1 Db2-Fb2 Db3-Fb3 Db4-Fb4 D1-F1 D2-F2 D3-F3 D4-F4 D#1-F#1 D#2-F#2
      D#3-F#3 D#4-F#4 D##1-F##1 D##2-F##2 D##3-F##3 D##4-F##4 D###1-F###1
      D###2-F###2 D###3-F###3 D###4-F###4 Ebbb1-Gbbb1 Ebbb2-Gbbb2 Ebbb3-Gbbb3
      Ebbb4-Gbbb4 Ebb1-Gbb1 Ebb2-Gbb2 Ebb3-Gbb3 Ebb4-Gbb4 Eb1-Gb1 Eb2-Gb2 Eb3-Gb3
      Eb4-Gb4 E1-G1 E2-G2 E3-G3 E4-G4 E#1-G#1 E#2-G#2 E#3-G#3 E#4-G#4 E##1-G##1
      E##2-G##2 E##3-G##3 E##4-G##4 E###1-G###1 E###2-G###2 E###3-G###3
      E###4-G###4 Fbb1-Abbb1 Fbb2-Abbb2 Fbb3-Abbb3 Fbb4-Abbb4 Fb1-Abb1 Fb2-Abb2
      Fb3-Abb3 Fb4-Abb4 F1-Ab1 F2-Ab2 F3-Ab3 F4-Ab4 F#1-A1 F#2-A2 F#3-A3 F#4-A4
      F##1-A#1 F##2-A#2 F##3-A#3 F##4-A#4 F###1-A##1 F###2-A##2 F###3-A##3
      F###4-A##4 Gbb1-Bbbb1 Gbb2-Bbbb2 Gbb3-Bbbb3 Gbb4-Bbbb4 Gb1-Bbb1 Gb2-Bbb2
      Gb3-Bbb3 Gb4-Bbb4 G1-Bb1 G2-Bb2 G3-Bb3 G4-Bb4 G#1-B1 G#2-B2 G#3-B3 G#4-B4
      G##1-B#1 G##2-B#2 G##3-B#3 G##4-B#4 G###1-B##1 G###2-B##2 G###3-B##3
      G###4-B##4))
    (3 dd4 "doubly diminished fourth"
     ((number Fourth) (quality Doubly_diminished) (additional_octaves 0))
     (Ab1-Dbbb2 Ab2-Dbbb3 Ab3-Dbbb4 A1-Dbb2 A2-Dbb3 A3-Dbb4 A#1-Db2 A#2-Db3
      A#3-Db4 A##1-D2 A##2-D3 A##3-D4 A###1-D#2 A###2-D#3 A###3-D#4 Bb1-Ebbb2
      Bb2-Ebbb3 Bb3-Ebbb4 B1-Ebb2 B2-Ebb3 B3-Ebb4 B#1-Eb2 B#2-Eb3 B#3-Eb4 B##1-E2
      B##2-E3 B##3-E4 B###1-E#2 B###2-E#3 B###3-E#4 Cb1-Fbbb1 Cb2-Fbbb2 Cb3-Fbbb3
      Cb4-Fbbb4 C1-Fbb1 C2-Fbb2 C3-Fbb3 C4-Fbb4 C#1-Fb1 C#2-Fb2 C#3-Fb3 C#4-Fb4
      C##1-F1 C##2-F2 C##3-F3 C##4-F4 C###1-F#1 C###2-F#2 C###3-F#3 C###4-F#4
      Db1-Gbbb1 Db2-Gbbb2 Db3-Gbbb3 Db4-Gbbb4 D1-Gbb1 D2-Gbb2 D3-Gbb3 D4-Gbb4
      D#1-Gb1 D#2-Gb2 D#3-Gb3 D#4-Gb4 D##1-G1 D##2-G2 D##3-G3 D##4-G4 D###1-G#1
      D###2-G#2 D###3-G#3 D###4-G#4 Eb1-Abbb1 Eb2-Abbb2 Eb3-Abbb3 Eb4-Abbb4
      E1-Abb1 E2-Abb2 E3-Abb3 E4-Abb4 E#1-Ab1 E#2-Ab2 E#3-Ab3 E#4-Ab4 E##1-A1
      E##2-A2 E##3-A3 E##4-A4 E###1-A#1 E###2-A#2 E###3-A#3 E###4-A#4 F1-Bbbb1
      F2-Bbbb2 F3-Bbbb3 F4-Bbbb4 F#1-Bbb1 F#2-Bbb2 F#3-Bbb3 F#4-Bbb4 F##1-Bb1
      F##2-Bb2 F##3-Bb3 F##4-Bb4 F###1-B1 F###2-B2 F###3-B3 F###4-B4 Gb1-Cbbb2
      Gb2-Cbbb3 Gb3-Cbbb4 G1-Cbb2 G2-Cbb3 G3-Cbb4 G#1-Cb2 G#2-Cb3 G#3-Cb4 G##1-C2
      G##2-C3 G##3-C4 G###1-C#2 G###2-C#3 G###3-C#4))
    (4 AA2 "doubly augmented second"
     ((number Second) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-Bb1 Abbb2-Bb2 Abbb3-Bb3 Abbb4-Bb4 Abb1-B1 Abb2-B2 Abb3-B3 Abb4-B4
      Ab1-B#1 Ab2-B#2 Ab3-B#3 Ab4-B#4 A1-B##1 A2-B##2 A3-B##3 A4-B##4 A#1-B###1
      A#2-B###2 A#3-B###3 A#4-B###4 Bbbb1-C2 Bbbb2-C3 Bbbb3-C4 Bbb1-C#2 Bbb2-C#3
      Bbb3-C#4 Bb1-C##2 Bb2-C##3 Bb3-C##4 B1-C###2 B2-C###3 B3-C###4 Cbbb1-Db1
      Cbbb2-Db2 Cbbb3-Db3 Cbbb4-Db4 Cbb1-D1 Cbb2-D2 Cbb3-D3 Cbb4-D4 Cb1-D#1
      Cb2-D#2 Cb3-D#3 Cb4-D#4 C1-D##1 C2-D##2 C3-D##3 C4-D##4 C#1-D###1 C#2-D###2
      C#3-D###3 C#4-D###4 Dbbb1-Eb1 Dbbb2-Eb2 Dbbb3-Eb3 Dbbb4-Eb4 Dbb1-E1 Dbb2-E2
      Dbb3-E3 Dbb4-E4 Db1-E#1 Db2-E#2 Db3-E#3 Db4-E#4 D1-E##1 D2-E##2 D3-E##3
      D4-E##4 D#1-E###1 D#2-E###2 D#3-E###3 D#4-E###4 Ebbb1-F1 Ebbb2-F2 Ebbb3-F3
      Ebbb4-F4 Ebb1-F#1 Ebb2-F#2 Ebb3-F#3 Ebb4-F#4 Eb1-F##1 Eb2-F##2 Eb3-F##3
      Eb4-F##4 E1-F###1 E2-F###2 E3-F###3 E4-F###4 Fbbb1-Gb1 Fbbb2-Gb2 Fbbb3-Gb3
      Fbbb4-Gb4 Fbb1-G1 Fbb2-G2 Fbb3-G3 Fbb4-G4 Fb1-G#1 Fb2-G#2 Fb3-G#3 Fb4-G#4
      F1-G##1 F2-G##2 F3-G##3 F4-G##4 F#1-G###1 F#2-G###2 F#3-G###3 F#4-G###4
      Gbbb1-Ab1 Gbbb2-Ab2 Gbbb3-Ab3 Gbbb4-Ab4 Gbb1-A1 Gbb2-A2 Gbb3-A3 Gbb4-A4
      Gb1-A#1 Gb2-A#2 Gb3-A#3 Gb4-A#4 G1-A##1 G2-A##2 G3-A##3 G4-A##4 G#1-A###1
      G#2-A###2 G#3-A###3 G#4-A###4))
    (4 M3 "major third" ((number Third) (quality Major) (additional_octaves 0))
     (Abbb1-Cbb2 Abbb2-Cbb3 Abbb3-Cbb4 Abb1-Cb2 Abb2-Cb3 Abb3-Cb4 Ab1-C2 Ab2-C3
      Ab3-C4 A1-C#2 A2-C#3 A3-C#4 A#1-C##2 A#2-C##3 A#3-C##4 A##1-C###2
      A##2-C###3 A##3-C###4 Bbbb1-Dbb2 Bbbb2-Dbb3 Bbbb3-Dbb4 Bbb1-Db2 Bbb2-Db3
      Bbb3-Db4 Bb1-D2 Bb2-D3 Bb3-D4 B1-D#2 B2-D#3 B3-D#4 B#1-D##2 B#2-D##3
      B#3-D##4 B##1-D###2 B##2-D###3 B##3-D###4 Cbbb1-Ebbb1 Cbbb2-Ebbb2
      Cbbb3-Ebbb3 Cbbb4-Ebbb4 Cbb1-Ebb1 Cbb2-Ebb2 Cbb3-Ebb3 Cbb4-Ebb4 Cb1-Eb1
      Cb2-Eb2 Cb3-Eb3 Cb4-Eb4 C1-E1 C2-E2 C3-E3 C4-E4 C#1-E#1 C#2-E#2 C#3-E#3
      C#4-E#4 C##1-E##1 C##2-E##2 C##3-E##3 C##4-E##4 C###1-E###1 C###2-E###2
      C###3-E###3 C###4-E###4 Dbbb1-Fbb1 Dbbb2-Fbb2 Dbbb3-Fbb3 Dbbb4-Fbb4
      Dbb1-Fb1 Dbb2-Fb2 Dbb3-Fb3 Dbb4-Fb4 Db1-F1 Db2-F2 Db3-F3 Db4-F4 D1-F#1
      D2-F#2 D3-F#3 D4-F#4 D#1-F##1 D#2-F##2 D#3-F##3 D#4-F##4 D##1-F###1
      D##2-F###2 D##3-F###3 D##4-F###4 Ebbb1-Gbb1 Ebbb2-Gbb2 Ebbb3-Gbb3
      Ebbb4-Gbb4 Ebb1-Gb1 Ebb2-Gb2 Ebb3-Gb3 Ebb4-Gb4 Eb1-G1 Eb2-G2 Eb3-G3 Eb4-G4
      E1-G#1 E2-G#2 E3-G#3 E4-G#4 E#1-G##1 E#2-G##2 E#3-G##3 E#4-G##4 E##1-G###1
      E##2-G###2 E##3-G###3 E##4-G###4 Fbbb1-Abbb1 Fbbb2-Abbb2 Fbbb3-Abbb3
      Fbbb4-Abbb4 Fbb1-Abb1 Fbb2-Abb2 Fbb3-Abb3 Fbb4-Abb4 Fb1-Ab1 Fb2-Ab2 Fb3-Ab3
      Fb4-Ab4 F1-A1 F2-A2 F3-A3 F4-A4 F#1-A#1 F#2-A#2 F#3-A#3 F#4-A#4 F##1-A##1
      F##2-A##2 F##3-A##3 F##4-A##4 F###1-A###1 F###2-A###2 F###3-A###3
      F###4-A###4 Gbbb1-Bbbb1 Gbbb2-Bbbb2 Gbbb3-Bbbb3 Gbbb4-Bbbb4 Gbb1-Bbb1
      Gbb2-Bbb2 Gbb3-Bbb3 Gbb4-Bbb4 Gb1-Bb1 Gb2-Bb2 Gb3-Bb3 Gb4-Bb4 G1-B1 G2-B2
      G3-B3 G4-B4 G#1-B#1 G#2-B#2 G#3-B#3 G#4-B#4 G##1-B##1 G##2-B##2 G##3-B##3
      G##4-B##4 G###1-B###1 G###2-B###2 G###3-B###3 G###4-B###4))
    (4 d4 "diminished fourth"
     ((number Fourth) (quality Diminished) (additional_octaves 0))
     (Abb1-Dbbb2 Abb2-Dbbb3 Abb3-Dbbb4 Ab1-Dbb2 Ab2-Dbb3 Ab3-Dbb4 A1-Db2 A2-Db3
      A3-Db4 A#1-D2 A#2-D3 A#3-D4 A##1-D#2 A##2-D#3 A##3-D#4 A###1-D##2
      A###2-D##3 A###3-D##4 Bbb1-Ebbb2 Bbb2-Ebbb3 Bbb3-Ebbb4 Bb1-Ebb2 Bb2-Ebb3
      Bb3-Ebb4 B1-Eb2 B2-Eb3 B3-Eb4 B#1-E2 B#2-E3 B#3-E4 B##1-E#2 B##2-E#3
      B##3-E#4 B###1-E##2 B###2-E##3 B###3-E##4 Cbb1-Fbbb1 Cbb2-Fbbb2 Cbb3-Fbbb3
      Cbb4-Fbbb4 Cb1-Fbb1 Cb2-Fbb2 Cb3-Fbb3 Cb4-Fbb4 C1-Fb1 C2-Fb2 C3-Fb3 C4-Fb4
      C#1-F1 C#2-F2 C#3-F3 C#4-F4 C##1-F#1 C##2-F#2 C##3-F#3 C##4-F#4 C###1-F##1
      C###2-F##2 C###3-F##3 C###4-F##4 Dbb1-Gbbb1 Dbb2-Gbbb2 Dbb3-Gbbb3
      Dbb4-Gbbb4 Db1-Gbb1 Db2-Gbb2 Db3-Gbb3 Db4-Gbb4 D1-Gb1 D2-Gb2 D3-Gb3 D4-Gb4
      D#1-G1 D#2-G2 D#3-G3 D#4-G4 D##1-G#1 D##2-G#2 D##3-G#3 D##4-G#4 D###1-G##1
      D###2-G##2 D###3-G##3 D###4-G##4 Ebb1-Abbb1 Ebb2-Abbb2 Ebb3-Abbb3
      Ebb4-Abbb4 Eb1-Abb1 Eb2-Abb2 Eb3-Abb3 Eb4-Abb4 E1-Ab1 E2-Ab2 E3-Ab3 E4-Ab4
      E#1-A1 E#2-A2 E#3-A3 E#4-A4 E##1-A#1 E##2-A#2 E##3-A#3 E##4-A#4 E###1-A##1
      E###2-A##2 E###3-A##3 E###4-A##4 Fb1-Bbbb1 Fb2-Bbbb2 Fb3-Bbbb3 Fb4-Bbbb4
      F1-Bbb1 F2-Bbb2 F3-Bbb3 F4-Bbb4 F#1-Bb1 F#2-Bb2 F#3-Bb3 F#4-Bb4 F##1-B1
      F##2-B2 F##3-B3 F##4-B4 F###1-B#1 F###2-B#2 F###3-B#3 F###4-B#4 Gbb1-Cbbb2
      Gbb2-Cbbb3 Gbb3-Cbbb4 Gb1-Cbb2 Gb2-Cbb3 Gb3-Cbb4 G1-Cb2 G2-Cb3 G3-Cb4
      G#1-C2 G#2-C3 G#3-C4 G##1-C#2 G##2-C#3 G##3-C#4 G###1-C##2 G###2-C##3
      G###3-C##4))
    (5 A3 "augmented third"
     ((number Third) (quality Augmented) (additional_octaves 0))
     (Abbb1-Cb2 Abbb2-Cb3 Abbb3-Cb4 Abb1-C2 Abb2-C3 Abb3-C4 Ab1-C#2 Ab2-C#3
      Ab3-C#4 A1-C##2 A2-C##3 A3-C##4 A#1-C###2 A#2-C###3 A#3-C###4 Bbbb1-Db2
      Bbbb2-Db3 Bbbb3-Db4 Bbb1-D2 Bbb2-D3 Bbb3-D4 Bb1-D#2 Bb2-D#3 Bb3-D#4 B1-D##2
      B2-D##3 B3-D##4 B#1-D###2 B#2-D###3 B#3-D###4 Cbbb1-Ebb1 Cbbb2-Ebb2
      Cbbb3-Ebb3 Cbbb4-Ebb4 Cbb1-Eb1 Cbb2-Eb2 Cbb3-Eb3 Cbb4-Eb4 Cb1-E1 Cb2-E2
      Cb3-E3 Cb4-E4 C1-E#1 C2-E#2 C3-E#3 C4-E#4 C#1-E##1 C#2-E##2 C#3-E##3
      C#4-E##4 C##1-E###1 C##2-E###2 C##3-E###3 C##4-E###4 Dbbb1-Fb1 Dbbb2-Fb2
      Dbbb3-Fb3 Dbbb4-Fb4 Dbb1-F1 Dbb2-F2 Dbb3-F3 Dbb4-F4 Db1-F#1 Db2-F#2 Db3-F#3
      Db4-F#4 D1-F##1 D2-F##2 D3-F##3 D4-F##4 D#1-F###1 D#2-F###2 D#3-F###3
      D#4-F###4 Ebbb1-Gb1 Ebbb2-Gb2 Ebbb3-Gb3 Ebbb4-Gb4 Ebb1-G1 Ebb2-G2 Ebb3-G3
      Ebb4-G4 Eb1-G#1 Eb2-G#2 Eb3-G#3 Eb4-G#4 E1-G##1 E2-G##2 E3-G##3 E4-G##4
      E#1-G###1 E#2-G###2 E#3-G###3 E#4-G###4 Fbbb1-Abb1 Fbbb2-Abb2 Fbbb3-Abb3
      Fbbb4-Abb4 Fbb1-Ab1 Fbb2-Ab2 Fbb3-Ab3 Fbb4-Ab4 Fb1-A1 Fb2-A2 Fb3-A3 Fb4-A4
      F1-A#1 F2-A#2 F3-A#3 F4-A#4 F#1-A##1 F#2-A##2 F#3-A##3 F#4-A##4 F##1-A###1
      F##2-A###2 F##3-A###3 F##4-A###4 Gbbb1-Bbb1 Gbbb2-Bbb2 Gbbb3-Bbb3
      Gbbb4-Bbb4 Gbb1-Bb1 Gbb2-Bb2 Gbb3-Bb3 Gbb4-Bb4 Gb1-B1 Gb2-B2 Gb3-B3 Gb4-B4
      G1-B#1 G2-B#2 G3-B#3 G4-B#4 G#1-B##1 G#2-B##2 G#3-B##3 G#4-B##4 G##1-B###1
      G##2-B###2 G##3-B###3 G##4-B###4))
    (5 P4 fourth ((number Fourth) (quality Perfect) (additional_octaves 0))
     (Abbb1-Dbbb2 Abbb2-Dbbb3 Abbb3-Dbbb4 Abb1-Dbb2 Abb2-Dbb3 Abb3-Dbb4 Ab1-Db2
      Ab2-Db3 Ab3-Db4 A1-D2 A2-D3 A3-D4 A#1-D#2 A#2-D#3 A#3-D#4 A##1-D##2
      A##2-D##3 A##3-D##4 A###1-D###2 A###2-D###3 A###3-D###4 Bbbb1-Ebbb2
      Bbbb2-Ebbb3 Bbbb3-Ebbb4 Bbb1-Ebb2 Bbb2-Ebb3 Bbb3-Ebb4 Bb1-Eb2 Bb2-Eb3
      Bb3-Eb4 B1-E2 B2-E3 B3-E4 B#1-E#2 B#2-E#3 B#3-E#4 B##1-E##2 B##2-E##3
      B##3-E##4 B###1-E###2 B###2-E###3 B###3-E###4 Cbbb1-Fbbb1 Cbbb2-Fbbb2
      Cbbb3-Fbbb3 Cbbb4-Fbbb4 Cbb1-Fbb1 Cbb2-Fbb2 Cbb3-Fbb3 Cbb4-Fbb4 Cb1-Fb1
      Cb2-Fb2 Cb3-Fb3 Cb4-Fb4 C1-F1 C2-F2 C3-F3 C4-F4 C#1-F#1 C#2-F#2 C#3-F#3
      C#4-F#4 C##1-F##1 C##2-F##2 C##3-F##3 C##4-F##4 C###1-F###1 C###2-F###2
      C###3-F###3 C###4-F###4 Dbbb1-Gbbb1 Dbbb2-Gbbb2 Dbbb3-Gbbb3 Dbbb4-Gbbb4
      Dbb1-Gbb1 Dbb2-Gbb2 Dbb3-Gbb3 Dbb4-Gbb4 Db1-Gb1 Db2-Gb2 Db3-Gb3 Db4-Gb4
      D1-G1 D2-G2 D3-G3 D4-G4 D#1-G#1 D#2-G#2 D#3-G#3 D#4-G#4 D##1-G##1 D##2-G##2
      D##3-G##3 D##4-G##4 D###1-G###1 D###2-G###2 D###3-G###3 D###4-G###4
      Ebbb1-Abbb1 Ebbb2-Abbb2 Ebbb3-Abbb3 Ebbb4-Abbb4 Ebb1-Abb1 Ebb2-Abb2
      Ebb3-Abb3 Ebb4-Abb4 Eb1-Ab1 Eb2-Ab2 Eb3-Ab3 Eb4-Ab4 E1-A1 E2-A2 E3-A3 E4-A4
      E#1-A#1 E#2-A#2 E#3-A#3 E#4-A#4 E##1-A##1 E##2-A##2 E##3-A##3 E##4-A##4
      E###1-A###1 E###2-A###2 E###3-A###3 E###4-A###4 Fbb1-Bbbb1 Fbb2-Bbbb2
      Fbb3-Bbbb3 Fbb4-Bbbb4 Fb1-Bbb1 Fb2-Bbb2 Fb3-Bbb3 Fb4-Bbb4 F1-Bb1 F2-Bb2
      F3-Bb3 F4-Bb4 F#1-B1 F#2-B2 F#3-B3 F#4-B4 F##1-B#1 F##2-B#2 F##3-B#3
      F##4-B#4 F###1-B##1 F###2-B##2 F###3-B##3 F###4-B##4 Gbbb1-Cbbb2
      Gbbb2-Cbbb3 Gbbb3-Cbbb4 Gbb1-Cbb2 Gbb2-Cbb3 Gbb3-Cbb4 Gb1-Cb2 Gb2-Cb3
      Gb3-Cb4 G1-C2 G2-C3 G3-C4 G#1-C#2 G#2-C#3 G#3-C#4 G##1-C##2 G##2-C##3
      G##3-C##4 G###1-C###2 G###2-C###3 G###3-C###4))
    (5 dd5 "doubly diminished fifth"
     ((number Fifth) (quality Doubly_diminished) (additional_octaves 0))
     (Ab1-Ebbb2 Ab2-Ebbb3 Ab3-Ebbb4 A1-Ebb2 A2-Ebb3 A3-Ebb4 A#1-Eb2 A#2-Eb3
      A#3-Eb4 A##1-E2 A##2-E3 A##3-E4 A###1-E#2 A###2-E#3 A###3-E#4 Bbb1-Fbbb2
      Bbb2-Fbbb3 Bbb3-Fbbb4 Bb1-Fbb2 Bb2-Fbb3 Bb3-Fbb4 B1-Fb2 B2-Fb3 B3-Fb4
      B#1-F2 B#2-F3 B#3-F4 B##1-F#2 B##2-F#3 B##3-F#4 B###1-F##2 B###2-F##3
      B###3-F##4 Cb1-Gbbb1 Cb2-Gbbb2 Cb3-Gbbb3 Cb4-Gbbb4 C1-Gbb1 C2-Gbb2 C3-Gbb3
      C4-Gbb4 C#1-Gb1 C#2-Gb2 C#3-Gb3 C#4-Gb4 C##1-G1 C##2-G2 C##3-G3 C##4-G4
      C###1-G#1 C###2-G#2 C###3-G#3 C###4-G#4 Db1-Abbb1 Db2-Abbb2 Db3-Abbb3
      Db4-Abbb4 D1-Abb1 D2-Abb2 D3-Abb3 D4-Abb4 D#1-Ab1 D#2-Ab2 D#3-Ab3 D#4-Ab4
      D##1-A1 D##2-A2 D##3-A3 D##4-A4 D###1-A#1 D###2-A#2 D###3-A#3 D###4-A#4
      Eb1-Bbbb1 Eb2-Bbbb2 Eb3-Bbbb3 Eb4-Bbbb4 E1-Bbb1 E2-Bbb2 E3-Bbb3 E4-Bbb4
      E#1-Bb1 E#2-Bb2 E#3-Bb3 E#4-Bb4 E##1-B1 E##2-B2 E##3-B3 E##4-B4 E###1-B#1
      E###2-B#2 E###3-B#3 E###4-B#4 Fb1-Cbbb2 Fb2-Cbbb3 Fb3-Cbbb4 F1-Cbb2 F2-Cbb3
      F3-Cbb4 F#1-Cb2 F#2-Cb3 F#3-Cb4 F##1-C2 F##2-C3 F##3-C4 F###1-C#2 F###2-C#3
      F###3-C#4 Gb1-Dbbb2 Gb2-Dbbb3 Gb3-Dbbb4 G1-Dbb2 G2-Dbb3 G3-Dbb4 G#1-Db2
      G#2-Db3 G#3-Db4 G##1-D2 G##2-D3 G##3-D4 G###1-D#2 G###2-D#3 G###3-D#4))
    (6 AA3 "doubly augmented third"
     ((number Third) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-C2 Abbb2-C3 Abbb3-C4 Abb1-C#2 Abb2-C#3 Abb3-C#4 Ab1-C##2 Ab2-C##3
      Ab3-C##4 A1-C###2 A2-C###3 A3-C###4 Bbbb1-D2 Bbbb2-D3 Bbbb3-D4 Bbb1-D#2
      Bbb2-D#3 Bbb3-D#4 Bb1-D##2 Bb2-D##3 Bb3-D##4 B1-D###2 B2-D###3 B3-D###4
      Cbbb1-Eb1 Cbbb2-Eb2 Cbbb3-Eb3 Cbbb4-Eb4 Cbb1-E1 Cbb2-E2 Cbb3-E3 Cbb4-E4
      Cb1-E#1 Cb2-E#2 Cb3-E#3 Cb4-E#4 C1-E##1 C2-E##2 C3-E##3 C4-E##4 C#1-E###1
      C#2-E###2 C#3-E###3 C#4-E###4 Dbbb1-F1 Dbbb2-F2 Dbbb3-F3 Dbbb4-F4 Dbb1-F#1
      Dbb2-F#2 Dbb3-F#3 Dbb4-F#4 Db1-F##1 Db2-F##2 Db3-F##3 Db4-F##4 D1-F###1
      D2-F###2 D3-F###3 D4-F###4 Ebbb1-G1 Ebbb2-G2 Ebbb3-G3 Ebbb4-G4 Ebb1-G#1
      Ebb2-G#2 Ebb3-G#3 Ebb4-G#4 Eb1-G##1 Eb2-G##2 Eb3-G##3 Eb4-G##4 E1-G###1
      E2-G###2 E3-G###3 E4-G###4 Fbbb1-Ab1 Fbbb2-Ab2 Fbbb3-Ab3 Fbbb4-Ab4 Fbb1-A1
      Fbb2-A2 Fbb3-A3 Fbb4-A4 Fb1-A#1 Fb2-A#2 Fb3-A#3 Fb4-A#4 F1-A##1 F2-A##2
      F3-A##3 F4-A##4 F#1-A###1 F#2-A###2 F#3-A###3 F#4-A###4 Gbbb1-Bb1 Gbbb2-Bb2
      Gbbb3-Bb3 Gbbb4-Bb4 Gbb1-B1 Gbb2-B2 Gbb3-B3 Gbb4-B4 Gb1-B#1 Gb2-B#2 Gb3-B#3
      Gb4-B#4 G1-B##1 G2-B##2 G3-B##3 G4-B##4 G#1-B###1 G#2-B###2 G#3-B###3
      G#4-B###4))
    (6 A4 "augmented fourth"
     ((number Fourth) (quality Augmented) (additional_octaves 0))
     (Abbb1-Dbb2 Abbb2-Dbb3 Abbb3-Dbb4 Abb1-Db2 Abb2-Db3 Abb3-Db4 Ab1-D2 Ab2-D3
      Ab3-D4 A1-D#2 A2-D#3 A3-D#4 A#1-D##2 A#2-D##3 A#3-D##4 A##1-D###2
      A##2-D###3 A##3-D###4 Bbbb1-Ebb2 Bbbb2-Ebb3 Bbbb3-Ebb4 Bbb1-Eb2 Bbb2-Eb3
      Bbb3-Eb4 Bb1-E2 Bb2-E3 Bb3-E4 B1-E#2 B2-E#3 B3-E#4 B#1-E##2 B#2-E##3
      B#3-E##4 B##1-E###2 B##2-E###3 B##3-E###4 Cbbb1-Fbb1 Cbbb2-Fbb2 Cbbb3-Fbb3
      Cbbb4-Fbb4 Cbb1-Fb1 Cbb2-Fb2 Cbb3-Fb3 Cbb4-Fb4 Cb1-F1 Cb2-F2 Cb3-F3 Cb4-F4
      C1-F#1 C2-F#2 C3-F#3 C4-F#4 C#1-F##1 C#2-F##2 C#3-F##3 C#4-F##4 C##1-F###1
      C##2-F###2 C##3-F###3 C##4-F###4 Dbbb1-Gbb1 Dbbb2-Gbb2 Dbbb3-Gbb3
      Dbbb4-Gbb4 Dbb1-Gb1 Dbb2-Gb2 Dbb3-Gb3 Dbb4-Gb4 Db1-G1 Db2-G2 Db3-G3 Db4-G4
      D1-G#1 D2-G#2 D3-G#3 D4-G#4 D#1-G##1 D#2-G##2 D#3-G##3 D#4-G##4 D##1-G###1
      D##2-G###2 D##3-G###3 D##4-G###4 Ebbb1-Abb1 Ebbb2-Abb2 Ebbb3-Abb3
      Ebbb4-Abb4 Ebb1-Ab1 Ebb2-Ab2 Ebb3-Ab3 Ebb4-Ab4 Eb1-A1 Eb2-A2 Eb3-A3 Eb4-A4
      E1-A#1 E2-A#2 E3-A#3 E4-A#4 E#1-A##1 E#2-A##2 E#3-A##3 E#4-A##4 E##1-A###1
      E##2-A###2 E##3-A###3 E##4-A###4 Fbbb1-Bbbb1 Fbbb2-Bbbb2 Fbbb3-Bbbb3
      Fbbb4-Bbbb4 Fbb1-Bbb1 Fbb2-Bbb2 Fbb3-Bbb3 Fbb4-Bbb4 Fb1-Bb1 Fb2-Bb2 Fb3-Bb3
      Fb4-Bb4 F1-B1 F2-B2 F3-B3 F4-B4 F#1-B#1 F#2-B#2 F#3-B#3 F#4-B#4 F##1-B##1
      F##2-B##2 F##3-B##3 F##4-B##4 F###1-B###1 F###2-B###2 F###3-B###3
      F###4-B###4 Gbbb1-Cbb2 Gbbb2-Cbb3 Gbbb3-Cbb4 Gbb1-Cb2 Gbb2-Cb3 Gbb3-Cb4
      Gb1-C2 Gb2-C3 Gb3-C4 G1-C#2 G2-C#3 G3-C#4 G#1-C##2 G#2-C##3 G#3-C##4
      G##1-C###2 G##2-C###3 G##3-C###4))
    (6 d5 "diminished fifth"
     ((number Fifth) (quality Diminished) (additional_octaves 0))
     (Abb1-Ebbb2 Abb2-Ebbb3 Abb3-Ebbb4 Ab1-Ebb2 Ab2-Ebb3 Ab3-Ebb4 A1-Eb2 A2-Eb3
      A3-Eb4 A#1-E2 A#2-E3 A#3-E4 A##1-E#2 A##2-E#3 A##3-E#4 A###1-E##2
      A###2-E##3 A###3-E##4 Bbbb1-Fbbb2 Bbbb2-Fbbb3 Bbbb3-Fbbb4 Bbb1-Fbb2
      Bbb2-Fbb3 Bbb3-Fbb4 Bb1-Fb2 Bb2-Fb3 Bb3-Fb4 B1-F2 B2-F3 B3-F4 B#1-F#2
      B#2-F#3 B#3-F#4 B##1-F##2 B##2-F##3 B##3-F##4 B###1-F###2 B###2-F###3
      B###3-F###4 Cbb1-Gbbb1 Cbb2-Gbbb2 Cbb3-Gbbb3 Cbb4-Gbbb4 Cb1-Gbb1 Cb2-Gbb2
      Cb3-Gbb3 Cb4-Gbb4 C1-Gb1 C2-Gb2 C3-Gb3 C4-Gb4 C#1-G1 C#2-G2 C#3-G3 C#4-G4
      C##1-G#1 C##2-G#2 C##3-G#3 C##4-G#4 C###1-G##1 C###2-G##2 C###3-G##3
      C###4-G##4 Dbb1-Abbb1 Dbb2-Abbb2 Dbb3-Abbb3 Dbb4-Abbb4 Db1-Abb1 Db2-Abb2
      Db3-Abb3 Db4-Abb4 D1-Ab1 D2-Ab2 D3-Ab3 D4-Ab4 D#1-A1 D#2-A2 D#3-A3 D#4-A4
      D##1-A#1 D##2-A#2 D##3-A#3 D##4-A#4 D###1-A##1 D###2-A##2 D###3-A##3
      D###4-A##4 Ebb1-Bbbb1 Ebb2-Bbbb2 Ebb3-Bbbb3 Ebb4-Bbbb4 Eb1-Bbb1 Eb2-Bbb2
      Eb3-Bbb3 Eb4-Bbb4 E1-Bb1 E2-Bb2 E3-Bb3 E4-Bb4 E#1-B1 E#2-B2 E#3-B3 E#4-B4
      E##1-B#1 E##2-B#2 E##3-B#3 E##4-B#4 E###1-B##1 E###2-B##2 E###3-B##3
      E###4-B##4 Fbb1-Cbbb2 Fbb2-Cbbb3 Fbb3-Cbbb4 Fb1-Cbb2 Fb2-Cbb3 Fb3-Cbb4
      F1-Cb2 F2-Cb3 F3-Cb4 F#1-C2 F#2-C3 F#3-C4 F##1-C#2 F##2-C#3 F##3-C#4
      F###1-C##2 F###2-C##3 F###3-C##4 Gbb1-Dbbb2 Gbb2-Dbbb3 Gbb3-Dbbb4 Gb1-Dbb2
      Gb2-Dbb3 Gb3-Dbb4 G1-Db2 G2-Db3 G3-Db4 G#1-D2 G#2-D3 G#3-D4 G##1-D#2
      G##2-D#3 G##3-D#4 G###1-D##2 G###2-D##3 G###3-D##4))
    (6 dd6 "doubly diminished sixth"
     ((number Sixth) (quality Doubly_diminished) (additional_octaves 0))
     (Ab1-Fbbb2 Ab2-Fbbb3 Ab3-Fbbb4 A1-Fbb2 A2-Fbb3 A3-Fbb4 A#1-Fb2 A#2-Fb3
      A#3-Fb4 A##1-F2 A##2-F3 A##3-F4 A###1-F#2 A###2-F#3 A###3-F#4 Bb1-Gbbb2
      Bb2-Gbbb3 Bb3-Gbbb4 B1-Gbb2 B2-Gbb3 B3-Gbb4 B#1-Gb2 B#2-Gb3 B#3-Gb4 B##1-G2
      B##2-G3 B##3-G4 B###1-G#2 B###2-G#3 B###3-G#4 C1-Abbb1 C2-Abbb2 C3-Abbb3
      C4-Abbb4 C#1-Abb1 C#2-Abb2 C#3-Abb3 C#4-Abb4 C##1-Ab1 C##2-Ab2 C##3-Ab3
      C##4-Ab4 C###1-A1 C###2-A2 C###3-A3 C###4-A4 D1-Bbbb1 D2-Bbbb2 D3-Bbbb3
      D4-Bbbb4 D#1-Bbb1 D#2-Bbb2 D#3-Bbb3 D#4-Bbb4 D##1-Bb1 D##2-Bb2 D##3-Bb3
      D##4-Bb4 D###1-B1 D###2-B2 D###3-B3 D###4-B4 Eb1-Cbbb2 Eb2-Cbbb3 Eb3-Cbbb4
      E1-Cbb2 E2-Cbb3 E3-Cbb4 E#1-Cb2 E#2-Cb3 E#3-Cb4 E##1-C2 E##2-C3 E##3-C4
      E###1-C#2 E###2-C#3 E###3-C#4 F1-Dbbb2 F2-Dbbb3 F3-Dbbb4 F#1-Dbb2 F#2-Dbb3
      F#3-Dbb4 F##1-Db2 F##2-Db3 F##3-Db4 F###1-D2 F###2-D3 F###3-D4 G1-Ebbb2
      G2-Ebbb3 G3-Ebbb4 G#1-Ebb2 G#2-Ebb3 G#3-Ebb4 G##1-Eb2 G##2-Eb3 G##3-Eb4
      G###1-E2 G###2-E3 G###3-E4))
    (7 AA4 "doubly augmented fourth"
     ((number Fourth) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-Db2 Abbb2-Db3 Abbb3-Db4 Abb1-D2 Abb2-D3 Abb3-D4 Ab1-D#2 Ab2-D#3
      Ab3-D#4 A1-D##2 A2-D##3 A3-D##4 A#1-D###2 A#2-D###3 A#3-D###4 Bbbb1-Eb2
      Bbbb2-Eb3 Bbbb3-Eb4 Bbb1-E2 Bbb2-E3 Bbb3-E4 Bb1-E#2 Bb2-E#3 Bb3-E#4 B1-E##2
      B2-E##3 B3-E##4 B#1-E###2 B#2-E###3 B#3-E###4 Cbbb1-Fb1 Cbbb2-Fb2 Cbbb3-Fb3
      Cbbb4-Fb4 Cbb1-F1 Cbb2-F2 Cbb3-F3 Cbb4-F4 Cb1-F#1 Cb2-F#2 Cb3-F#3 Cb4-F#4
      C1-F##1 C2-F##2 C3-F##3 C4-F##4 C#1-F###1 C#2-F###2 C#3-F###3 C#4-F###4
      Dbbb1-Gb1 Dbbb2-Gb2 Dbbb3-Gb3 Dbbb4-Gb4 Dbb1-G1 Dbb2-G2 Dbb3-G3 Dbb4-G4
      Db1-G#1 Db2-G#2 Db3-G#3 Db4-G#4 D1-G##1 D2-G##2 D3-G##3 D4-G##4 D#1-G###1
      D#2-G###2 D#3-G###3 D#4-G###4 Ebbb1-Ab1 Ebbb2-Ab2 Ebbb3-Ab3 Ebbb4-Ab4
      Ebb1-A1 Ebb2-A2 Ebb3-A3 Ebb4-A4 Eb1-A#1 Eb2-A#2 Eb3-A#3 Eb4-A#4 E1-A##1
      E2-A##2 E3-A##3 E4-A##4 E#1-A###1 E#2-A###2 E#3-A###3 E#4-A###4 Fbbb1-Bbb1
      Fbbb2-Bbb2 Fbbb3-Bbb3 Fbbb4-Bbb4 Fbb1-Bb1 Fbb2-Bb2 Fbb3-Bb3 Fbb4-Bb4 Fb1-B1
      Fb2-B2 Fb3-B3 Fb4-B4 F1-B#1 F2-B#2 F3-B#3 F4-B#4 F#1-B##1 F#2-B##2 F#3-B##3
      F#4-B##4 F##1-B###1 F##2-B###2 F##3-B###3 F##4-B###4 Gbbb1-Cb2 Gbbb2-Cb3
      Gbbb3-Cb4 Gbb1-C2 Gbb2-C3 Gbb3-C4 Gb1-C#2 Gb2-C#3 Gb3-C#4 G1-C##2 G2-C##3
      G3-C##4 G#1-C###2 G#2-C###3 G#3-C###4))
    (7 P5 fifth ((number Fifth) (quality Perfect) (additional_octaves 0))
     (Abbb1-Ebbb2 Abbb2-Ebbb3 Abbb3-Ebbb4 Abb1-Ebb2 Abb2-Ebb3 Abb3-Ebb4 Ab1-Eb2
      Ab2-Eb3 Ab3-Eb4 A1-E2 A2-E3 A3-E4 A#1-E#2 A#2-E#3 A#3-E#4 A##1-E##2
      A##2-E##3 A##3-E##4 A###1-E###2 A###2-E###3 A###3-E###4 Bbbb1-Fbb2
      Bbbb2-Fbb3 Bbbb3-Fbb4 Bbb1-Fb2 Bbb2-Fb3 Bbb3-Fb4 Bb1-F2 Bb2-F3 Bb3-F4
      B1-F#2 B2-F#3 B3-F#4 B#1-F##2 B#2-F##3 B#3-F##4 B##1-F###2 B##2-F###3
      B##3-F###4 Cbbb1-Gbbb1 Cbbb2-Gbbb2 Cbbb3-Gbbb3 Cbbb4-Gbbb4 Cbb1-Gbb1
      Cbb2-Gbb2 Cbb3-Gbb3 Cbb4-Gbb4 Cb1-Gb1 Cb2-Gb2 Cb3-Gb3 Cb4-Gb4 C1-G1 C2-G2
      C3-G3 C4-G4 C#1-G#1 C#2-G#2 C#3-G#3 C#4-G#4 C##1-G##1 C##2-G##2 C##3-G##3
      C##4-G##4 C###1-G###1 C###2-G###2 C###3-G###3 C###4-G###4 Dbbb1-Abbb1
      Dbbb2-Abbb2 Dbbb3-Abbb3 Dbbb4-Abbb4 Dbb1-Abb1 Dbb2-Abb2 Dbb3-Abb3 Dbb4-Abb4
      Db1-Ab1 Db2-Ab2 Db3-Ab3 Db4-Ab4 D1-A1 D2-A2 D3-A3 D4-A4 D#1-A#1 D#2-A#2
      D#3-A#3 D#4-A#4 D##1-A##1 D##2-A##2 D##3-A##3 D##4-A##4 D###1-A###1
      D###2-A###2 D###3-A###3 D###4-A###4 Ebbb1-Bbbb1 Ebbb2-Bbbb2 Ebbb3-Bbbb3
      Ebbb4-Bbbb4 Ebb1-Bbb1 Ebb2-Bbb2 Ebb3-Bbb3 Ebb4-Bbb4 Eb1-Bb1 Eb2-Bb2 Eb3-Bb3
      Eb4-Bb4 E1-B1 E2-B2 E3-B3 E4-B4 E#1-B#1 E#2-B#2 E#3-B#3 E#4-B#4 E##1-B##1
      E##2-B##2 E##3-B##3 E##4-B##4 E###1-B###1 E###2-B###2 E###3-B###3
      E###4-B###4 Fbbb1-Cbbb2 Fbbb2-Cbbb3 Fbbb3-Cbbb4 Fbb1-Cbb2 Fbb2-Cbb3
      Fbb3-Cbb4 Fb1-Cb2 Fb2-Cb3 Fb3-Cb4 F1-C2 F2-C3 F3-C4 F#1-C#2 F#2-C#3 F#3-C#4
      F##1-C##2 F##2-C##3 F##3-C##4 F###1-C###2 F###2-C###3 F###3-C###4
      Gbbb1-Dbbb2 Gbbb2-Dbbb3 Gbbb3-Dbbb4 Gbb1-Dbb2 Gbb2-Dbb3 Gbb3-Dbb4 Gb1-Db2
      Gb2-Db3 Gb3-Db4 G1-D2 G2-D3 G3-D4 G#1-D#2 G#2-D#3 G#3-D#4 G##1-D##2
      G##2-D##3 G##3-D##4 G###1-D###2 G###2-D###3 G###3-D###4))
    (7 d6 "diminished sixth"
     ((number Sixth) (quality Diminished) (additional_octaves 0))
     (Abb1-Fbbb2 Abb2-Fbbb3 Abb3-Fbbb4 Ab1-Fbb2 Ab2-Fbb3 Ab3-Fbb4 A1-Fb2 A2-Fb3
      A3-Fb4 A#1-F2 A#2-F3 A#3-F4 A##1-F#2 A##2-F#3 A##3-F#4 A###1-F##2
      A###2-F##3 A###3-F##4 Bbb1-Gbbb2 Bbb2-Gbbb3 Bbb3-Gbbb4 Bb1-Gbb2 Bb2-Gbb3
      Bb3-Gbb4 B1-Gb2 B2-Gb3 B3-Gb4 B#1-G2 B#2-G3 B#3-G4 B##1-G#2 B##2-G#3
      B##3-G#4 B###1-G##2 B###2-G##3 B###3-G##4 Cb1-Abbb1 Cb2-Abbb2 Cb3-Abbb3
      Cb4-Abbb4 C1-Abb1 C2-Abb2 C3-Abb3 C4-Abb4 C#1-Ab1 C#2-Ab2 C#3-Ab3 C#4-Ab4
      C##1-A1 C##2-A2 C##3-A3 C##4-A4 C###1-A#1 C###2-A#2 C###3-A#3 C###4-A#4
      Db1-Bbbb1 Db2-Bbbb2 Db3-Bbbb3 Db4-Bbbb4 D1-Bbb1 D2-Bbb2 D3-Bbb3 D4-Bbb4
      D#1-Bb1 D#2-Bb2 D#3-Bb3 D#4-Bb4 D##1-B1 D##2-B2 D##3-B3 D##4-B4 D###1-B#1
      D###2-B#2 D###3-B#3 D###4-B#4 Ebb1-Cbbb2 Ebb2-Cbbb3 Ebb3-Cbbb4 Eb1-Cbb2
      Eb2-Cbb3 Eb3-Cbb4 E1-Cb2 E2-Cb3 E3-Cb4 E#1-C2 E#2-C3 E#3-C4 E##1-C#2
      E##2-C#3 E##3-C#4 E###1-C##2 E###2-C##3 E###3-C##4 Fb1-Dbbb2 Fb2-Dbbb3
      Fb3-Dbbb4 F1-Dbb2 F2-Dbb3 F3-Dbb4 F#1-Db2 F#2-Db3 F#3-Db4 F##1-D2 F##2-D3
      F##3-D4 F###1-D#2 F###2-D#3 F###3-D#4 Gb1-Ebbb2 Gb2-Ebbb3 Gb3-Ebbb4 G1-Ebb2
      G2-Ebb3 G3-Ebb4 G#1-Eb2 G#2-Eb3 G#3-Eb4 G##1-E2 G##2-E3 G##3-E4 G###1-E#2
      G###2-E#3 G###3-E#4))
    (8 A5 "augmented fifth"
     ((number Fifth) (quality Augmented) (additional_octaves 0))
     (Abbb1-Ebb2 Abbb2-Ebb3 Abbb3-Ebb4 Abb1-Eb2 Abb2-Eb3 Abb3-Eb4 Ab1-E2 Ab2-E3
      Ab3-E4 A1-E#2 A2-E#3 A3-E#4 A#1-E##2 A#2-E##3 A#3-E##4 A##1-E###2
      A##2-E###3 A##3-E###4 Bbbb1-Fb2 Bbbb2-Fb3 Bbbb3-Fb4 Bbb1-F2 Bbb2-F3 Bbb3-F4
      Bb1-F#2 Bb2-F#3 Bb3-F#4 B1-F##2 B2-F##3 B3-F##4 B#1-F###2 B#2-F###3
      B#3-F###4 Cbbb1-Gbb1 Cbbb2-Gbb2 Cbbb3-Gbb3 Cbbb4-Gbb4 Cbb1-Gb1 Cbb2-Gb2
      Cbb3-Gb3 Cbb4-Gb4 Cb1-G1 Cb2-G2 Cb3-G3 Cb4-G4 C1-G#1 C2-G#2 C3-G#3 C4-G#4
      C#1-G##1 C#2-G##2 C#3-G##3 C#4-G##4 C##1-G###1 C##2-G###2 C##3-G###3
      C##4-G###4 Dbbb1-Abb1 Dbbb2-Abb2 Dbbb3-Abb3 Dbbb4-Abb4 Dbb1-Ab1 Dbb2-Ab2
      Dbb3-Ab3 Dbb4-Ab4 Db1-A1 Db2-A2 Db3-A3 Db4-A4 D1-A#1 D2-A#2 D3-A#3 D4-A#4
      D#1-A##1 D#2-A##2 D#3-A##3 D#4-A##4 D##1-A###1 D##2-A###2 D##3-A###3
      D##4-A###4 Ebbb1-Bbb1 Ebbb2-Bbb2 Ebbb3-Bbb3 Ebbb4-Bbb4 Ebb1-Bb1 Ebb2-Bb2
      Ebb3-Bb3 Ebb4-Bb4 Eb1-B1 Eb2-B2 Eb3-B3 Eb4-B4 E1-B#1 E2-B#2 E3-B#3 E4-B#4
      E#1-B##1 E#2-B##2 E#3-B##3 E#4-B##4 E##1-B###1 E##2-B###2 E##3-B###3
      E##4-B###4 Fbbb1-Cbb2 Fbbb2-Cbb3 Fbbb3-Cbb4 Fbb1-Cb2 Fbb2-Cb3 Fbb3-Cb4
      Fb1-C2 Fb2-C3 Fb3-C4 F1-C#2 F2-C#3 F3-C#4 F#1-C##2 F#2-C##3 F#3-C##4
      F##1-C###2 F##2-C###3 F##3-C###4 Gbbb1-Dbb2 Gbbb2-Dbb3 Gbbb3-Dbb4 Gbb1-Db2
      Gbb2-Db3 Gbb3-Db4 Gb1-D2 Gb2-D3 Gb3-D4 G1-D#2 G2-D#3 G3-D#4 G#1-D##2
      G#2-D##3 G#3-D##4 G##1-D###2 G##2-D###3 G##3-D###4))
    (8 m6 "minor sixth" ((number Sixth) (quality Minor) (additional_octaves 0))
     (Abbb1-Fbbb2 Abbb2-Fbbb3 Abbb3-Fbbb4 Abb1-Fbb2 Abb2-Fbb3 Abb3-Fbb4 Ab1-Fb2
      Ab2-Fb3 Ab3-Fb4 A1-F2 A2-F3 A3-F4 A#1-F#2 A#2-F#3 A#3-F#4 A##1-F##2
      A##2-F##3 A##3-F##4 A###1-F###2 A###2-F###3 A###3-F###4 Bbbb1-Gbbb2
      Bbbb2-Gbbb3 Bbbb3-Gbbb4 Bbb1-Gbb2 Bbb2-Gbb3 Bbb3-Gbb4 Bb1-Gb2 Bb2-Gb3
      Bb3-Gb4 B1-G2 B2-G3 B3-G4 B#1-G#2 B#2-G#3 B#3-G#4 B##1-G##2 B##2-G##3
      B##3-G##4 B###1-G###2 B###2-G###3 B###3-G###4 Cbb1-Abbb1 Cbb2-Abbb2
      Cbb3-Abbb3 Cbb4-Abbb4 Cb1-Abb1 Cb2-Abb2 Cb3-Abb3 Cb4-Abb4 C1-Ab1 C2-Ab2
      C3-Ab3 C4-Ab4 C#1-A1 C#2-A2 C#3-A3 C#4-A4 C##1-A#1 C##2-A#2 C##3-A#3
      C##4-A#4 C###1-A##1 C###2-A##2 C###3-A##3 C###4-A##4 Dbb1-Bbbb1 Dbb2-Bbbb2
      Dbb3-Bbbb3 Dbb4-Bbbb4 Db1-Bbb1 Db2-Bbb2 Db3-Bbb3 Db4-Bbb4 D1-Bb1 D2-Bb2
      D3-Bb3 D4-Bb4 D#1-B1 D#2-B2 D#3-B3 D#4-B4 D##1-B#1 D##2-B#2 D##3-B#3
      D##4-B#4 D###1-B##1 D###2-B##2 D###3-B##3 D###4-B##4 Ebbb1-Cbbb2
      Ebbb2-Cbbb3 Ebbb3-Cbbb4 Ebb1-Cbb2 Ebb2-Cbb3 Ebb3-Cbb4 Eb1-Cb2 Eb2-Cb3
      Eb3-Cb4 E1-C2 E2-C3 E3-C4 E#1-C#2 E#2-C#3 E#3-C#4 E##1-C##2 E##2-C##3
      E##3-C##4 E###1-C###2 E###2-C###3 E###3-C###4 Fbb1-Dbbb2 Fbb2-Dbbb3
      Fbb3-Dbbb4 Fb1-Dbb2 Fb2-Dbb3 Fb3-Dbb4 F1-Db2 F2-Db3 F3-Db4 F#1-D2 F#2-D3
      F#3-D4 F##1-D#2 F##2-D#3 F##3-D#4 F###1-D##2 F###2-D##3 F###3-D##4
      Gbb1-Ebbb2 Gbb2-Ebbb3 Gbb3-Ebbb4 Gb1-Ebb2 Gb2-Ebb3 Gb3-Ebb4 G1-Eb2 G2-Eb3
      G3-Eb4 G#1-E2 G#2-E3 G#3-E4 G##1-E#2 G##2-E#3 G##3-E#4 G###1-E##2
      G###2-E##3 G###3-E##4))
    (8 dd7 "doubly diminished seventh"
     ((number Seventh) (quality Doubly_diminished) (additional_octaves 0))
     (Ab1-Gbbb2 Ab2-Gbbb3 Ab3-Gbbb4 A1-Gbb2 A2-Gbb3 A3-Gbb4 A#1-Gb2 A#2-Gb3
      A#3-Gb4 A##1-G2 A##2-G3 A##3-G4 A###1-G#2 A###2-G#3 A###3-G#4 Bb1-Abbb2
      Bb2-Abbb3 Bb3-Abbb4 B1-Abb2 B2-Abb3 B3-Abb4 B#1-Ab2 B#2-Ab3 B#3-Ab4 B##1-A2
      B##2-A3 B##3-A4 B###1-A#2 B###2-A#3 B###3-A#4 C1-Bbbb1 C2-Bbbb2 C3-Bbbb3
      C4-Bbbb4 C#1-Bbb1 C#2-Bbb2 C#3-Bbb3 C#4-Bbb4 C##1-Bb1 C##2-Bb2 C##3-Bb3
      C##4-Bb4 C###1-B1 C###2-B2 C###3-B3 C###4-B4 Db1-Cbbb2 Db2-Cbbb3 Db3-Cbbb4
      D1-Cbb2 D2-Cbb3 D3-Cbb4 D#1-Cb2 D#2-Cb3 D#3-Cb4 D##1-C2 D##2-C3 D##3-C4
      D###1-C#2 D###2-C#3 D###3-C#4 Eb1-Dbbb2 Eb2-Dbbb3 Eb3-Dbbb4 E1-Dbb2 E2-Dbb3
      E3-Dbb4 E#1-Db2 E#2-Db3 E#3-Db4 E##1-D2 E##2-D3 E##3-D4 E###1-D#2 E###2-D#3
      E###3-D#4 F1-Ebbb2 F2-Ebbb3 F3-Ebbb4 F#1-Ebb2 F#2-Ebb3 F#3-Ebb4 F##1-Eb2
      F##2-Eb3 F##3-Eb4 F###1-E2 F###2-E3 F###3-E4 Gb1-Fbbb2 Gb2-Fbbb3 Gb3-Fbbb4
      G1-Fbb2 G2-Fbb3 G3-Fbb4 G#1-Fb2 G#2-Fb3 G#3-Fb4 G##1-F2 G##2-F3 G##3-F4
      G###1-F#2 G###2-F#3 G###3-F#4))
    (9 AA5 "doubly augmented fifth"
     ((number Fifth) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-Eb2 Abbb2-Eb3 Abbb3-Eb4 Abb1-E2 Abb2-E3 Abb3-E4 Ab1-E#2 Ab2-E#3
      Ab3-E#4 A1-E##2 A2-E##3 A3-E##4 A#1-E###2 A#2-E###3 A#3-E###4 Bbbb1-F2
      Bbbb2-F3 Bbbb3-F4 Bbb1-F#2 Bbb2-F#3 Bbb3-F#4 Bb1-F##2 Bb2-F##3 Bb3-F##4
      B1-F###2 B2-F###3 B3-F###4 Cbbb1-Gb1 Cbbb2-Gb2 Cbbb3-Gb3 Cbbb4-Gb4 Cbb1-G1
      Cbb2-G2 Cbb3-G3 Cbb4-G4 Cb1-G#1 Cb2-G#2 Cb3-G#3 Cb4-G#4 C1-G##1 C2-G##2
      C3-G##3 C4-G##4 C#1-G###1 C#2-G###2 C#3-G###3 C#4-G###4 Dbbb1-Ab1 Dbbb2-Ab2
      Dbbb3-Ab3 Dbbb4-Ab4 Dbb1-A1 Dbb2-A2 Dbb3-A3 Dbb4-A4 Db1-A#1 Db2-A#2 Db3-A#3
      Db4-A#4 D1-A##1 D2-A##2 D3-A##3 D4-A##4 D#1-A###1 D#2-A###2 D#3-A###3
      D#4-A###4 Ebbb1-Bb1 Ebbb2-Bb2 Ebbb3-Bb3 Ebbb4-Bb4 Ebb1-B1 Ebb2-B2 Ebb3-B3
      Ebb4-B4 Eb1-B#1 Eb2-B#2 Eb3-B#3 Eb4-B#4 E1-B##1 E2-B##2 E3-B##3 E4-B##4
      E#1-B###1 E#2-B###2 E#3-B###3 E#4-B###4 Fbbb1-Cb2 Fbbb2-Cb3 Fbbb3-Cb4
      Fbb1-C2 Fbb2-C3 Fbb3-C4 Fb1-C#2 Fb2-C#3 Fb3-C#4 F1-C##2 F2-C##3 F3-C##4
      F#1-C###2 F#2-C###3 F#3-C###4 Gbbb1-Db2 Gbbb2-Db3 Gbbb3-Db4 Gbb1-D2 Gbb2-D3
      Gbb3-D4 Gb1-D#2 Gb2-D#3 Gb3-D#4 G1-D##2 G2-D##3 G3-D##4 G#1-D###2 G#2-D###3
      G#3-D###4))
    (9 M6 "major sixth" ((number Sixth) (quality Major) (additional_octaves 0))
     (Abbb1-Fbb2 Abbb2-Fbb3 Abbb3-Fbb4 Abb1-Fb2 Abb2-Fb3 Abb3-Fb4 Ab1-F2 Ab2-F3
      Ab3-F4 A1-F#2 A2-F#3 A3-F#4 A#1-F##2 A#2-F##3 A#3-F##4 A##1-F###2
      A##2-F###3 A##3-F###4 Bbbb1-Gbb2 Bbbb2-Gbb3 Bbbb3-Gbb4 Bbb1-Gb2 Bbb2-Gb3
      Bbb3-Gb4 Bb1-G2 Bb2-G3 Bb3-G4 B1-G#2 B2-G#3 B3-G#4 B#1-G##2 B#2-G##3
      B#3-G##4 B##1-G###2 B##2-G###3 B##3-G###4 Cbbb1-Abbb1 Cbbb2-Abbb2
      Cbbb3-Abbb3 Cbbb4-Abbb4 Cbb1-Abb1 Cbb2-Abb2 Cbb3-Abb3 Cbb4-Abb4 Cb1-Ab1
      Cb2-Ab2 Cb3-Ab3 Cb4-Ab4 C1-A1 C2-A2 C3-A3 C4-A4 C#1-A#1 C#2-A#2 C#3-A#3
      C#4-A#4 C##1-A##1 C##2-A##2 C##3-A##3 C##4-A##4 C###1-A###1 C###2-A###2
      C###3-A###3 C###4-A###4 Dbbb1-Bbbb1 Dbbb2-Bbbb2 Dbbb3-Bbbb3 Dbbb4-Bbbb4
      Dbb1-Bbb1 Dbb2-Bbb2 Dbb3-Bbb3 Dbb4-Bbb4 Db1-Bb1 Db2-Bb2 Db3-Bb3 Db4-Bb4
      D1-B1 D2-B2 D3-B3 D4-B4 D#1-B#1 D#2-B#2 D#3-B#3 D#4-B#4 D##1-B##1 D##2-B##2
      D##3-B##3 D##4-B##4 D###1-B###1 D###2-B###2 D###3-B###3 D###4-B###4
      Ebbb1-Cbb2 Ebbb2-Cbb3 Ebbb3-Cbb4 Ebb1-Cb2 Ebb2-Cb3 Ebb3-Cb4 Eb1-C2 Eb2-C3
      Eb3-C4 E1-C#2 E2-C#3 E3-C#4 E#1-C##2 E#2-C##3 E#3-C##4 E##1-C###2
      E##2-C###3 E##3-C###4 Fbbb1-Dbbb2 Fbbb2-Dbbb3 Fbbb3-Dbbb4 Fbb1-Dbb2
      Fbb2-Dbb3 Fbb3-Dbb4 Fb1-Db2 Fb2-Db3 Fb3-Db4 F1-D2 F2-D3 F3-D4 F#1-D#2
      F#2-D#3 F#3-D#4 F##1-D##2 F##2-D##3 F##3-D##4 F###1-D###2 F###2-D###3
      F###3-D###4 Gbbb1-Ebbb2 Gbbb2-Ebbb3 Gbbb3-Ebbb4 Gbb1-Ebb2 Gbb2-Ebb3
      Gbb3-Ebb4 Gb1-Eb2 Gb2-Eb3 Gb3-Eb4 G1-E2 G2-E3 G3-E4 G#1-E#2 G#2-E#3 G#3-E#4
      G##1-E##2 G##2-E##3 G##3-E##4 G###1-E###2 G###2-E###3 G###3-E###4))
    (9 d7 "diminished seventh"
     ((number Seventh) (quality Diminished) (additional_octaves 0))
     (Abb1-Gbbb2 Abb2-Gbbb3 Abb3-Gbbb4 Ab1-Gbb2 Ab2-Gbb3 Ab3-Gbb4 A1-Gb2 A2-Gb3
      A3-Gb4 A#1-G2 A#2-G3 A#3-G4 A##1-G#2 A##2-G#3 A##3-G#4 A###1-G##2
      A###2-G##3 A###3-G##4 Bbb1-Abbb2 Bbb2-Abbb3 Bbb3-Abbb4 Bb1-Abb2 Bb2-Abb3
      Bb3-Abb4 B1-Ab2 B2-Ab3 B3-Ab4 B#1-A2 B#2-A3 B#3-A4 B##1-A#2 B##2-A#3
      B##3-A#4 B###1-A##2 B###2-A##3 B###3-A##4 Cb1-Bbbb1 Cb2-Bbbb2 Cb3-Bbbb3
      Cb4-Bbbb4 C1-Bbb1 C2-Bbb2 C3-Bbb3 C4-Bbb4 C#1-Bb1 C#2-Bb2 C#3-Bb3 C#4-Bb4
      C##1-B1 C##2-B2 C##3-B3 C##4-B4 C###1-B#1 C###2-B#2 C###3-B#3 C###4-B#4
      Dbb1-Cbbb2 Dbb2-Cbbb3 Dbb3-Cbbb4 Db1-Cbb2 Db2-Cbb3 Db3-Cbb4 D1-Cb2 D2-Cb3
      D3-Cb4 D#1-C2 D#2-C3 D#3-C4 D##1-C#2 D##2-C#3 D##3-C#4 D###1-C##2
      D###2-C##3 D###3-C##4 Ebb1-Dbbb2 Ebb2-Dbbb3 Ebb3-Dbbb4 Eb1-Dbb2 Eb2-Dbb3
      Eb3-Dbb4 E1-Db2 E2-Db3 E3-Db4 E#1-D2 E#2-D3 E#3-D4 E##1-D#2 E##2-D#3
      E##3-D#4 E###1-D##2 E###2-D##3 E###3-D##4 Fb1-Ebbb2 Fb2-Ebbb3 Fb3-Ebbb4
      F1-Ebb2 F2-Ebb3 F3-Ebb4 F#1-Eb2 F#2-Eb3 F#3-Eb4 F##1-E2 F##2-E3 F##3-E4
      F###1-E#2 F###2-E#3 F###3-E#4 Gbb1-Fbbb2 Gbb2-Fbbb3 Gbb3-Fbbb4 Gb1-Fbb2
      Gb2-Fbb3 Gb3-Fbb4 G1-Fb2 G2-Fb3 G3-Fb4 G#1-F2 G#2-F3 G#3-F4 G##1-F#2
      G##2-F#3 G##3-F#4 G###1-F##2 G###2-F##3 G###3-F##4))
    (10 A6 "augmented sixth"
     ((number Sixth) (quality Augmented) (additional_octaves 0))
     (Abbb1-Fb2 Abbb2-Fb3 Abbb3-Fb4 Abb1-F2 Abb2-F3 Abb3-F4 Ab1-F#2 Ab2-F#3
      Ab3-F#4 A1-F##2 A2-F##3 A3-F##4 A#1-F###2 A#2-F###3 A#3-F###4 Bbbb1-Gb2
      Bbbb2-Gb3 Bbbb3-Gb4 Bbb1-G2 Bbb2-G3 Bbb3-G4 Bb1-G#2 Bb2-G#3 Bb3-G#4 B1-G##2
      B2-G##3 B3-G##4 B#1-G###2 B#2-G###3 B#3-G###4 Cbbb1-Abb1 Cbbb2-Abb2
      Cbbb3-Abb3 Cbbb4-Abb4 Cbb1-Ab1 Cbb2-Ab2 Cbb3-Ab3 Cbb4-Ab4 Cb1-A1 Cb2-A2
      Cb3-A3 Cb4-A4 C1-A#1 C2-A#2 C3-A#3 C4-A#4 C#1-A##1 C#2-A##2 C#3-A##3
      C#4-A##4 C##1-A###1 C##2-A###2 C##3-A###3 C##4-A###4 Dbbb1-Bbb1 Dbbb2-Bbb2
      Dbbb3-Bbb3 Dbbb4-Bbb4 Dbb1-Bb1 Dbb2-Bb2 Dbb3-Bb3 Dbb4-Bb4 Db1-B1 Db2-B2
      Db3-B3 Db4-B4 D1-B#1 D2-B#2 D3-B#3 D4-B#4 D#1-B##1 D#2-B##2 D#3-B##3
      D#4-B##4 D##1-B###1 D##2-B###2 D##3-B###3 D##4-B###4 Ebbb1-Cb2 Ebbb2-Cb3
      Ebbb3-Cb4 Ebb1-C2 Ebb2-C3 Ebb3-C4 Eb1-C#2 Eb2-C#3 Eb3-C#4 E1-C##2 E2-C##3
      E3-C##4 E#1-C###2 E#2-C###3 E#3-C###4 Fbbb1-Dbb2 Fbbb2-Dbb3 Fbbb3-Dbb4
      Fbb1-Db2 Fbb2-Db3 Fbb3-Db4 Fb1-D2 Fb2-D3 Fb3-D4 F1-D#2 F2-D#3 F3-D#4
      F#1-D##2 F#2-D##3 F#3-D##4 F##1-D###2 F##2-D###3 F##3-D###4 Gbbb1-Ebb2
      Gbbb2-Ebb3 Gbbb3-Ebb4 Gbb1-Eb2 Gbb2-Eb3 Gbb3-Eb4 Gb1-E2 Gb2-E3 Gb3-E4
      G1-E#2 G2-E#3 G3-E#4 G#1-E##2 G#2-E##3 G#3-E##4 G##1-E###2 G##2-E###3
      G##3-E###4))
    (10 m7 "minor seventh"
     ((number Seventh) (quality Minor) (additional_octaves 0))
     (Abbb1-Gbbb2 Abbb2-Gbbb3 Abbb3-Gbbb4 Abb1-Gbb2 Abb2-Gbb3 Abb3-Gbb4 Ab1-Gb2
      Ab2-Gb3 Ab3-Gb4 A1-G2 A2-G3 A3-G4 A#1-G#2 A#2-G#3 A#3-G#4 A##1-G##2
      A##2-G##3 A##3-G##4 A###1-G###2 A###2-G###3 A###3-G###4 Bbbb1-Abbb2
      Bbbb2-Abbb3 Bbbb3-Abbb4 Bbb1-Abb2 Bbb2-Abb3 Bbb3-Abb4 Bb1-Ab2 Bb2-Ab3
      Bb3-Ab4 B1-A2 B2-A3 B3-A4 B#1-A#2 B#2-A#3 B#3-A#4 B##1-A##2 B##2-A##3
      B##3-A##4 B###1-A###2 B###2-A###3 B###3-A###4 Cbb1-Bbbb1 Cbb2-Bbbb2
      Cbb3-Bbbb3 Cbb4-Bbbb4 Cb1-Bbb1 Cb2-Bbb2 Cb3-Bbb3 Cb4-Bbb4 C1-Bb1 C2-Bb2
      C3-Bb3 C4-Bb4 C#1-B1 C#2-B2 C#3-B3 C#4-B4 C##1-B#1 C##2-B#2 C##3-B#3
      C##4-B#4 C###1-B##1 C###2-B##2 C###3-B##3 C###4-B##4 Dbbb1-Cbbb2
      Dbbb2-Cbbb3 Dbbb3-Cbbb4 Dbb1-Cbb2 Dbb2-Cbb3 Dbb3-Cbb4 Db1-Cb2 Db2-Cb3
      Db3-Cb4 D1-C2 D2-C3 D3-C4 D#1-C#2 D#2-C#3 D#3-C#4 D##1-C##2 D##2-C##3
      D##3-C##4 D###1-C###2 D###2-C###3 D###3-C###4 Ebbb1-Dbbb2 Ebbb2-Dbbb3
      Ebbb3-Dbbb4 Ebb1-Dbb2 Ebb2-Dbb3 Ebb3-Dbb4 Eb1-Db2 Eb2-Db3 Eb3-Db4 E1-D2
      E2-D3 E3-D4 E#1-D#2 E#2-D#3 E#3-D#4 E##1-D##2 E##2-D##3 E##3-D##4
      E###1-D###2 E###2-D###3 E###3-D###4 Fbb1-Ebbb2 Fbb2-Ebbb3 Fbb3-Ebbb4
      Fb1-Ebb2 Fb2-Ebb3 Fb3-Ebb4 F1-Eb2 F2-Eb3 F3-Eb4 F#1-E2 F#2-E3 F#3-E4
      F##1-E#2 F##2-E#3 F##3-E#4 F###1-E##2 F###2-E##3 F###3-E##4 Gbbb1-Fbbb2
      Gbbb2-Fbbb3 Gbbb3-Fbbb4 Gbb1-Fbb2 Gbb2-Fbb3 Gbb3-Fbb4 Gb1-Fb2 Gb2-Fb3
      Gb3-Fb4 G1-F2 G2-F3 G3-F4 G#1-F#2 G#2-F#3 G#3-F#4 G##1-F##2 G##2-F##3
      G##3-F##4 G###1-F###2 G###2-F###3 G###3-F###4))
    (10 dd8 "doubly diminished octave"
     ((number Octave) (quality Doubly_diminished) (additional_octaves 0))
     (Ab1-Abbb2 Ab2-Abbb3 Ab3-Abbb4 A1-Abb2 A2-Abb3 A3-Abb4 A#1-Ab2 A#2-Ab3
      A#3-Ab4 A##1-A2 A##2-A3 A##3-A4 A###1-A#2 A###2-A#3 A###3-A#4 Bb1-Bbbb2
      Bb2-Bbbb3 Bb3-Bbbb4 B1-Bbb2 B2-Bbb3 B3-Bbb4 B#1-Bb2 B#2-Bb3 B#3-Bb4 B##1-B2
      B##2-B3 B##3-B4 B###1-B#2 B###2-B#3 B###3-B#4 Cb1-Cbbb2 Cb2-Cbbb3 Cb3-Cbbb4
      C1-Cbb2 C2-Cbb3 C3-Cbb4 C#1-Cb2 C#2-Cb3 C#3-Cb4 C##1-C2 C##2-C3 C##3-C4
      C###1-C#2 C###2-C#3 C###3-C#4 Db1-Dbbb2 Db2-Dbbb3 Db3-Dbbb4 D1-Dbb2 D2-Dbb3
      D3-Dbb4 D#1-Db2 D#2-Db3 D#3-Db4 D##1-D2 D##2-D3 D##3-D4 D###1-D#2 D###2-D#3
      D###3-D#4 Eb1-Ebbb2 Eb2-Ebbb3 Eb3-Ebbb4 E1-Ebb2 E2-Ebb3 E3-Ebb4 E#1-Eb2
      E#2-Eb3 E#3-Eb4 E##1-E2 E##2-E3 E##3-E4 E###1-E#2 E###2-E#3 E###3-E#4
      Fb1-Fbbb2 Fb2-Fbbb3 Fb3-Fbbb4 F1-Fbb2 F2-Fbb3 F3-Fbb4 F#1-Fb2 F#2-Fb3
      F#3-Fb4 F##1-F2 F##2-F3 F##3-F4 F###1-F#2 F###2-F#3 F###3-F#4 Gb1-Gbbb2
      Gb2-Gbbb3 Gb3-Gbbb4 G1-Gbb2 G2-Gbb3 G3-Gbb4 G#1-Gb2 G#2-Gb3 G#3-Gb4 G##1-G2
      G##2-G3 G##3-G4 G###1-G#2 G###2-G#3 G###3-G#4))
    (11 AA6 "doubly augmented sixth"
     ((number Sixth) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-F2 Abbb2-F3 Abbb3-F4 Abb1-F#2 Abb2-F#3 Abb3-F#4 Ab1-F##2 Ab2-F##3
      Ab3-F##4 A1-F###2 A2-F###3 A3-F###4 Bbbb1-G2 Bbbb2-G3 Bbbb3-G4 Bbb1-G#2
      Bbb2-G#3 Bbb3-G#4 Bb1-G##2 Bb2-G##3 Bb3-G##4 B1-G###2 B2-G###3 B3-G###4
      Cbbb1-Ab1 Cbbb2-Ab2 Cbbb3-Ab3 Cbbb4-Ab4 Cbb1-A1 Cbb2-A2 Cbb3-A3 Cbb4-A4
      Cb1-A#1 Cb2-A#2 Cb3-A#3 Cb4-A#4 C1-A##1 C2-A##2 C3-A##3 C4-A##4 C#1-A###1
      C#2-A###2 C#3-A###3 C#4-A###4 Dbbb1-Bb1 Dbbb2-Bb2 Dbbb3-Bb3 Dbbb4-Bb4
      Dbb1-B1 Dbb2-B2 Dbb3-B3 Dbb4-B4 Db1-B#1 Db2-B#2 Db3-B#3 Db4-B#4 D1-B##1
      D2-B##2 D3-B##3 D4-B##4 D#1-B###1 D#2-B###2 D#3-B###3 D#4-B###4 Ebbb1-C2
      Ebbb2-C3 Ebbb3-C4 Ebb1-C#2 Ebb2-C#3 Ebb3-C#4 Eb1-C##2 Eb2-C##3 Eb3-C##4
      E1-C###2 E2-C###3 E3-C###4 Fbbb1-Db2 Fbbb2-Db3 Fbbb3-Db4 Fbb1-D2 Fbb2-D3
      Fbb3-D4 Fb1-D#2 Fb2-D#3 Fb3-D#4 F1-D##2 F2-D##3 F3-D##4 F#1-D###2 F#2-D###3
      F#3-D###4 Gbbb1-Eb2 Gbbb2-Eb3 Gbbb3-Eb4 Gbb1-E2 Gbb2-E3 Gbb3-E4 Gb1-E#2
      Gb2-E#3 Gb3-E#4 G1-E##2 G2-E##3 G3-E##4 G#1-E###2 G#2-E###3 G#3-E###4))
    (11 M7 "major seventh"
     ((number Seventh) (quality Major) (additional_octaves 0))
     (Abbb1-Gbb2 Abbb2-Gbb3 Abbb3-Gbb4 Abb1-Gb2 Abb2-Gb3 Abb3-Gb4 Ab1-G2 Ab2-G3
      Ab3-G4 A1-G#2 A2-G#3 A3-G#4 A#1-G##2 A#2-G##3 A#3-G##4 A##1-G###2
      A##2-G###3 A##3-G###4 Bbbb1-Abb2 Bbbb2-Abb3 Bbbb3-Abb4 Bbb1-Ab2 Bbb2-Ab3
      Bbb3-Ab4 Bb1-A2 Bb2-A3 Bb3-A4 B1-A#2 B2-A#3 B3-A#4 B#1-A##2 B#2-A##3
      B#3-A##4 B##1-A###2 B##2-A###3 B##3-A###4 Cbbb1-Bbbb1 Cbbb2-Bbbb2
      Cbbb3-Bbbb3 Cbbb4-Bbbb4 Cbb1-Bbb1 Cbb2-Bbb2 Cbb3-Bbb3 Cbb4-Bbb4 Cb1-Bb1
      Cb2-Bb2 Cb3-Bb3 Cb4-Bb4 C1-B1 C2-B2 C3-B3 C4-B4 C#1-B#1 C#2-B#2 C#3-B#3
      C#4-B#4 C##1-B##1 C##2-B##2 C##3-B##3 C##4-B##4 C###1-B###1 C###2-B###2
      C###3-B###3 C###4-B###4 Dbbb1-Cbb2 Dbbb2-Cbb3 Dbbb3-Cbb4 Dbb1-Cb2 Dbb2-Cb3
      Dbb3-Cb4 Db1-C2 Db2-C3 Db3-C4 D1-C#2 D2-C#3 D3-C#4 D#1-C##2 D#2-C##3
      D#3-C##4 D##1-C###2 D##2-C###3 D##3-C###4 Ebbb1-Dbb2 Ebbb2-Dbb3 Ebbb3-Dbb4
      Ebb1-Db2 Ebb2-Db3 Ebb3-Db4 Eb1-D2 Eb2-D3 Eb3-D4 E1-D#2 E2-D#3 E3-D#4
      E#1-D##2 E#2-D##3 E#3-D##4 E##1-D###2 E##2-D###3 E##3-D###4 Fbbb1-Ebbb2
      Fbbb2-Ebbb3 Fbbb3-Ebbb4 Fbb1-Ebb2 Fbb2-Ebb3 Fbb3-Ebb4 Fb1-Eb2 Fb2-Eb3
      Fb3-Eb4 F1-E2 F2-E3 F3-E4 F#1-E#2 F#2-E#3 F#3-E#4 F##1-E##2 F##2-E##3
      F##3-E##4 F###1-E###2 F###2-E###3 F###3-E###4 Gbbb1-Fbb2 Gbbb2-Fbb3
      Gbbb3-Fbb4 Gbb1-Fb2 Gbb2-Fb3 Gbb3-Fb4 Gb1-F2 Gb2-F3 Gb3-F4 G1-F#2 G2-F#3
      G3-F#4 G#1-F##2 G#2-F##3 G#3-F##4 G##1-F###2 G##2-F###3 G##3-F###4))
    (11 d8 "diminished octave"
     ((number Octave) (quality Diminished) (additional_octaves 0))
     (Abb1-Abbb2 Abb2-Abbb3 Abb3-Abbb4 Ab1-Abb2 Ab2-Abb3 Ab3-Abb4 A1-Ab2 A2-Ab3
      A3-Ab4 A#1-A2 A#2-A3 A#3-A4 A##1-A#2 A##2-A#3 A##3-A#4 A###1-A##2
      A###2-A##3 A###3-A##4 Bbb1-Bbbb2 Bbb2-Bbbb3 Bbb3-Bbbb4 Bb1-Bbb2 Bb2-Bbb3
      Bb3-Bbb4 B1-Bb2 B2-Bb3 B3-Bb4 B#1-B2 B#2-B3 B#3-B4 B##1-B#2 B##2-B#3
      B##3-B#4 B###1-B##2 B###2-B##3 B###3-B##4 Cbb1-Cbbb2 Cbb2-Cbbb3 Cbb3-Cbbb4
      Cb1-Cbb2 Cb2-Cbb3 Cb3-Cbb4 C1-Cb2 C2-Cb3 C3-Cb4 C#1-C2 C#2-C3 C#3-C4
      C##1-C#2 C##2-C#3 C##3-C#4 C###1-C##2 C###2-C##3 C###3-C##4 Dbb1-Dbbb2
      Dbb2-Dbbb3 Dbb3-Dbbb4 Db1-Dbb2 Db2-Dbb3 Db3-Dbb4 D1-Db2 D2-Db3 D3-Db4
      D#1-D2 D#2-D3 D#3-D4 D##1-D#2 D##2-D#3 D##3-D#4 D###1-D##2 D###2-D##3
      D###3-D##4 Ebb1-Ebbb2 Ebb2-Ebbb3 Ebb3-Ebbb4 Eb1-Ebb2 Eb2-Ebb3 Eb3-Ebb4
      E1-Eb2 E2-Eb3 E3-Eb4 E#1-E2 E#2-E3 E#3-E4 E##1-E#2 E##2-E#3 E##3-E#4
      E###1-E##2 E###2-E##3 E###3-E##4 Fbb1-Fbbb2 Fbb2-Fbbb3 Fbb3-Fbbb4 Fb1-Fbb2
      Fb2-Fbb3 Fb3-Fbb4 F1-Fb2 F2-Fb3 F3-Fb4 F#1-F2 F#2-F3 F#3-F4 F##1-F#2
      F##2-F#3 F##3-F#4 F###1-F##2 F###2-F##3 F###3-F##4 Gbb1-Gbbb2 Gbb2-Gbbb3
      Gbb3-Gbbb4 Gb1-Gbb2 Gb2-Gbb3 Gb3-Gbb4 G1-Gb2 G2-Gb3 G3-Gb4 G#1-G2 G#2-G3
      G#3-G4 G##1-G#2 G##2-G#3 G##3-G#4 G###1-G##2 G###2-G##3 G###3-G##4))
    (12 P8 octave ((number Unison) (quality Perfect) (additional_octaves 1))
     (Abbb1-Abbb2 Abbb2-Abbb3 Abbb3-Abbb4 Abb1-Abb2 Abb2-Abb3 Abb3-Abb4 Ab1-Ab2
      Ab2-Ab3 Ab3-Ab4 A1-A2 A2-A3 A3-A4 A#1-A#2 A#2-A#3 A#3-A#4 A##1-A##2
      A##2-A##3 A##3-A##4 A###1-A###2 A###2-A###3 A###3-A###4 Bbbb1-Bbbb2
      Bbbb2-Bbbb3 Bbbb3-Bbbb4 Bbb1-Bbb2 Bbb2-Bbb3 Bbb3-Bbb4 Bb1-Bb2 Bb2-Bb3
      Bb3-Bb4 B1-B2 B2-B3 B3-B4 B#1-B#2 B#2-B#3 B#3-B#4 B##1-B##2 B##2-B##3
      B##3-B##4 B###1-B###2 B###2-B###3 B###3-B###4 Cbbb1-Cbbb2 Cbbb2-Cbbb3
      Cbbb3-Cbbb4 Cbb1-Cbb2 Cbb2-Cbb3 Cbb3-Cbb4 Cb1-Cb2 Cb2-Cb3 Cb3-Cb4 C1-C2
      C2-C3 C3-C4 C#1-C#2 C#2-C#3 C#3-C#4 C##1-C##2 C##2-C##3 C##3-C##4
      C###1-C###2 C###2-C###3 C###3-C###4 Dbbb1-Dbbb2 Dbbb2-Dbbb3 Dbbb3-Dbbb4
      Dbb1-Dbb2 Dbb2-Dbb3 Dbb3-Dbb4 Db1-Db2 Db2-Db3 Db3-Db4 D1-D2 D2-D3 D3-D4
      D#1-D#2 D#2-D#3 D#3-D#4 D##1-D##2 D##2-D##3 D##3-D##4 D###1-D###2
      D###2-D###3 D###3-D###4 Ebbb1-Ebbb2 Ebbb2-Ebbb3 Ebbb3-Ebbb4 Ebb1-Ebb2
      Ebb2-Ebb3 Ebb3-Ebb4 Eb1-Eb2 Eb2-Eb3 Eb3-Eb4 E1-E2 E2-E3 E3-E4 E#1-E#2
      E#2-E#3 E#3-E#4 E##1-E##2 E##2-E##3 E##3-E##4 E###1-E###2 E###2-E###3
      E###3-E###4 Fbbb1-Fbbb2 Fbbb2-Fbbb3 Fbbb3-Fbbb4 Fbb1-Fbb2 Fbb2-Fbb3
      Fbb3-Fbb4 Fb1-Fb2 Fb2-Fb3 Fb3-Fb4 F1-F2 F2-F3 F3-F4 F#1-F#2 F#2-F#3 F#3-F#4
      F##1-F##2 F##2-F##3 F##3-F##4 F###1-F###2 F###2-F###3 F###3-F###4
      Gbbb1-Gbbb2 Gbbb2-Gbbb3 Gbbb3-Gbbb4 Gbb1-Gbb2 Gbb2-Gbb3 Gbb3-Gbb4 Gb1-Gb2
      Gb2-Gb3 Gb3-Gb4 G1-G2 G2-G3 G3-G4 G#1-G#2 G#2-G#3 G#3-G#4 G##1-G##2
      G##2-G##3 G##3-G##4 G###1-G###2 G###2-G###3 G###3-G###4))
    (12 "P8 + d2" "octave + diminished second"
     ((number Second) (quality Diminished) (additional_octaves 1))
     (Ab1-Bbbb2 Ab2-Bbbb3 Ab3-Bbbb4 A1-Bbb2 A2-Bbb3 A3-Bbb4 A#1-Bb2 A#2-Bb3
      A#3-Bb4 A##1-B2 A##2-B3 A##3-B4 A###1-B#2 A###2-B#3 A###3-B#4 Bbb1-Cbbb3
      Bbb2-Cbbb4 Bb1-Cbb3 Bb2-Cbb4 B1-Cb3 B2-Cb4 B#1-C3 B#2-C4 B##1-C#3 B##2-C#4
      B###1-C##3 B###2-C##4 Cb1-Dbbb2 Cb2-Dbbb3 Cb3-Dbbb4 C1-Dbb2 C2-Dbb3 C3-Dbb4
      C#1-Db2 C#2-Db3 C#3-Db4 C##1-D2 C##2-D3 C##3-D4 C###1-D#2 C###2-D#3
      C###3-D#4 Db1-Ebbb2 Db2-Ebbb3 Db3-Ebbb4 D1-Ebb2 D2-Ebb3 D3-Ebb4 D#1-Eb2
      D#2-Eb3 D#3-Eb4 D##1-E2 D##2-E3 D##3-E4 D###1-E#2 D###2-E#3 D###3-E#4
      Ebb1-Fbbb2 Ebb2-Fbbb3 Ebb3-Fbbb4 Eb1-Fbb2 Eb2-Fbb3 Eb3-Fbb4 E1-Fb2 E2-Fb3
      E3-Fb4 E#1-F2 E#2-F3 E#3-F4 E##1-F#2 E##2-F#3 E##3-F#4 E###1-F##2
      E###2-F##3 E###3-F##4 Fb1-Gbbb2 Fb2-Gbbb3 Fb3-Gbbb4 F1-Gbb2 F2-Gbb3 F3-Gbb4
      F#1-Gb2 F#2-Gb3 F#3-Gb4 F##1-G2 F##2-G3 F##3-G4 F###1-G#2 F###2-G#3
      F###3-G#4 Gb1-Abbb2 Gb2-Abbb3 Gb3-Abbb4 G1-Abb2 G2-Abb3 G3-Abb4 G#1-Ab2
      G#2-Ab3 G#3-Ab4 G##1-A2 G##2-A3 G##3-A4 G###1-A#2 G###2-A#3 G###3-A#4))
    (12 A7 "augmented seventh"
     ((number Seventh) (quality Augmented) (additional_octaves 0))
     (Abbb1-Gb2 Abbb2-Gb3 Abbb3-Gb4 Abb1-G2 Abb2-G3 Abb3-G4 Ab1-G#2 Ab2-G#3
      Ab3-G#4 A1-G##2 A2-G##3 A3-G##4 A#1-G###2 A#2-G###3 A#3-G###4 Bbbb1-Ab2
      Bbbb2-Ab3 Bbbb3-Ab4 Bbb1-A2 Bbb2-A3 Bbb3-A4 Bb1-A#2 Bb2-A#3 Bb3-A#4 B1-A##2
      B2-A##3 B3-A##4 B#1-A###2 B#2-A###3 B#3-A###4 Cbbb1-Bbb1 Cbbb2-Bbb2
      Cbbb3-Bbb3 Cbbb4-Bbb4 Cbb1-Bb1 Cbb2-Bb2 Cbb3-Bb3 Cbb4-Bb4 Cb1-B1 Cb2-B2
      Cb3-B3 Cb4-B4 C1-B#1 C2-B#2 C3-B#3 C4-B#4 C#1-B##1 C#2-B##2 C#3-B##3
      C#4-B##4 C##1-B###1 C##2-B###2 C##3-B###3 C##4-B###4 Dbbb1-Cb2 Dbbb2-Cb3
      Dbbb3-Cb4 Dbb1-C2 Dbb2-C3 Dbb3-C4 Db1-C#2 Db2-C#3 Db3-C#4 D1-C##2 D2-C##3
      D3-C##4 D#1-C###2 D#2-C###3 D#3-C###4 Ebbb1-Db2 Ebbb2-Db3 Ebbb3-Db4 Ebb1-D2
      Ebb2-D3 Ebb3-D4 Eb1-D#2 Eb2-D#3 Eb3-D#4 E1-D##2 E2-D##3 E3-D##4 E#1-D###2
      E#2-D###3 E#3-D###4 Fbbb1-Ebb2 Fbbb2-Ebb3 Fbbb3-Ebb4 Fbb1-Eb2 Fbb2-Eb3
      Fbb3-Eb4 Fb1-E2 Fb2-E3 Fb3-E4 F1-E#2 F2-E#3 F3-E#4 F#1-E##2 F#2-E##3
      F#3-E##4 F##1-E###2 F##2-E###3 F##3-E###4 Gbbb1-Fb2 Gbbb2-Fb3 Gbbb3-Fb4
      Gbb1-F2 Gbb2-F3 Gbb3-F4 Gb1-F#2 Gb2-F#3 Gb3-F#4 G1-F##2 G2-F##3 G3-F##4
      G#1-F###2 G#2-F###3 G#3-F###4))
    (13 "P8 + A1" "octave + augmented unison"
     ((number Unison) (quality Augmented) (additional_octaves 1))
     (Abbb1-Abb2 Abbb2-Abb3 Abbb3-Abb4 Abb1-Ab2 Abb2-Ab3 Abb3-Ab4 Ab1-A2 Ab2-A3
      Ab3-A4 A1-A#2 A2-A#3 A3-A#4 A#1-A##2 A#2-A##3 A#3-A##4 A##1-A###2
      A##2-A###3 A##3-A###4 Bbbb1-Bbb2 Bbbb2-Bbb3 Bbbb3-Bbb4 Bbb1-Bb2 Bbb2-Bb3
      Bbb3-Bb4 Bb1-B2 Bb2-B3 Bb3-B4 B1-B#2 B2-B#3 B3-B#4 B#1-B##2 B#2-B##3
      B#3-B##4 B##1-B###2 B##2-B###3 B##3-B###4 Cbbb1-Cbb2 Cbbb2-Cbb3 Cbbb3-Cbb4
      Cbb1-Cb2 Cbb2-Cb3 Cbb3-Cb4 Cb1-C2 Cb2-C3 Cb3-C4 C1-C#2 C2-C#3 C3-C#4
      C#1-C##2 C#2-C##3 C#3-C##4 C##1-C###2 C##2-C###3 C##3-C###4 Dbbb1-Dbb2
      Dbbb2-Dbb3 Dbbb3-Dbb4 Dbb1-Db2 Dbb2-Db3 Dbb3-Db4 Db1-D2 Db2-D3 Db3-D4
      D1-D#2 D2-D#3 D3-D#4 D#1-D##2 D#2-D##3 D#3-D##4 D##1-D###2 D##2-D###3
      D##3-D###4 Ebbb1-Ebb2 Ebbb2-Ebb3 Ebbb3-Ebb4 Ebb1-Eb2 Ebb2-Eb3 Ebb3-Eb4
      Eb1-E2 Eb2-E3 Eb3-E4 E1-E#2 E2-E#3 E3-E#4 E#1-E##2 E#2-E##3 E#3-E##4
      E##1-E###2 E##2-E###3 E##3-E###4 Fbbb1-Fbb2 Fbbb2-Fbb3 Fbbb3-Fbb4 Fbb1-Fb2
      Fbb2-Fb3 Fbb3-Fb4 Fb1-F2 Fb2-F3 Fb3-F4 F1-F#2 F2-F#3 F3-F#4 F#1-F##2
      F#2-F##3 F#3-F##4 F##1-F###2 F##2-F###3 F##3-F###4 Gbbb1-Gbb2 Gbbb2-Gbb3
      Gbbb3-Gbb4 Gbb1-Gb2 Gbb2-Gb3 Gbb3-Gb4 Gb1-G2 Gb2-G3 Gb3-G4 G1-G#2 G2-G#3
      G3-G#4 G#1-G##2 G#2-G##3 G#3-G##4 G##1-G###2 G##2-G###3 G##3-G###4))
    (13 "P8 + m2" "octave + minor second"
     ((number Second) (quality Minor) (additional_octaves 1))
     (Abb1-Bbbb2 Abb2-Bbbb3 Abb3-Bbbb4 Ab1-Bbb2 Ab2-Bbb3 Ab3-Bbb4 A1-Bb2 A2-Bb3
      A3-Bb4 A#1-B2 A#2-B3 A#3-B4 A##1-B#2 A##2-B#3 A##3-B#4 A###1-B##2
      A###2-B##3 A###3-B##4 Bbbb1-Cbbb3 Bbbb2-Cbbb4 Bbb1-Cbb3 Bbb2-Cbb4 Bb1-Cb3
      Bb2-Cb4 B1-C3 B2-C4 B#1-C#3 B#2-C#4 B##1-C##3 B##2-C##4 B###1-C###3
      B###2-C###4 Cbb1-Dbbb2 Cbb2-Dbbb3 Cbb3-Dbbb4 Cb1-Dbb2 Cb2-Dbb3 Cb3-Dbb4
      C1-Db2 C2-Db3 C3-Db4 C#1-D2 C#2-D3 C#3-D4 C##1-D#2 C##2-D#3 C##3-D#4
      C###1-D##2 C###2-D##3 C###3-D##4 Dbb1-Ebbb2 Dbb2-Ebbb3 Dbb3-Ebbb4 Db1-Ebb2
      Db2-Ebb3 Db3-Ebb4 D1-Eb2 D2-Eb3 D3-Eb4 D#1-E2 D#2-E3 D#3-E4 D##1-E#2
      D##2-E#3 D##3-E#4 D###1-E##2 D###2-E##3 D###3-E##4 Ebbb1-Fbbb2 Ebbb2-Fbbb3
      Ebbb3-Fbbb4 Ebb1-Fbb2 Ebb2-Fbb3 Ebb3-Fbb4 Eb1-Fb2 Eb2-Fb3 Eb3-Fb4 E1-F2
      E2-F3 E3-F4 E#1-F#2 E#2-F#3 E#3-F#4 E##1-F##2 E##2-F##3 E##3-F##4
      E###1-F###2 E###2-F###3 E###3-F###4 Fbb1-Gbbb2 Fbb2-Gbbb3 Fbb3-Gbbb4
      Fb1-Gbb2 Fb2-Gbb3 Fb3-Gbb4 F1-Gb2 F2-Gb3 F3-Gb4 F#1-G2 F#2-G3 F#3-G4
      F##1-G#2 F##2-G#3 F##3-G#4 F###1-G##2 F###2-G##3 F###3-G##4 Gbb1-Abbb2
      Gbb2-Abbb3 Gbb3-Abbb4 Gb1-Abb2 Gb2-Abb3 Gb3-Abb4 G1-Ab2 G2-Ab3 G3-Ab4
      G#1-A2 G#2-A3 G#3-A4 G##1-A#2 G##2-A#3 G##3-A#4 G###1-A##2 G###2-A##3
      G###3-A##4))
    (13 "P8 + dd3" "octave + doubly diminished third"
     ((number Third) (quality Doubly_diminished) (additional_octaves 1))
     (Ab1-Cbbb3 Ab2-Cbbb4 A1-Cbb3 A2-Cbb4 A#1-Cb3 A#2-Cb4 A##1-C3 A##2-C4
      A###1-C#3 A###2-C#4 Bb1-Dbbb3 Bb2-Dbbb4 B1-Dbb3 B2-Dbb4 B#1-Db3 B#2-Db4
      B##1-D3 B##2-D4 B###1-D#3 B###2-D#4 C1-Ebbb2 C2-Ebbb3 C3-Ebbb4 C#1-Ebb2
      C#2-Ebb3 C#3-Ebb4 C##1-Eb2 C##2-Eb3 C##3-Eb4 C###1-E2 C###2-E3 C###3-E4
      Db1-Fbbb2 Db2-Fbbb3 Db3-Fbbb4 D1-Fbb2 D2-Fbb3 D3-Fbb4 D#1-Fb2 D#2-Fb3
      D#3-Fb4 D##1-F2 D##2-F3 D##3-F4 D###1-F#2 D###2-F#3 D###3-F#4 Eb1-Gbbb2
      Eb2-Gbbb3 Eb3-Gbbb4 E1-Gbb2 E2-Gbb3 E3-Gbb4 E#1-Gb2 E#2-Gb3 E#3-Gb4 E##1-G2
      E##2-G3 E##3-G4 E###1-G#2 E###2-G#3 E###3-G#4 F1-Abbb2 F2-Abbb3 F3-Abbb4
      F#1-Abb2 F#2-Abb3 F#3-Abb4 F##1-Ab2 F##2-Ab3 F##3-Ab4 F###1-A2 F###2-A3
      F###3-A4 G1-Bbbb2 G2-Bbbb3 G3-Bbbb4 G#1-Bbb2 G#2-Bbb3 G#3-Bbb4 G##1-Bb2
      G##2-Bb3 G##3-Bb4 G###1-B2 G###2-B3 G###3-B4))
    (13 AA7 "doubly augmented seventh"
     ((number Seventh) (quality Doubly_augmented) (additional_octaves 0))
     (Abbb1-G2 Abbb2-G3 Abbb3-G4 Abb1-G#2 Abb2-G#3 Abb3-G#4 Ab1-G##2 Ab2-G##3
      Ab3-G##4 A1-G###2 A2-G###3 A3-G###4 Bbbb1-A2 Bbbb2-A3 Bbbb3-A4 Bbb1-A#2
      Bbb2-A#3 Bbb3-A#4 Bb1-A##2 Bb2-A##3 Bb3-A##4 B1-A###2 B2-A###3 B3-A###4
      Cbbb1-Bb1 Cbbb2-Bb2 Cbbb3-Bb3 Cbbb4-Bb4 Cbb1-B1 Cbb2-B2 Cbb3-B3 Cbb4-B4
      Cb1-B#1 Cb2-B#2 Cb3-B#3 Cb4-B#4 C1-B##1 C2-B##2 C3-B##3 C4-B##4 C#1-B###1
      C#2-B###2 C#3-B###3 C#4-B###4 Dbbb1-C2 Dbbb2-C3 Dbbb3-C4 Dbb1-C#2 Dbb2-C#3
      Dbb3-C#4 Db1-C##2 Db2-C##3 Db3-C##4 D1-C###2 D2-C###3 D3-C###4 Ebbb1-D2
      Ebbb2-D3 Ebbb3-D4 Ebb1-D#2 Ebb2-D#3 Ebb3-D#4 Eb1-D##2 Eb2-D##3 Eb3-D##4
      E1-D###2 E2-D###3 E3-D###4 Fbbb1-Eb2 Fbbb2-Eb3 Fbbb3-Eb4 Fbb1-E2 Fbb2-E3
      Fbb3-E4 Fb1-E#2 Fb2-E#3 Fb3-E#4 F1-E##2 F2-E##3 F3-E##4 F#1-E###2 F#2-E###3
      F#3-E###4 Gbbb1-F2 Gbbb2-F3 Gbbb3-F4 Gbb1-F#2 Gbb2-F#3 Gbb3-F#4 Gb1-F##2
      Gb2-F##3 Gb3-F##4 G1-F###2 G2-F###3 G3-F###4))
    (14 "P8 + AA1" "octave + doubly augmented unison"
     ((number Unison) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-Ab2 Abbb2-Ab3 Abbb3-Ab4 Abb1-A2 Abb2-A3 Abb3-A4 Ab1-A#2 Ab2-A#3
      Ab3-A#4 A1-A##2 A2-A##3 A3-A##4 A#1-A###2 A#2-A###3 A#3-A###4 Bbbb1-Bb2
      Bbbb2-Bb3 Bbbb3-Bb4 Bbb1-B2 Bbb2-B3 Bbb3-B4 Bb1-B#2 Bb2-B#3 Bb3-B#4 B1-B##2
      B2-B##3 B3-B##4 B#1-B###2 B#2-B###3 B#3-B###4 Cbbb1-Cb2 Cbbb2-Cb3 Cbbb3-Cb4
      Cbb1-C2 Cbb2-C3 Cbb3-C4 Cb1-C#2 Cb2-C#3 Cb3-C#4 C1-C##2 C2-C##3 C3-C##4
      C#1-C###2 C#2-C###3 C#3-C###4 Dbbb1-Db2 Dbbb2-Db3 Dbbb3-Db4 Dbb1-D2 Dbb2-D3
      Dbb3-D4 Db1-D#2 Db2-D#3 Db3-D#4 D1-D##2 D2-D##3 D3-D##4 D#1-D###2 D#2-D###3
      D#3-D###4 Ebbb1-Eb2 Ebbb2-Eb3 Ebbb3-Eb4 Ebb1-E2 Ebb2-E3 Ebb3-E4 Eb1-E#2
      Eb2-E#3 Eb3-E#4 E1-E##2 E2-E##3 E3-E##4 E#1-E###2 E#2-E###3 E#3-E###4
      Fbbb1-Fb2 Fbbb2-Fb3 Fbbb3-Fb4 Fbb1-F2 Fbb2-F3 Fbb3-F4 Fb1-F#2 Fb2-F#3
      Fb3-F#4 F1-F##2 F2-F##3 F3-F##4 F#1-F###2 F#2-F###3 F#3-F###4 Gbbb1-Gb2
      Gbbb2-Gb3 Gbbb3-Gb4 Gbb1-G2 Gbb2-G3 Gbb3-G4 Gb1-G#2 Gb2-G#3 Gb3-G#4 G1-G##2
      G2-G##3 G3-G##4 G#1-G###2 G#2-G###3 G#3-G###4))
    (14 "P8 + M2" "octave + major second"
     ((number Second) (quality Major) (additional_octaves 1))
     (Abbb1-Bbbb2 Abbb2-Bbbb3 Abbb3-Bbbb4 Abb1-Bbb2 Abb2-Bbb3 Abb3-Bbb4 Ab1-Bb2
      Ab2-Bb3 Ab3-Bb4 A1-B2 A2-B3 A3-B4 A#1-B#2 A#2-B#3 A#3-B#4 A##1-B##2
      A##2-B##3 A##3-B##4 A###1-B###2 A###2-B###3 A###3-B###4 Bbbb1-Cbb3
      Bbbb2-Cbb4 Bbb1-Cb3 Bbb2-Cb4 Bb1-C3 Bb2-C4 B1-C#3 B2-C#4 B#1-C##3 B#2-C##4
      B##1-C###3 B##2-C###4 Cbbb1-Dbbb2 Cbbb2-Dbbb3 Cbbb3-Dbbb4 Cbb1-Dbb2
      Cbb2-Dbb3 Cbb3-Dbb4 Cb1-Db2 Cb2-Db3 Cb3-Db4 C1-D2 C2-D3 C3-D4 C#1-D#2
      C#2-D#3 C#3-D#4 C##1-D##2 C##2-D##3 C##3-D##4 C###1-D###2 C###2-D###3
      C###3-D###4 Dbbb1-Ebbb2 Dbbb2-Ebbb3 Dbbb3-Ebbb4 Dbb1-Ebb2 Dbb2-Ebb3
      Dbb3-Ebb4 Db1-Eb2 Db2-Eb3 Db3-Eb4 D1-E2 D2-E3 D3-E4 D#1-E#2 D#2-E#3 D#3-E#4
      D##1-E##2 D##2-E##3 D##3-E##4 D###1-E###2 D###2-E###3 D###3-E###4
      Ebbb1-Fbb2 Ebbb2-Fbb3 Ebbb3-Fbb4 Ebb1-Fb2 Ebb2-Fb3 Ebb3-Fb4 Eb1-F2 Eb2-F3
      Eb3-F4 E1-F#2 E2-F#3 E3-F#4 E#1-F##2 E#2-F##3 E#3-F##4 E##1-F###2
      E##2-F###3 E##3-F###4 Fbbb1-Gbbb2 Fbbb2-Gbbb3 Fbbb3-Gbbb4 Fbb1-Gbb2
      Fbb2-Gbb3 Fbb3-Gbb4 Fb1-Gb2 Fb2-Gb3 Fb3-Gb4 F1-G2 F2-G3 F3-G4 F#1-G#2
      F#2-G#3 F#3-G#4 F##1-G##2 F##2-G##3 F##3-G##4 F###1-G###2 F###2-G###3
      F###3-G###4 Gbbb1-Abbb2 Gbbb2-Abbb3 Gbbb3-Abbb4 Gbb1-Abb2 Gbb2-Abb3
      Gbb3-Abb4 Gb1-Ab2 Gb2-Ab3 Gb3-Ab4 G1-A2 G2-A3 G3-A4 G#1-A#2 G#2-A#3 G#3-A#4
      G##1-A##2 G##2-A##3 G##3-A##4 G###1-A###2 G###2-A###3 G###3-A###4))
    (14 "P8 + d3" "octave + diminished third"
     ((number Third) (quality Diminished) (additional_octaves 1))
     (Abb1-Cbbb3 Abb2-Cbbb4 Ab1-Cbb3 Ab2-Cbb4 A1-Cb3 A2-Cb4 A#1-C3 A#2-C4
      A##1-C#3 A##2-C#4 A###1-C##3 A###2-C##4 Bbb1-Dbbb3 Bbb2-Dbbb4 Bb1-Dbb3
      Bb2-Dbb4 B1-Db3 B2-Db4 B#1-D3 B#2-D4 B##1-D#3 B##2-D#4 B###1-D##3
      B###2-D##4 Cb1-Ebbb2 Cb2-Ebbb3 Cb3-Ebbb4 C1-Ebb2 C2-Ebb3 C3-Ebb4 C#1-Eb2
      C#2-Eb3 C#3-Eb4 C##1-E2 C##2-E3 C##3-E4 C###1-E#2 C###2-E#3 C###3-E#4
      Dbb1-Fbbb2 Dbb2-Fbbb3 Dbb3-Fbbb4 Db1-Fbb2 Db2-Fbb3 Db3-Fbb4 D1-Fb2 D2-Fb3
      D3-Fb4 D#1-F2 D#2-F3 D#3-F4 D##1-F#2 D##2-F#3 D##3-F#4 D###1-F##2
      D###2-F##3 D###3-F##4 Ebb1-Gbbb2 Ebb2-Gbbb3 Ebb3-Gbbb4 Eb1-Gbb2 Eb2-Gbb3
      Eb3-Gbb4 E1-Gb2 E2-Gb3 E3-Gb4 E#1-G2 E#2-G3 E#3-G4 E##1-G#2 E##2-G#3
      E##3-G#4 E###1-G##2 E###2-G##3 E###3-G##4 Fb1-Abbb2 Fb2-Abbb3 Fb3-Abbb4
      F1-Abb2 F2-Abb3 F3-Abb4 F#1-Ab2 F#2-Ab3 F#3-Ab4 F##1-A2 F##2-A3 F##3-A4
      F###1-A#2 F###2-A#3 F###3-A#4 Gb1-Bbbb2 Gb2-Bbbb3 Gb3-Bbbb4 G1-Bbb2 G2-Bbb3
      G3-Bbb4 G#1-Bb2 G#2-Bb3 G#3-Bb4 G##1-B2 G##2-B3 G##3-B4 G###1-B#2 G###2-B#3
      G###3-B#4))
    (15 "P8 + A2" "octave + augmented second"
     ((number Second) (quality Augmented) (additional_octaves 1))
     (Abbb1-Bbb2 Abbb2-Bbb3 Abbb3-Bbb4 Abb1-Bb2 Abb2-Bb3 Abb3-Bb4 Ab1-B2 Ab2-B3
      Ab3-B4 A1-B#2 A2-B#3 A3-B#4 A#1-B##2 A#2-B##3 A#3-B##4 A##1-B###2
      A##2-B###3 A##3-B###4 Bbbb1-Cb3 Bbbb2-Cb4 Bbb1-C3 Bbb2-C4 Bb1-C#3 Bb2-C#4
      B1-C##3 B2-C##4 B#1-C###3 B#2-C###4 Cbbb1-Dbb2 Cbbb2-Dbb3 Cbbb3-Dbb4
      Cbb1-Db2 Cbb2-Db3 Cbb3-Db4 Cb1-D2 Cb2-D3 Cb3-D4 C1-D#2 C2-D#3 C3-D#4
      C#1-D##2 C#2-D##3 C#3-D##4 C##1-D###2 C##2-D###3 C##3-D###4 Dbbb1-Ebb2
      Dbbb2-Ebb3 Dbbb3-Ebb4 Dbb1-Eb2 Dbb2-Eb3 Dbb3-Eb4 Db1-E2 Db2-E3 Db3-E4
      D1-E#2 D2-E#3 D3-E#4 D#1-E##2 D#2-E##3 D#3-E##4 D##1-E###2 D##2-E###3
      D##3-E###4 Ebbb1-Fb2 Ebbb2-Fb3 Ebbb3-Fb4 Ebb1-F2 Ebb2-F3 Ebb3-F4 Eb1-F#2
      Eb2-F#3 Eb3-F#4 E1-F##2 E2-F##3 E3-F##4 E#1-F###2 E#2-F###3 E#3-F###4
      Fbbb1-Gbb2 Fbbb2-Gbb3 Fbbb3-Gbb4 Fbb1-Gb2 Fbb2-Gb3 Fbb3-Gb4 Fb1-G2 Fb2-G3
      Fb3-G4 F1-G#2 F2-G#3 F3-G#4 F#1-G##2 F#2-G##3 F#3-G##4 F##1-G###2
      F##2-G###3 F##3-G###4 Gbbb1-Abb2 Gbbb2-Abb3 Gbbb3-Abb4 Gbb1-Ab2 Gbb2-Ab3
      Gbb3-Ab4 Gb1-A2 Gb2-A3 Gb3-A4 G1-A#2 G2-A#3 G3-A#4 G#1-A##2 G#2-A##3
      G#3-A##4 G##1-A###2 G##2-A###3 G##3-A###4))
    (15 "P8 + m3" "octave + minor third"
     ((number Third) (quality Minor) (additional_octaves 1))
     (Abbb1-Cbbb3 Abbb2-Cbbb4 Abb1-Cbb3 Abb2-Cbb4 Ab1-Cb3 Ab2-Cb4 A1-C3 A2-C4
      A#1-C#3 A#2-C#4 A##1-C##3 A##2-C##4 A###1-C###3 A###2-C###4 Bbbb1-Dbbb3
      Bbbb2-Dbbb4 Bbb1-Dbb3 Bbb2-Dbb4 Bb1-Db3 Bb2-Db4 B1-D3 B2-D4 B#1-D#3 B#2-D#4
      B##1-D##3 B##2-D##4 B###1-D###3 B###2-D###4 Cbb1-Ebbb2 Cbb2-Ebbb3
      Cbb3-Ebbb4 Cb1-Ebb2 Cb2-Ebb3 Cb3-Ebb4 C1-Eb2 C2-Eb3 C3-Eb4 C#1-E2 C#2-E3
      C#3-E4 C##1-E#2 C##2-E#3 C##3-E#4 C###1-E##2 C###2-E##3 C###3-E##4
      Dbbb1-Fbbb2 Dbbb2-Fbbb3 Dbbb3-Fbbb4 Dbb1-Fbb2 Dbb2-Fbb3 Dbb3-Fbb4 Db1-Fb2
      Db2-Fb3 Db3-Fb4 D1-F2 D2-F3 D3-F4 D#1-F#2 D#2-F#3 D#3-F#4 D##1-F##2
      D##2-F##3 D##3-F##4 D###1-F###2 D###2-F###3 D###3-F###4 Ebbb1-Gbbb2
      Ebbb2-Gbbb3 Ebbb3-Gbbb4 Ebb1-Gbb2 Ebb2-Gbb3 Ebb3-Gbb4 Eb1-Gb2 Eb2-Gb3
      Eb3-Gb4 E1-G2 E2-G3 E3-G4 E#1-G#2 E#2-G#3 E#3-G#4 E##1-G##2 E##2-G##3
      E##3-G##4 E###1-G###2 E###2-G###3 E###3-G###4 Fbb1-Abbb2 Fbb2-Abbb3
      Fbb3-Abbb4 Fb1-Abb2 Fb2-Abb3 Fb3-Abb4 F1-Ab2 F2-Ab3 F3-Ab4 F#1-A2 F#2-A3
      F#3-A4 F##1-A#2 F##2-A#3 F##3-A#4 F###1-A##2 F###2-A##3 F###3-A##4
      Gbb1-Bbbb2 Gbb2-Bbbb3 Gbb3-Bbbb4 Gb1-Bbb2 Gb2-Bbb3 Gb3-Bbb4 G1-Bb2 G2-Bb3
      G3-Bb4 G#1-B2 G#2-B3 G#3-B4 G##1-B#2 G##2-B#3 G##3-B#4 G###1-B##2
      G###2-B##3 G###3-B##4))
    (15 "P8 + dd4" "octave + doubly diminished fourth"
     ((number Fourth) (quality Doubly_diminished) (additional_octaves 1))
     (Ab1-Dbbb3 Ab2-Dbbb4 A1-Dbb3 A2-Dbb4 A#1-Db3 A#2-Db4 A##1-D3 A##2-D4
      A###1-D#3 A###2-D#4 Bb1-Ebbb3 Bb2-Ebbb4 B1-Ebb3 B2-Ebb4 B#1-Eb3 B#2-Eb4
      B##1-E3 B##2-E4 B###1-E#3 B###2-E#4 Cb1-Fbbb2 Cb2-Fbbb3 Cb3-Fbbb4 C1-Fbb2
      C2-Fbb3 C3-Fbb4 C#1-Fb2 C#2-Fb3 C#3-Fb4 C##1-F2 C##2-F3 C##3-F4 C###1-F#2
      C###2-F#3 C###3-F#4 Db1-Gbbb2 Db2-Gbbb3 Db3-Gbbb4 D1-Gbb2 D2-Gbb3 D3-Gbb4
      D#1-Gb2 D#2-Gb3 D#3-Gb4 D##1-G2 D##2-G3 D##3-G4 D###1-G#2 D###2-G#3
      D###3-G#4 Eb1-Abbb2 Eb2-Abbb3 Eb3-Abbb4 E1-Abb2 E2-Abb3 E3-Abb4 E#1-Ab2
      E#2-Ab3 E#3-Ab4 E##1-A2 E##2-A3 E##3-A4 E###1-A#2 E###2-A#3 E###3-A#4
      F1-Bbbb2 F2-Bbbb3 F3-Bbbb4 F#1-Bbb2 F#2-Bbb3 F#3-Bbb4 F##1-Bb2 F##2-Bb3
      F##3-Bb4 F###1-B2 F###2-B3 F###3-B4 Gb1-Cbbb3 Gb2-Cbbb4 G1-Cbb3 G2-Cbb4
      G#1-Cb3 G#2-Cb4 G##1-C3 G##2-C4 G###1-C#3 G###2-C#4))
    (16 "P8 + AA2" "octave + doubly augmented second"
     ((number Second) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-Bb2 Abbb2-Bb3 Abbb3-Bb4 Abb1-B2 Abb2-B3 Abb3-B4 Ab1-B#2 Ab2-B#3
      Ab3-B#4 A1-B##2 A2-B##3 A3-B##4 A#1-B###2 A#2-B###3 A#3-B###4 Bbbb1-C3
      Bbbb2-C4 Bbb1-C#3 Bbb2-C#4 Bb1-C##3 Bb2-C##4 B1-C###3 B2-C###4 Cbbb1-Db2
      Cbbb2-Db3 Cbbb3-Db4 Cbb1-D2 Cbb2-D3 Cbb3-D4 Cb1-D#2 Cb2-D#3 Cb3-D#4 C1-D##2
      C2-D##3 C3-D##4 C#1-D###2 C#2-D###3 C#3-D###4 Dbbb1-Eb2 Dbbb2-Eb3 Dbbb3-Eb4
      Dbb1-E2 Dbb2-E3 Dbb3-E4 Db1-E#2 Db2-E#3 Db3-E#4 D1-E##2 D2-E##3 D3-E##4
      D#1-E###2 D#2-E###3 D#3-E###4 Ebbb1-F2 Ebbb2-F3 Ebbb3-F4 Ebb1-F#2 Ebb2-F#3
      Ebb3-F#4 Eb1-F##2 Eb2-F##3 Eb3-F##4 E1-F###2 E2-F###3 E3-F###4 Fbbb1-Gb2
      Fbbb2-Gb3 Fbbb3-Gb4 Fbb1-G2 Fbb2-G3 Fbb3-G4 Fb1-G#2 Fb2-G#3 Fb3-G#4 F1-G##2
      F2-G##3 F3-G##4 F#1-G###2 F#2-G###3 F#3-G###4 Gbbb1-Ab2 Gbbb2-Ab3 Gbbb3-Ab4
      Gbb1-A2 Gbb2-A3 Gbb3-A4 Gb1-A#2 Gb2-A#3 Gb3-A#4 G1-A##2 G2-A##3 G3-A##4
      G#1-A###2 G#2-A###3 G#3-A###4))
    (16 "P8 + M3" "octave + major third"
     ((number Third) (quality Major) (additional_octaves 1))
     (Abbb1-Cbb3 Abbb2-Cbb4 Abb1-Cb3 Abb2-Cb4 Ab1-C3 Ab2-C4 A1-C#3 A2-C#4
      A#1-C##3 A#2-C##4 A##1-C###3 A##2-C###4 Bbbb1-Dbb3 Bbbb2-Dbb4 Bbb1-Db3
      Bbb2-Db4 Bb1-D3 Bb2-D4 B1-D#3 B2-D#4 B#1-D##3 B#2-D##4 B##1-D###3
      B##2-D###4 Cbbb1-Ebbb2 Cbbb2-Ebbb3 Cbbb3-Ebbb4 Cbb1-Ebb2 Cbb2-Ebb3
      Cbb3-Ebb4 Cb1-Eb2 Cb2-Eb3 Cb3-Eb4 C1-E2 C2-E3 C3-E4 C#1-E#2 C#2-E#3 C#3-E#4
      C##1-E##2 C##2-E##3 C##3-E##4 C###1-E###2 C###2-E###3 C###3-E###4
      Dbbb1-Fbb2 Dbbb2-Fbb3 Dbbb3-Fbb4 Dbb1-Fb2 Dbb2-Fb3 Dbb3-Fb4 Db1-F2 Db2-F3
      Db3-F4 D1-F#2 D2-F#3 D3-F#4 D#1-F##2 D#2-F##3 D#3-F##4 D##1-F###2
      D##2-F###3 D##3-F###4 Ebbb1-Gbb2 Ebbb2-Gbb3 Ebbb3-Gbb4 Ebb1-Gb2 Ebb2-Gb3
      Ebb3-Gb4 Eb1-G2 Eb2-G3 Eb3-G4 E1-G#2 E2-G#3 E3-G#4 E#1-G##2 E#2-G##3
      E#3-G##4 E##1-G###2 E##2-G###3 E##3-G###4 Fbbb1-Abbb2 Fbbb2-Abbb3
      Fbbb3-Abbb4 Fbb1-Abb2 Fbb2-Abb3 Fbb3-Abb4 Fb1-Ab2 Fb2-Ab3 Fb3-Ab4 F1-A2
      F2-A3 F3-A4 F#1-A#2 F#2-A#3 F#3-A#4 F##1-A##2 F##2-A##3 F##3-A##4
      F###1-A###2 F###2-A###3 F###3-A###4 Gbbb1-Bbbb2 Gbbb2-Bbbb3 Gbbb3-Bbbb4
      Gbb1-Bbb2 Gbb2-Bbb3 Gbb3-Bbb4 Gb1-Bb2 Gb2-Bb3 Gb3-Bb4 G1-B2 G2-B3 G3-B4
      G#1-B#2 G#2-B#3 G#3-B#4 G##1-B##2 G##2-B##3 G##3-B##4 G###1-B###2
      G###2-B###3 G###3-B###4))
    (16 "P8 + d4" "octave + diminished fourth"
     ((number Fourth) (quality Diminished) (additional_octaves 1))
     (Abb1-Dbbb3 Abb2-Dbbb4 Ab1-Dbb3 Ab2-Dbb4 A1-Db3 A2-Db4 A#1-D3 A#2-D4
      A##1-D#3 A##2-D#4 A###1-D##3 A###2-D##4 Bbb1-Ebbb3 Bbb2-Ebbb4 Bb1-Ebb3
      Bb2-Ebb4 B1-Eb3 B2-Eb4 B#1-E3 B#2-E4 B##1-E#3 B##2-E#4 B###1-E##3
      B###2-E##4 Cbb1-Fbbb2 Cbb2-Fbbb3 Cbb3-Fbbb4 Cb1-Fbb2 Cb2-Fbb3 Cb3-Fbb4
      C1-Fb2 C2-Fb3 C3-Fb4 C#1-F2 C#2-F3 C#3-F4 C##1-F#2 C##2-F#3 C##3-F#4
      C###1-F##2 C###2-F##3 C###3-F##4 Dbb1-Gbbb2 Dbb2-Gbbb3 Dbb3-Gbbb4 Db1-Gbb2
      Db2-Gbb3 Db3-Gbb4 D1-Gb2 D2-Gb3 D3-Gb4 D#1-G2 D#2-G3 D#3-G4 D##1-G#2
      D##2-G#3 D##3-G#4 D###1-G##2 D###2-G##3 D###3-G##4 Ebb1-Abbb2 Ebb2-Abbb3
      Ebb3-Abbb4 Eb1-Abb2 Eb2-Abb3 Eb3-Abb4 E1-Ab2 E2-Ab3 E3-Ab4 E#1-A2 E#2-A3
      E#3-A4 E##1-A#2 E##2-A#3 E##3-A#4 E###1-A##2 E###2-A##3 E###3-A##4
      Fb1-Bbbb2 Fb2-Bbbb3 Fb3-Bbbb4 F1-Bbb2 F2-Bbb3 F3-Bbb4 F#1-Bb2 F#2-Bb3
      F#3-Bb4 F##1-B2 F##2-B3 F##3-B4 F###1-B#2 F###2-B#3 F###3-B#4 Gbb1-Cbbb3
      Gbb2-Cbbb4 Gb1-Cbb3 Gb2-Cbb4 G1-Cb3 G2-Cb4 G#1-C3 G#2-C4 G##1-C#3 G##2-C#4
      G###1-C##3 G###2-C##4))
    (17 "P8 + A3" "octave + augmented third"
     ((number Third) (quality Augmented) (additional_octaves 1))
     (Abbb1-Cb3 Abbb2-Cb4 Abb1-C3 Abb2-C4 Ab1-C#3 Ab2-C#4 A1-C##3 A2-C##4
      A#1-C###3 A#2-C###4 Bbbb1-Db3 Bbbb2-Db4 Bbb1-D3 Bbb2-D4 Bb1-D#3 Bb2-D#4
      B1-D##3 B2-D##4 B#1-D###3 B#2-D###4 Cbbb1-Ebb2 Cbbb2-Ebb3 Cbbb3-Ebb4
      Cbb1-Eb2 Cbb2-Eb3 Cbb3-Eb4 Cb1-E2 Cb2-E3 Cb3-E4 C1-E#2 C2-E#3 C3-E#4
      C#1-E##2 C#2-E##3 C#3-E##4 C##1-E###2 C##2-E###3 C##3-E###4 Dbbb1-Fb2
      Dbbb2-Fb3 Dbbb3-Fb4 Dbb1-F2 Dbb2-F3 Dbb3-F4 Db1-F#2 Db2-F#3 Db3-F#4 D1-F##2
      D2-F##3 D3-F##4 D#1-F###2 D#2-F###3 D#3-F###4 Ebbb1-Gb2 Ebbb2-Gb3 Ebbb3-Gb4
      Ebb1-G2 Ebb2-G3 Ebb3-G4 Eb1-G#2 Eb2-G#3 Eb3-G#4 E1-G##2 E2-G##3 E3-G##4
      E#1-G###2 E#2-G###3 E#3-G###4 Fbbb1-Abb2 Fbbb2-Abb3 Fbbb3-Abb4 Fbb1-Ab2
      Fbb2-Ab3 Fbb3-Ab4 Fb1-A2 Fb2-A3 Fb3-A4 F1-A#2 F2-A#3 F3-A#4 F#1-A##2
      F#2-A##3 F#3-A##4 F##1-A###2 F##2-A###3 F##3-A###4 Gbbb1-Bbb2 Gbbb2-Bbb3
      Gbbb3-Bbb4 Gbb1-Bb2 Gbb2-Bb3 Gbb3-Bb4 Gb1-B2 Gb2-B3 Gb3-B4 G1-B#2 G2-B#3
      G3-B#4 G#1-B##2 G#2-B##3 G#3-B##4 G##1-B###2 G##2-B###3 G##3-B###4))
    (17 "P8 + P4" "octave + fourth"
     ((number Fourth) (quality Perfect) (additional_octaves 1))
     (Abbb1-Dbbb3 Abbb2-Dbbb4 Abb1-Dbb3 Abb2-Dbb4 Ab1-Db3 Ab2-Db4 A1-D3 A2-D4
      A#1-D#3 A#2-D#4 A##1-D##3 A##2-D##4 A###1-D###3 A###2-D###4 Bbbb1-Ebbb3
      Bbbb2-Ebbb4 Bbb1-Ebb3 Bbb2-Ebb4 Bb1-Eb3 Bb2-Eb4 B1-E3 B2-E4 B#1-E#3 B#2-E#4
      B##1-E##3 B##2-E##4 B###1-E###3 B###2-E###4 Cbbb1-Fbbb2 Cbbb2-Fbbb3
      Cbbb3-Fbbb4 Cbb1-Fbb2 Cbb2-Fbb3 Cbb3-Fbb4 Cb1-Fb2 Cb2-Fb3 Cb3-Fb4 C1-F2
      C2-F3 C3-F4 C#1-F#2 C#2-F#3 C#3-F#4 C##1-F##2 C##2-F##3 C##3-F##4
      C###1-F###2 C###2-F###3 C###3-F###4 Dbbb1-Gbbb2 Dbbb2-Gbbb3 Dbbb3-Gbbb4
      Dbb1-Gbb2 Dbb2-Gbb3 Dbb3-Gbb4 Db1-Gb2 Db2-Gb3 Db3-Gb4 D1-G2 D2-G3 D3-G4
      D#1-G#2 D#2-G#3 D#3-G#4 D##1-G##2 D##2-G##3 D##3-G##4 D###1-G###2
      D###2-G###3 D###3-G###4 Ebbb1-Abbb2 Ebbb2-Abbb3 Ebbb3-Abbb4 Ebb1-Abb2
      Ebb2-Abb3 Ebb3-Abb4 Eb1-Ab2 Eb2-Ab3 Eb3-Ab4 E1-A2 E2-A3 E3-A4 E#1-A#2
      E#2-A#3 E#3-A#4 E##1-A##2 E##2-A##3 E##3-A##4 E###1-A###2 E###2-A###3
      E###3-A###4 Fbb1-Bbbb2 Fbb2-Bbbb3 Fbb3-Bbbb4 Fb1-Bbb2 Fb2-Bbb3 Fb3-Bbb4
      F1-Bb2 F2-Bb3 F3-Bb4 F#1-B2 F#2-B3 F#3-B4 F##1-B#2 F##2-B#3 F##3-B#4
      F###1-B##2 F###2-B##3 F###3-B##4 Gbbb1-Cbbb3 Gbbb2-Cbbb4 Gbb1-Cbb3
      Gbb2-Cbb4 Gb1-Cb3 Gb2-Cb4 G1-C3 G2-C4 G#1-C#3 G#2-C#4 G##1-C##3 G##2-C##4
      G###1-C###3 G###2-C###4))
    (17 "P8 + dd5" "octave + doubly diminished fifth"
     ((number Fifth) (quality Doubly_diminished) (additional_octaves 1))
     (Ab1-Ebbb3 Ab2-Ebbb4 A1-Ebb3 A2-Ebb4 A#1-Eb3 A#2-Eb4 A##1-E3 A##2-E4
      A###1-E#3 A###2-E#4 Bbb1-Fbbb3 Bbb2-Fbbb4 Bb1-Fbb3 Bb2-Fbb4 B1-Fb3 B2-Fb4
      B#1-F3 B#2-F4 B##1-F#3 B##2-F#4 B###1-F##3 B###2-F##4 Cb1-Gbbb2 Cb2-Gbbb3
      Cb3-Gbbb4 C1-Gbb2 C2-Gbb3 C3-Gbb4 C#1-Gb2 C#2-Gb3 C#3-Gb4 C##1-G2 C##2-G3
      C##3-G4 C###1-G#2 C###2-G#3 C###3-G#4 Db1-Abbb2 Db2-Abbb3 Db3-Abbb4 D1-Abb2
      D2-Abb3 D3-Abb4 D#1-Ab2 D#2-Ab3 D#3-Ab4 D##1-A2 D##2-A3 D##3-A4 D###1-A#2
      D###2-A#3 D###3-A#4 Eb1-Bbbb2 Eb2-Bbbb3 Eb3-Bbbb4 E1-Bbb2 E2-Bbb3 E3-Bbb4
      E#1-Bb2 E#2-Bb3 E#3-Bb4 E##1-B2 E##2-B3 E##3-B4 E###1-B#2 E###2-B#3
      E###3-B#4 Fb1-Cbbb3 Fb2-Cbbb4 F1-Cbb3 F2-Cbb4 F#1-Cb3 F#2-Cb4 F##1-C3
      F##2-C4 F###1-C#3 F###2-C#4 Gb1-Dbbb3 Gb2-Dbbb4 G1-Dbb3 G2-Dbb4 G#1-Db3
      G#2-Db4 G##1-D3 G##2-D4 G###1-D#3 G###2-D#4))
    (18 "P8 + AA3" "octave + doubly augmented third"
     ((number Third) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-C3 Abbb2-C4 Abb1-C#3 Abb2-C#4 Ab1-C##3 Ab2-C##4 A1-C###3 A2-C###4
      Bbbb1-D3 Bbbb2-D4 Bbb1-D#3 Bbb2-D#4 Bb1-D##3 Bb2-D##4 B1-D###3 B2-D###4
      Cbbb1-Eb2 Cbbb2-Eb3 Cbbb3-Eb4 Cbb1-E2 Cbb2-E3 Cbb3-E4 Cb1-E#2 Cb2-E#3
      Cb3-E#4 C1-E##2 C2-E##3 C3-E##4 C#1-E###2 C#2-E###3 C#3-E###4 Dbbb1-F2
      Dbbb2-F3 Dbbb3-F4 Dbb1-F#2 Dbb2-F#3 Dbb3-F#4 Db1-F##2 Db2-F##3 Db3-F##4
      D1-F###2 D2-F###3 D3-F###4 Ebbb1-G2 Ebbb2-G3 Ebbb3-G4 Ebb1-G#2 Ebb2-G#3
      Ebb3-G#4 Eb1-G##2 Eb2-G##3 Eb3-G##4 E1-G###2 E2-G###3 E3-G###4 Fbbb1-Ab2
      Fbbb2-Ab3 Fbbb3-Ab4 Fbb1-A2 Fbb2-A3 Fbb3-A4 Fb1-A#2 Fb2-A#3 Fb3-A#4 F1-A##2
      F2-A##3 F3-A##4 F#1-A###2 F#2-A###3 F#3-A###4 Gbbb1-Bb2 Gbbb2-Bb3 Gbbb3-Bb4
      Gbb1-B2 Gbb2-B3 Gbb3-B4 Gb1-B#2 Gb2-B#3 Gb3-B#4 G1-B##2 G2-B##3 G3-B##4
      G#1-B###2 G#2-B###3 G#3-B###4))
    (18 "P8 + A4" "octave + augmented fourth"
     ((number Fourth) (quality Augmented) (additional_octaves 1))
     (Abbb1-Dbb3 Abbb2-Dbb4 Abb1-Db3 Abb2-Db4 Ab1-D3 Ab2-D4 A1-D#3 A2-D#4
      A#1-D##3 A#2-D##4 A##1-D###3 A##2-D###4 Bbbb1-Ebb3 Bbbb2-Ebb4 Bbb1-Eb3
      Bbb2-Eb4 Bb1-E3 Bb2-E4 B1-E#3 B2-E#4 B#1-E##3 B#2-E##4 B##1-E###3
      B##2-E###4 Cbbb1-Fbb2 Cbbb2-Fbb3 Cbbb3-Fbb4 Cbb1-Fb2 Cbb2-Fb3 Cbb3-Fb4
      Cb1-F2 Cb2-F3 Cb3-F4 C1-F#2 C2-F#3 C3-F#4 C#1-F##2 C#2-F##3 C#3-F##4
      C##1-F###2 C##2-F###3 C##3-F###4 Dbbb1-Gbb2 Dbbb2-Gbb3 Dbbb3-Gbb4 Dbb1-Gb2
      Dbb2-Gb3 Dbb3-Gb4 Db1-G2 Db2-G3 Db3-G4 D1-G#2 D2-G#3 D3-G#4 D#1-G##2
      D#2-G##3 D#3-G##4 D##1-G###2 D##2-G###3 D##3-G###4 Ebbb1-Abb2 Ebbb2-Abb3
      Ebbb3-Abb4 Ebb1-Ab2 Ebb2-Ab3 Ebb3-Ab4 Eb1-A2 Eb2-A3 Eb3-A4 E1-A#2 E2-A#3
      E3-A#4 E#1-A##2 E#2-A##3 E#3-A##4 E##1-A###2 E##2-A###3 E##3-A###4
      Fbbb1-Bbbb2 Fbbb2-Bbbb3 Fbbb3-Bbbb4 Fbb1-Bbb2 Fbb2-Bbb3 Fbb3-Bbb4 Fb1-Bb2
      Fb2-Bb3 Fb3-Bb4 F1-B2 F2-B3 F3-B4 F#1-B#2 F#2-B#3 F#3-B#4 F##1-B##2
      F##2-B##3 F##3-B##4 F###1-B###2 F###2-B###3 F###3-B###4 Gbbb1-Cbb3
      Gbbb2-Cbb4 Gbb1-Cb3 Gbb2-Cb4 Gb1-C3 Gb2-C4 G1-C#3 G2-C#4 G#1-C##3 G#2-C##4
      G##1-C###3 G##2-C###4))
    (18 "P8 + d5" "octave + diminished fifth"
     ((number Fifth) (quality Diminished) (additional_octaves 1))
     (Abb1-Ebbb3 Abb2-Ebbb4 Ab1-Ebb3 Ab2-Ebb4 A1-Eb3 A2-Eb4 A#1-E3 A#2-E4
      A##1-E#3 A##2-E#4 A###1-E##3 A###2-E##4 Bbbb1-Fbbb3 Bbbb2-Fbbb4 Bbb1-Fbb3
      Bbb2-Fbb4 Bb1-Fb3 Bb2-Fb4 B1-F3 B2-F4 B#1-F#3 B#2-F#4 B##1-F##3 B##2-F##4
      B###1-F###3 B###2-F###4 Cbb1-Gbbb2 Cbb2-Gbbb3 Cbb3-Gbbb4 Cb1-Gbb2 Cb2-Gbb3
      Cb3-Gbb4 C1-Gb2 C2-Gb3 C3-Gb4 C#1-G2 C#2-G3 C#3-G4 C##1-G#2 C##2-G#3
      C##3-G#4 C###1-G##2 C###2-G##3 C###3-G##4 Dbb1-Abbb2 Dbb2-Abbb3 Dbb3-Abbb4
      Db1-Abb2 Db2-Abb3 Db3-Abb4 D1-Ab2 D2-Ab3 D3-Ab4 D#1-A2 D#2-A3 D#3-A4
      D##1-A#2 D##2-A#3 D##3-A#4 D###1-A##2 D###2-A##3 D###3-A##4 Ebb1-Bbbb2
      Ebb2-Bbbb3 Ebb3-Bbbb4 Eb1-Bbb2 Eb2-Bbb3 Eb3-Bbb4 E1-Bb2 E2-Bb3 E3-Bb4
      E#1-B2 E#2-B3 E#3-B4 E##1-B#2 E##2-B#3 E##3-B#4 E###1-B##2 E###2-B##3
      E###3-B##4 Fbb1-Cbbb3 Fbb2-Cbbb4 Fb1-Cbb3 Fb2-Cbb4 F1-Cb3 F2-Cb4 F#1-C3
      F#2-C4 F##1-C#3 F##2-C#4 F###1-C##3 F###2-C##4 Gbb1-Dbbb3 Gbb2-Dbbb4
      Gb1-Dbb3 Gb2-Dbb4 G1-Db3 G2-Db4 G#1-D3 G#2-D4 G##1-D#3 G##2-D#4 G###1-D##3
      G###2-D##4))
    (18 "P8 + dd6" "octave + doubly diminished sixth"
     ((number Sixth) (quality Doubly_diminished) (additional_octaves 1))
     (Ab1-Fbbb3 Ab2-Fbbb4 A1-Fbb3 A2-Fbb4 A#1-Fb3 A#2-Fb4 A##1-F3 A##2-F4
      A###1-F#3 A###2-F#4 Bb1-Gbbb3 Bb2-Gbbb4 B1-Gbb3 B2-Gbb4 B#1-Gb3 B#2-Gb4
      B##1-G3 B##2-G4 B###1-G#3 B###2-G#4 C1-Abbb2 C2-Abbb3 C3-Abbb4 C#1-Abb2
      C#2-Abb3 C#3-Abb4 C##1-Ab2 C##2-Ab3 C##3-Ab4 C###1-A2 C###2-A3 C###3-A4
      D1-Bbbb2 D2-Bbbb3 D3-Bbbb4 D#1-Bbb2 D#2-Bbb3 D#3-Bbb4 D##1-Bb2 D##2-Bb3
      D##3-Bb4 D###1-B2 D###2-B3 D###3-B4 Eb1-Cbbb3 Eb2-Cbbb4 E1-Cbb3 E2-Cbb4
      E#1-Cb3 E#2-Cb4 E##1-C3 E##2-C4 E###1-C#3 E###2-C#4 F1-Dbbb3 F2-Dbbb4
      F#1-Dbb3 F#2-Dbb4 F##1-Db3 F##2-Db4 F###1-D3 F###2-D4 G1-Ebbb3 G2-Ebbb4
      G#1-Ebb3 G#2-Ebb4 G##1-Eb3 G##2-Eb4 G###1-E3 G###2-E4))
    (19 "P8 + AA4" "octave + doubly augmented fourth"
     ((number Fourth) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-Db3 Abbb2-Db4 Abb1-D3 Abb2-D4 Ab1-D#3 Ab2-D#4 A1-D##3 A2-D##4
      A#1-D###3 A#2-D###4 Bbbb1-Eb3 Bbbb2-Eb4 Bbb1-E3 Bbb2-E4 Bb1-E#3 Bb2-E#4
      B1-E##3 B2-E##4 B#1-E###3 B#2-E###4 Cbbb1-Fb2 Cbbb2-Fb3 Cbbb3-Fb4 Cbb1-F2
      Cbb2-F3 Cbb3-F4 Cb1-F#2 Cb2-F#3 Cb3-F#4 C1-F##2 C2-F##3 C3-F##4 C#1-F###2
      C#2-F###3 C#3-F###4 Dbbb1-Gb2 Dbbb2-Gb3 Dbbb3-Gb4 Dbb1-G2 Dbb2-G3 Dbb3-G4
      Db1-G#2 Db2-G#3 Db3-G#4 D1-G##2 D2-G##3 D3-G##4 D#1-G###2 D#2-G###3
      D#3-G###4 Ebbb1-Ab2 Ebbb2-Ab3 Ebbb3-Ab4 Ebb1-A2 Ebb2-A3 Ebb3-A4 Eb1-A#2
      Eb2-A#3 Eb3-A#4 E1-A##2 E2-A##3 E3-A##4 E#1-A###2 E#2-A###3 E#3-A###4
      Fbbb1-Bbb2 Fbbb2-Bbb3 Fbbb3-Bbb4 Fbb1-Bb2 Fbb2-Bb3 Fbb3-Bb4 Fb1-B2 Fb2-B3
      Fb3-B4 F1-B#2 F2-B#3 F3-B#4 F#1-B##2 F#2-B##3 F#3-B##4 F##1-B###2
      F##2-B###3 F##3-B###4 Gbbb1-Cb3 Gbbb2-Cb4 Gbb1-C3 Gbb2-C4 Gb1-C#3 Gb2-C#4
      G1-C##3 G2-C##4 G#1-C###3 G#2-C###4))
    (19 "P8 + P5" "octave + fifth"
     ((number Fifth) (quality Perfect) (additional_octaves 1))
     (Abbb1-Ebbb3 Abbb2-Ebbb4 Abb1-Ebb3 Abb2-Ebb4 Ab1-Eb3 Ab2-Eb4 A1-E3 A2-E4
      A#1-E#3 A#2-E#4 A##1-E##3 A##2-E##4 A###1-E###3 A###2-E###4 Bbbb1-Fbb3
      Bbbb2-Fbb4 Bbb1-Fb3 Bbb2-Fb4 Bb1-F3 Bb2-F4 B1-F#3 B2-F#4 B#1-F##3 B#2-F##4
      B##1-F###3 B##2-F###4 Cbbb1-Gbbb2 Cbbb2-Gbbb3 Cbbb3-Gbbb4 Cbb1-Gbb2
      Cbb2-Gbb3 Cbb3-Gbb4 Cb1-Gb2 Cb2-Gb3 Cb3-Gb4 C1-G2 C2-G3 C3-G4 C#1-G#2
      C#2-G#3 C#3-G#4 C##1-G##2 C##2-G##3 C##3-G##4 C###1-G###2 C###2-G###3
      C###3-G###4 Dbbb1-Abbb2 Dbbb2-Abbb3 Dbbb3-Abbb4 Dbb1-Abb2 Dbb2-Abb3
      Dbb3-Abb4 Db1-Ab2 Db2-Ab3 Db3-Ab4 D1-A2 D2-A3 D3-A4 D#1-A#2 D#2-A#3 D#3-A#4
      D##1-A##2 D##2-A##3 D##3-A##4 D###1-A###2 D###2-A###3 D###3-A###4
      Ebbb1-Bbbb2 Ebbb2-Bbbb3 Ebbb3-Bbbb4 Ebb1-Bbb2 Ebb2-Bbb3 Ebb3-Bbb4 Eb1-Bb2
      Eb2-Bb3 Eb3-Bb4 E1-B2 E2-B3 E3-B4 E#1-B#2 E#2-B#3 E#3-B#4 E##1-B##2
      E##2-B##3 E##3-B##4 E###1-B###2 E###2-B###3 E###3-B###4 Fbbb1-Cbbb3
      Fbbb2-Cbbb4 Fbb1-Cbb3 Fbb2-Cbb4 Fb1-Cb3 Fb2-Cb4 F1-C3 F2-C4 F#1-C#3 F#2-C#4
      F##1-C##3 F##2-C##4 F###1-C###3 F###2-C###4 Gbbb1-Dbbb3 Gbbb2-Dbbb4
      Gbb1-Dbb3 Gbb2-Dbb4 Gb1-Db3 Gb2-Db4 G1-D3 G2-D4 G#1-D#3 G#2-D#4 G##1-D##3
      G##2-D##4 G###1-D###3 G###2-D###4))
    (19 "P8 + d6" "octave + diminished sixth"
     ((number Sixth) (quality Diminished) (additional_octaves 1))
     (Abb1-Fbbb3 Abb2-Fbbb4 Ab1-Fbb3 Ab2-Fbb4 A1-Fb3 A2-Fb4 A#1-F3 A#2-F4
      A##1-F#3 A##2-F#4 A###1-F##3 A###2-F##4 Bbb1-Gbbb3 Bbb2-Gbbb4 Bb1-Gbb3
      Bb2-Gbb4 B1-Gb3 B2-Gb4 B#1-G3 B#2-G4 B##1-G#3 B##2-G#4 B###1-G##3
      B###2-G##4 Cb1-Abbb2 Cb2-Abbb3 Cb3-Abbb4 C1-Abb2 C2-Abb3 C3-Abb4 C#1-Ab2
      C#2-Ab3 C#3-Ab4 C##1-A2 C##2-A3 C##3-A4 C###1-A#2 C###2-A#3 C###3-A#4
      Db1-Bbbb2 Db2-Bbbb3 Db3-Bbbb4 D1-Bbb2 D2-Bbb3 D3-Bbb4 D#1-Bb2 D#2-Bb3
      D#3-Bb4 D##1-B2 D##2-B3 D##3-B4 D###1-B#2 D###2-B#3 D###3-B#4 Ebb1-Cbbb3
      Ebb2-Cbbb4 Eb1-Cbb3 Eb2-Cbb4 E1-Cb3 E2-Cb4 E#1-C3 E#2-C4 E##1-C#3 E##2-C#4
      E###1-C##3 E###2-C##4 Fb1-Dbbb3 Fb2-Dbbb4 F1-Dbb3 F2-Dbb4 F#1-Db3 F#2-Db4
      F##1-D3 F##2-D4 F###1-D#3 F###2-D#4 Gb1-Ebbb3 Gb2-Ebbb4 G1-Ebb3 G2-Ebb4
      G#1-Eb3 G#2-Eb4 G##1-E3 G##2-E4 G###1-E#3 G###2-E#4))
    (20 "P8 + A5" "octave + augmented fifth"
     ((number Fifth) (quality Augmented) (additional_octaves 1))
     (Abbb1-Ebb3 Abbb2-Ebb4 Abb1-Eb3 Abb2-Eb4 Ab1-E3 Ab2-E4 A1-E#3 A2-E#4
      A#1-E##3 A#2-E##4 A##1-E###3 A##2-E###4 Bbbb1-Fb3 Bbbb2-Fb4 Bbb1-F3 Bbb2-F4
      Bb1-F#3 Bb2-F#4 B1-F##3 B2-F##4 B#1-F###3 B#2-F###4 Cbbb1-Gbb2 Cbbb2-Gbb3
      Cbbb3-Gbb4 Cbb1-Gb2 Cbb2-Gb3 Cbb3-Gb4 Cb1-G2 Cb2-G3 Cb3-G4 C1-G#2 C2-G#3
      C3-G#4 C#1-G##2 C#2-G##3 C#3-G##4 C##1-G###2 C##2-G###3 C##3-G###4
      Dbbb1-Abb2 Dbbb2-Abb3 Dbbb3-Abb4 Dbb1-Ab2 Dbb2-Ab3 Dbb3-Ab4 Db1-A2 Db2-A3
      Db3-A4 D1-A#2 D2-A#3 D3-A#4 D#1-A##2 D#2-A##3 D#3-A##4 D##1-A###2
      D##2-A###3 D##3-A###4 Ebbb1-Bbb2 Ebbb2-Bbb3 Ebbb3-Bbb4 Ebb1-Bb2 Ebb2-Bb3
      Ebb3-Bb4 Eb1-B2 Eb2-B3 Eb3-B4 E1-B#2 E2-B#3 E3-B#4 E#1-B##2 E#2-B##3
      E#3-B##4 E##1-B###2 E##2-B###3 E##3-B###4 Fbbb1-Cbb3 Fbbb2-Cbb4 Fbb1-Cb3
      Fbb2-Cb4 Fb1-C3 Fb2-C4 F1-C#3 F2-C#4 F#1-C##3 F#2-C##4 F##1-C###3
      F##2-C###4 Gbbb1-Dbb3 Gbbb2-Dbb4 Gbb1-Db3 Gbb2-Db4 Gb1-D3 Gb2-D4 G1-D#3
      G2-D#4 G#1-D##3 G#2-D##4 G##1-D###3 G##2-D###4))
    (20 "P8 + m6" "octave + minor sixth"
     ((number Sixth) (quality Minor) (additional_octaves 1))
     (Abbb1-Fbbb3 Abbb2-Fbbb4 Abb1-Fbb3 Abb2-Fbb4 Ab1-Fb3 Ab2-Fb4 A1-F3 A2-F4
      A#1-F#3 A#2-F#4 A##1-F##3 A##2-F##4 A###1-F###3 A###2-F###4 Bbbb1-Gbbb3
      Bbbb2-Gbbb4 Bbb1-Gbb3 Bbb2-Gbb4 Bb1-Gb3 Bb2-Gb4 B1-G3 B2-G4 B#1-G#3 B#2-G#4
      B##1-G##3 B##2-G##4 B###1-G###3 B###2-G###4 Cbb1-Abbb2 Cbb2-Abbb3
      Cbb3-Abbb4 Cb1-Abb2 Cb2-Abb3 Cb3-Abb4 C1-Ab2 C2-Ab3 C3-Ab4 C#1-A2 C#2-A3
      C#3-A4 C##1-A#2 C##2-A#3 C##3-A#4 C###1-A##2 C###2-A##3 C###3-A##4
      Dbb1-Bbbb2 Dbb2-Bbbb3 Dbb3-Bbbb4 Db1-Bbb2 Db2-Bbb3 Db3-Bbb4 D1-Bb2 D2-Bb3
      D3-Bb4 D#1-B2 D#2-B3 D#3-B4 D##1-B#2 D##2-B#3 D##3-B#4 D###1-B##2
      D###2-B##3 D###3-B##4 Ebbb1-Cbbb3 Ebbb2-Cbbb4 Ebb1-Cbb3 Ebb2-Cbb4 Eb1-Cb3
      Eb2-Cb4 E1-C3 E2-C4 E#1-C#3 E#2-C#4 E##1-C##3 E##2-C##4 E###1-C###3
      E###2-C###4 Fbb1-Dbbb3 Fbb2-Dbbb4 Fb1-Dbb3 Fb2-Dbb4 F1-Db3 F2-Db4 F#1-D3
      F#2-D4 F##1-D#3 F##2-D#4 F###1-D##3 F###2-D##4 Gbb1-Ebbb3 Gbb2-Ebbb4
      Gb1-Ebb3 Gb2-Ebb4 G1-Eb3 G2-Eb4 G#1-E3 G#2-E4 G##1-E#3 G##2-E#4 G###1-E##3
      G###2-E##4))
    (20 "P8 + dd7" "octave + doubly diminished seventh"
     ((number Seventh) (quality Doubly_diminished) (additional_octaves 1))
     (Ab1-Gbbb3 Ab2-Gbbb4 A1-Gbb3 A2-Gbb4 A#1-Gb3 A#2-Gb4 A##1-G3 A##2-G4
      A###1-G#3 A###2-G#4 Bb1-Abbb3 Bb2-Abbb4 B1-Abb3 B2-Abb4 B#1-Ab3 B#2-Ab4
      B##1-A3 B##2-A4 B###1-A#3 B###2-A#4 C1-Bbbb2 C2-Bbbb3 C3-Bbbb4 C#1-Bbb2
      C#2-Bbb3 C#3-Bbb4 C##1-Bb2 C##2-Bb3 C##3-Bb4 C###1-B2 C###2-B3 C###3-B4
      Db1-Cbbb3 Db2-Cbbb4 D1-Cbb3 D2-Cbb4 D#1-Cb3 D#2-Cb4 D##1-C3 D##2-C4
      D###1-C#3 D###2-C#4 Eb1-Dbbb3 Eb2-Dbbb4 E1-Dbb3 E2-Dbb4 E#1-Db3 E#2-Db4
      E##1-D3 E##2-D4 E###1-D#3 E###2-D#4 F1-Ebbb3 F2-Ebbb4 F#1-Ebb3 F#2-Ebb4
      F##1-Eb3 F##2-Eb4 F###1-E3 F###2-E4 Gb1-Fbbb3 Gb2-Fbbb4 G1-Fbb3 G2-Fbb4
      G#1-Fb3 G#2-Fb4 G##1-F3 G##2-F4 G###1-F#3 G###2-F#4))
    (21 "P8 + AA5" "octave + doubly augmented fifth"
     ((number Fifth) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-Eb3 Abbb2-Eb4 Abb1-E3 Abb2-E4 Ab1-E#3 Ab2-E#4 A1-E##3 A2-E##4
      A#1-E###3 A#2-E###4 Bbbb1-F3 Bbbb2-F4 Bbb1-F#3 Bbb2-F#4 Bb1-F##3 Bb2-F##4
      B1-F###3 B2-F###4 Cbbb1-Gb2 Cbbb2-Gb3 Cbbb3-Gb4 Cbb1-G2 Cbb2-G3 Cbb3-G4
      Cb1-G#2 Cb2-G#3 Cb3-G#4 C1-G##2 C2-G##3 C3-G##4 C#1-G###2 C#2-G###3
      C#3-G###4 Dbbb1-Ab2 Dbbb2-Ab3 Dbbb3-Ab4 Dbb1-A2 Dbb2-A3 Dbb3-A4 Db1-A#2
      Db2-A#3 Db3-A#4 D1-A##2 D2-A##3 D3-A##4 D#1-A###2 D#2-A###3 D#3-A###4
      Ebbb1-Bb2 Ebbb2-Bb3 Ebbb3-Bb4 Ebb1-B2 Ebb2-B3 Ebb3-B4 Eb1-B#2 Eb2-B#3
      Eb3-B#4 E1-B##2 E2-B##3 E3-B##4 E#1-B###2 E#2-B###3 E#3-B###4 Fbbb1-Cb3
      Fbbb2-Cb4 Fbb1-C3 Fbb2-C4 Fb1-C#3 Fb2-C#4 F1-C##3 F2-C##4 F#1-C###3
      F#2-C###4 Gbbb1-Db3 Gbbb2-Db4 Gbb1-D3 Gbb2-D4 Gb1-D#3 Gb2-D#4 G1-D##3
      G2-D##4 G#1-D###3 G#2-D###4))
    (21 "P8 + M6" "octave + major sixth"
     ((number Sixth) (quality Major) (additional_octaves 1))
     (Abbb1-Fbb3 Abbb2-Fbb4 Abb1-Fb3 Abb2-Fb4 Ab1-F3 Ab2-F4 A1-F#3 A2-F#4
      A#1-F##3 A#2-F##4 A##1-F###3 A##2-F###4 Bbbb1-Gbb3 Bbbb2-Gbb4 Bbb1-Gb3
      Bbb2-Gb4 Bb1-G3 Bb2-G4 B1-G#3 B2-G#4 B#1-G##3 B#2-G##4 B##1-G###3
      B##2-G###4 Cbbb1-Abbb2 Cbbb2-Abbb3 Cbbb3-Abbb4 Cbb1-Abb2 Cbb2-Abb3
      Cbb3-Abb4 Cb1-Ab2 Cb2-Ab3 Cb3-Ab4 C1-A2 C2-A3 C3-A4 C#1-A#2 C#2-A#3 C#3-A#4
      C##1-A##2 C##2-A##3 C##3-A##4 C###1-A###2 C###2-A###3 C###3-A###4
      Dbbb1-Bbbb2 Dbbb2-Bbbb3 Dbbb3-Bbbb4 Dbb1-Bbb2 Dbb2-Bbb3 Dbb3-Bbb4 Db1-Bb2
      Db2-Bb3 Db3-Bb4 D1-B2 D2-B3 D3-B4 D#1-B#2 D#2-B#3 D#3-B#4 D##1-B##2
      D##2-B##3 D##3-B##4 D###1-B###2 D###2-B###3 D###3-B###4 Ebbb1-Cbb3
      Ebbb2-Cbb4 Ebb1-Cb3 Ebb2-Cb4 Eb1-C3 Eb2-C4 E1-C#3 E2-C#4 E#1-C##3 E#2-C##4
      E##1-C###3 E##2-C###4 Fbbb1-Dbbb3 Fbbb2-Dbbb4 Fbb1-Dbb3 Fbb2-Dbb4 Fb1-Db3
      Fb2-Db4 F1-D3 F2-D4 F#1-D#3 F#2-D#4 F##1-D##3 F##2-D##4 F###1-D###3
      F###2-D###4 Gbbb1-Ebbb3 Gbbb2-Ebbb4 Gbb1-Ebb3 Gbb2-Ebb4 Gb1-Eb3 Gb2-Eb4
      G1-E3 G2-E4 G#1-E#3 G#2-E#4 G##1-E##3 G##2-E##4 G###1-E###3 G###2-E###4))
    (21 "P8 + d7" "octave + diminished seventh"
     ((number Seventh) (quality Diminished) (additional_octaves 1))
     (Abb1-Gbbb3 Abb2-Gbbb4 Ab1-Gbb3 Ab2-Gbb4 A1-Gb3 A2-Gb4 A#1-G3 A#2-G4
      A##1-G#3 A##2-G#4 A###1-G##3 A###2-G##4 Bbb1-Abbb3 Bbb2-Abbb4 Bb1-Abb3
      Bb2-Abb4 B1-Ab3 B2-Ab4 B#1-A3 B#2-A4 B##1-A#3 B##2-A#4 B###1-A##3
      B###2-A##4 Cb1-Bbbb2 Cb2-Bbbb3 Cb3-Bbbb4 C1-Bbb2 C2-Bbb3 C3-Bbb4 C#1-Bb2
      C#2-Bb3 C#3-Bb4 C##1-B2 C##2-B3 C##3-B4 C###1-B#2 C###2-B#3 C###3-B#4
      Dbb1-Cbbb3 Dbb2-Cbbb4 Db1-Cbb3 Db2-Cbb4 D1-Cb3 D2-Cb4 D#1-C3 D#2-C4
      D##1-C#3 D##2-C#4 D###1-C##3 D###2-C##4 Ebb1-Dbbb3 Ebb2-Dbbb4 Eb1-Dbb3
      Eb2-Dbb4 E1-Db3 E2-Db4 E#1-D3 E#2-D4 E##1-D#3 E##2-D#4 E###1-D##3
      E###2-D##4 Fb1-Ebbb3 Fb2-Ebbb4 F1-Ebb3 F2-Ebb4 F#1-Eb3 F#2-Eb4 F##1-E3
      F##2-E4 F###1-E#3 F###2-E#4 Gbb1-Fbbb3 Gbb2-Fbbb4 Gb1-Fbb3 Gb2-Fbb4 G1-Fb3
      G2-Fb4 G#1-F3 G#2-F4 G##1-F#3 G##2-F#4 G###1-F##3 G###2-F##4))
    (22 "P8 + A6" "octave + augmented sixth"
     ((number Sixth) (quality Augmented) (additional_octaves 1))
     (Abbb1-Fb3 Abbb2-Fb4 Abb1-F3 Abb2-F4 Ab1-F#3 Ab2-F#4 A1-F##3 A2-F##4
      A#1-F###3 A#2-F###4 Bbbb1-Gb3 Bbbb2-Gb4 Bbb1-G3 Bbb2-G4 Bb1-G#3 Bb2-G#4
      B1-G##3 B2-G##4 B#1-G###3 B#2-G###4 Cbbb1-Abb2 Cbbb2-Abb3 Cbbb3-Abb4
      Cbb1-Ab2 Cbb2-Ab3 Cbb3-Ab4 Cb1-A2 Cb2-A3 Cb3-A4 C1-A#2 C2-A#3 C3-A#4
      C#1-A##2 C#2-A##3 C#3-A##4 C##1-A###2 C##2-A###3 C##3-A###4 Dbbb1-Bbb2
      Dbbb2-Bbb3 Dbbb3-Bbb4 Dbb1-Bb2 Dbb2-Bb3 Dbb3-Bb4 Db1-B2 Db2-B3 Db3-B4
      D1-B#2 D2-B#3 D3-B#4 D#1-B##2 D#2-B##3 D#3-B##4 D##1-B###2 D##2-B###3
      D##3-B###4 Ebbb1-Cb3 Ebbb2-Cb4 Ebb1-C3 Ebb2-C4 Eb1-C#3 Eb2-C#4 E1-C##3
      E2-C##4 E#1-C###3 E#2-C###4 Fbbb1-Dbb3 Fbbb2-Dbb4 Fbb1-Db3 Fbb2-Db4 Fb1-D3
      Fb2-D4 F1-D#3 F2-D#4 F#1-D##3 F#2-D##4 F##1-D###3 F##2-D###4 Gbbb1-Ebb3
      Gbbb2-Ebb4 Gbb1-Eb3 Gbb2-Eb4 Gb1-E3 Gb2-E4 G1-E#3 G2-E#4 G#1-E##3 G#2-E##4
      G##1-E###3 G##2-E###4))
    (22 "P8 + m7" "octave + minor seventh"
     ((number Seventh) (quality Minor) (additional_octaves 1))
     (Abbb1-Gbbb3 Abbb2-Gbbb4 Abb1-Gbb3 Abb2-Gbb4 Ab1-Gb3 Ab2-Gb4 A1-G3 A2-G4
      A#1-G#3 A#2-G#4 A##1-G##3 A##2-G##4 A###1-G###3 A###2-G###4 Bbbb1-Abbb3
      Bbbb2-Abbb4 Bbb1-Abb3 Bbb2-Abb4 Bb1-Ab3 Bb2-Ab4 B1-A3 B2-A4 B#1-A#3 B#2-A#4
      B##1-A##3 B##2-A##4 B###1-A###3 B###2-A###4 Cbb1-Bbbb2 Cbb2-Bbbb3
      Cbb3-Bbbb4 Cb1-Bbb2 Cb2-Bbb3 Cb3-Bbb4 C1-Bb2 C2-Bb3 C3-Bb4 C#1-B2 C#2-B3
      C#3-B4 C##1-B#2 C##2-B#3 C##3-B#4 C###1-B##2 C###2-B##3 C###3-B##4
      Dbbb1-Cbbb3 Dbbb2-Cbbb4 Dbb1-Cbb3 Dbb2-Cbb4 Db1-Cb3 Db2-Cb4 D1-C3 D2-C4
      D#1-C#3 D#2-C#4 D##1-C##3 D##2-C##4 D###1-C###3 D###2-C###4 Ebbb1-Dbbb3
      Ebbb2-Dbbb4 Ebb1-Dbb3 Ebb2-Dbb4 Eb1-Db3 Eb2-Db4 E1-D3 E2-D4 E#1-D#3 E#2-D#4
      E##1-D##3 E##2-D##4 E###1-D###3 E###2-D###4 Fbb1-Ebbb3 Fbb2-Ebbb4 Fb1-Ebb3
      Fb2-Ebb4 F1-Eb3 F2-Eb4 F#1-E3 F#2-E4 F##1-E#3 F##2-E#4 F###1-E##3
      F###2-E##4 Gbbb1-Fbbb3 Gbbb2-Fbbb4 Gbb1-Fbb3 Gbb2-Fbb4 Gb1-Fb3 Gb2-Fb4
      G1-F3 G2-F4 G#1-F#3 G#2-F#4 G##1-F##3 G##2-F##4 G###1-F###3 G###2-F###4))
    (23 "P8 + AA6" "octave + doubly augmented sixth"
     ((number Sixth) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-F3 Abbb2-F4 Abb1-F#3 Abb2-F#4 Ab1-F##3 Ab2-F##4 A1-F###3 A2-F###4
      Bbbb1-G3 Bbbb2-G4 Bbb1-G#3 Bbb2-G#4 Bb1-G##3 Bb2-G##4 B1-G###3 B2-G###4
      Cbbb1-Ab2 Cbbb2-Ab3 Cbbb3-Ab4 Cbb1-A2 Cbb2-A3 Cbb3-A4 Cb1-A#2 Cb2-A#3
      Cb3-A#4 C1-A##2 C2-A##3 C3-A##4 C#1-A###2 C#2-A###3 C#3-A###4 Dbbb1-Bb2
      Dbbb2-Bb3 Dbbb3-Bb4 Dbb1-B2 Dbb2-B3 Dbb3-B4 Db1-B#2 Db2-B#3 Db3-B#4 D1-B##2
      D2-B##3 D3-B##4 D#1-B###2 D#2-B###3 D#3-B###4 Ebbb1-C3 Ebbb2-C4 Ebb1-C#3
      Ebb2-C#4 Eb1-C##3 Eb2-C##4 E1-C###3 E2-C###4 Fbbb1-Db3 Fbbb2-Db4 Fbb1-D3
      Fbb2-D4 Fb1-D#3 Fb2-D#4 F1-D##3 F2-D##4 F#1-D###3 F#2-D###4 Gbbb1-Eb3
      Gbbb2-Eb4 Gbb1-E3 Gbb2-E4 Gb1-E#3 Gb2-E#4 G1-E##3 G2-E##4 G#1-E###3
      G#2-E###4))
    (23 "P8 + M7" "octave + major seventh"
     ((number Seventh) (quality Major) (additional_octaves 1))
     (Abbb1-Gbb3 Abbb2-Gbb4 Abb1-Gb3 Abb2-Gb4 Ab1-G3 Ab2-G4 A1-G#3 A2-G#4
      A#1-G##3 A#2-G##4 A##1-G###3 A##2-G###4 Bbbb1-Abb3 Bbbb2-Abb4 Bbb1-Ab3
      Bbb2-Ab4 Bb1-A3 Bb2-A4 B1-A#3 B2-A#4 B#1-A##3 B#2-A##4 B##1-A###3
      B##2-A###4 Cbbb1-Bbbb2 Cbbb2-Bbbb3 Cbbb3-Bbbb4 Cbb1-Bbb2 Cbb2-Bbb3
      Cbb3-Bbb4 Cb1-Bb2 Cb2-Bb3 Cb3-Bb4 C1-B2 C2-B3 C3-B4 C#1-B#2 C#2-B#3 C#3-B#4
      C##1-B##2 C##2-B##3 C##3-B##4 C###1-B###2 C###2-B###3 C###3-B###4
      Dbbb1-Cbb3 Dbbb2-Cbb4 Dbb1-Cb3 Dbb2-Cb4 Db1-C3 Db2-C4 D1-C#3 D2-C#4
      D#1-C##3 D#2-C##4 D##1-C###3 D##2-C###4 Ebbb1-Dbb3 Ebbb2-Dbb4 Ebb1-Db3
      Ebb2-Db4 Eb1-D3 Eb2-D4 E1-D#3 E2-D#4 E#1-D##3 E#2-D##4 E##1-D###3
      E##2-D###4 Fbbb1-Ebbb3 Fbbb2-Ebbb4 Fbb1-Ebb3 Fbb2-Ebb4 Fb1-Eb3 Fb2-Eb4
      F1-E3 F2-E4 F#1-E#3 F#2-E#4 F##1-E##3 F##2-E##4 F###1-E###3 F###2-E###4
      Gbbb1-Fbb3 Gbbb2-Fbb4 Gbb1-Fb3 Gbb2-Fb4 Gb1-F3 Gb2-F4 G1-F#3 G2-F#4
      G#1-F##3 G#2-F##4 G##1-F###3 G##2-F###4))
    (24 "2 P8" "2 octaves"
     ((number Unison) (quality Perfect) (additional_octaves 2))
     (Abbb1-Abbb3 Abbb2-Abbb4 Abb1-Abb3 Abb2-Abb4 Ab1-Ab3 Ab2-Ab4 A1-A3 A2-A4
      A#1-A#3 A#2-A#4 A##1-A##3 A##2-A##4 A###1-A###3 A###2-A###4 Bbbb1-Bbbb3
      Bbbb2-Bbbb4 Bbb1-Bbb3 Bbb2-Bbb4 Bb1-Bb3 Bb2-Bb4 B1-B3 B2-B4 B#1-B#3 B#2-B#4
      B##1-B##3 B##2-B##4 B###1-B###3 B###2-B###4 Cbbb1-Cbbb3 Cbbb2-Cbbb4
      Cbb1-Cbb3 Cbb2-Cbb4 Cb1-Cb3 Cb2-Cb4 C1-C3 C2-C4 C#1-C#3 C#2-C#4 C##1-C##3
      C##2-C##4 C###1-C###3 C###2-C###4 Dbbb1-Dbbb3 Dbbb2-Dbbb4 Dbb1-Dbb3
      Dbb2-Dbb4 Db1-Db3 Db2-Db4 D1-D3 D2-D4 D#1-D#3 D#2-D#4 D##1-D##3 D##2-D##4
      D###1-D###3 D###2-D###4 Ebbb1-Ebbb3 Ebbb2-Ebbb4 Ebb1-Ebb3 Ebb2-Ebb4 Eb1-Eb3
      Eb2-Eb4 E1-E3 E2-E4 E#1-E#3 E#2-E#4 E##1-E##3 E##2-E##4 E###1-E###3
      E###2-E###4 Fbbb1-Fbbb3 Fbbb2-Fbbb4 Fbb1-Fbb3 Fbb2-Fbb4 Fb1-Fb3 Fb2-Fb4
      F1-F3 F2-F4 F#1-F#3 F#2-F#4 F##1-F##3 F##2-F##4 F###1-F###3 F###2-F###4
      Gbbb1-Gbbb3 Gbbb2-Gbbb4 Gbb1-Gbb3 Gbb2-Gbb4 Gb1-Gb3 Gb2-Gb4 G1-G3 G2-G4
      G#1-G#3 G#2-G#4 G##1-G##3 G##2-G##4 G###1-G###3 G###2-G###4))
    (24 "2 P8 + d2" "2 octaves + diminished second"
     ((number Second) (quality Diminished) (additional_octaves 2))
     (Ab1-Bbbb3 Ab2-Bbbb4 A1-Bbb3 A2-Bbb4 A#1-Bb3 A#2-Bb4 A##1-B3 A##2-B4
      A###1-B#3 A###2-B#4 Bbb1-Cbbb4 Bb1-Cbb4 B1-Cb4 B#1-C4 B##1-C#4 B###1-C##4
      Cb1-Dbbb3 Cb2-Dbbb4 C1-Dbb3 C2-Dbb4 C#1-Db3 C#2-Db4 C##1-D3 C##2-D4
      C###1-D#3 C###2-D#4 Db1-Ebbb3 Db2-Ebbb4 D1-Ebb3 D2-Ebb4 D#1-Eb3 D#2-Eb4
      D##1-E3 D##2-E4 D###1-E#3 D###2-E#4 Ebb1-Fbbb3 Ebb2-Fbbb4 Eb1-Fbb3 Eb2-Fbb4
      E1-Fb3 E2-Fb4 E#1-F3 E#2-F4 E##1-F#3 E##2-F#4 E###1-F##3 E###2-F##4
      Fb1-Gbbb3 Fb2-Gbbb4 F1-Gbb3 F2-Gbb4 F#1-Gb3 F#2-Gb4 F##1-G3 F##2-G4
      F###1-G#3 F###2-G#4 Gb1-Abbb3 Gb2-Abbb4 G1-Abb3 G2-Abb4 G#1-Ab3 G#2-Ab4
      G##1-A3 G##2-A4 G###1-A#3 G###2-A#4))
    (24 "P8 + A7" "octave + augmented seventh"
     ((number Seventh) (quality Augmented) (additional_octaves 1))
     (Abbb1-Gb3 Abbb2-Gb4 Abb1-G3 Abb2-G4 Ab1-G#3 Ab2-G#4 A1-G##3 A2-G##4
      A#1-G###3 A#2-G###4 Bbbb1-Ab3 Bbbb2-Ab4 Bbb1-A3 Bbb2-A4 Bb1-A#3 Bb2-A#4
      B1-A##3 B2-A##4 B#1-A###3 B#2-A###4 Cbbb1-Bbb2 Cbbb2-Bbb3 Cbbb3-Bbb4
      Cbb1-Bb2 Cbb2-Bb3 Cbb3-Bb4 Cb1-B2 Cb2-B3 Cb3-B4 C1-B#2 C2-B#3 C3-B#4
      C#1-B##2 C#2-B##3 C#3-B##4 C##1-B###2 C##2-B###3 C##3-B###4 Dbbb1-Cb3
      Dbbb2-Cb4 Dbb1-C3 Dbb2-C4 Db1-C#3 Db2-C#4 D1-C##3 D2-C##4 D#1-C###3
      D#2-C###4 Ebbb1-Db3 Ebbb2-Db4 Ebb1-D3 Ebb2-D4 Eb1-D#3 Eb2-D#4 E1-D##3
      E2-D##4 E#1-D###3 E#2-D###4 Fbbb1-Ebb3 Fbbb2-Ebb4 Fbb1-Eb3 Fbb2-Eb4 Fb1-E3
      Fb2-E4 F1-E#3 F2-E#4 F#1-E##3 F#2-E##4 F##1-E###3 F##2-E###4 Gbbb1-Fb3
      Gbbb2-Fb4 Gbb1-F3 Gbb2-F4 Gb1-F#3 Gb2-F#4 G1-F##3 G2-F##4 G#1-F###3
      G#2-F###4))
    (25 "2 P8 + A1" "2 octaves + augmented unison"
     ((number Unison) (quality Augmented) (additional_octaves 2))
     (Abbb1-Abb3 Abbb2-Abb4 Abb1-Ab3 Abb2-Ab4 Ab1-A3 Ab2-A4 A1-A#3 A2-A#4
      A#1-A##3 A#2-A##4 A##1-A###3 A##2-A###4 Bbbb1-Bbb3 Bbbb2-Bbb4 Bbb1-Bb3
      Bbb2-Bb4 Bb1-B3 Bb2-B4 B1-B#3 B2-B#4 B#1-B##3 B#2-B##4 B##1-B###3
      B##2-B###4 Cbbb1-Cbb3 Cbbb2-Cbb4 Cbb1-Cb3 Cbb2-Cb4 Cb1-C3 Cb2-C4 C1-C#3
      C2-C#4 C#1-C##3 C#2-C##4 C##1-C###3 C##2-C###4 Dbbb1-Dbb3 Dbbb2-Dbb4
      Dbb1-Db3 Dbb2-Db4 Db1-D3 Db2-D4 D1-D#3 D2-D#4 D#1-D##3 D#2-D##4 D##1-D###3
      D##2-D###4 Ebbb1-Ebb3 Ebbb2-Ebb4 Ebb1-Eb3 Ebb2-Eb4 Eb1-E3 Eb2-E4 E1-E#3
      E2-E#4 E#1-E##3 E#2-E##4 E##1-E###3 E##2-E###4 Fbbb1-Fbb3 Fbbb2-Fbb4
      Fbb1-Fb3 Fbb2-Fb4 Fb1-F3 Fb2-F4 F1-F#3 F2-F#4 F#1-F##3 F#2-F##4 F##1-F###3
      F##2-F###4 Gbbb1-Gbb3 Gbbb2-Gbb4 Gbb1-Gb3 Gbb2-Gb4 Gb1-G3 Gb2-G4 G1-G#3
      G2-G#4 G#1-G##3 G#2-G##4 G##1-G###3 G##2-G###4))
    (25 "2 P8 + m2" "2 octaves + minor second"
     ((number Second) (quality Minor) (additional_octaves 2))
     (Abb1-Bbbb3 Abb2-Bbbb4 Ab1-Bbb3 Ab2-Bbb4 A1-Bb3 A2-Bb4 A#1-B3 A#2-B4
      A##1-B#3 A##2-B#4 A###1-B##3 A###2-B##4 Bbbb1-Cbbb4 Bbb1-Cbb4 Bb1-Cb4 B1-C4
      B#1-C#4 B##1-C##4 B###1-C###4 Cbb1-Dbbb3 Cbb2-Dbbb4 Cb1-Dbb3 Cb2-Dbb4
      C1-Db3 C2-Db4 C#1-D3 C#2-D4 C##1-D#3 C##2-D#4 C###1-D##3 C###2-D##4
      Dbb1-Ebbb3 Dbb2-Ebbb4 Db1-Ebb3 Db2-Ebb4 D1-Eb3 D2-Eb4 D#1-E3 D#2-E4
      D##1-E#3 D##2-E#4 D###1-E##3 D###2-E##4 Ebbb1-Fbbb3 Ebbb2-Fbbb4 Ebb1-Fbb3
      Ebb2-Fbb4 Eb1-Fb3 Eb2-Fb4 E1-F3 E2-F4 E#1-F#3 E#2-F#4 E##1-F##3 E##2-F##4
      E###1-F###3 E###2-F###4 Fbb1-Gbbb3 Fbb2-Gbbb4 Fb1-Gbb3 Fb2-Gbb4 F1-Gb3
      F2-Gb4 F#1-G3 F#2-G4 F##1-G#3 F##2-G#4 F###1-G##3 F###2-G##4 Gbb1-Abbb3
      Gbb2-Abbb4 Gb1-Abb3 Gb2-Abb4 G1-Ab3 G2-Ab4 G#1-A3 G#2-A4 G##1-A#3 G##2-A#4
      G###1-A##3 G###2-A##4))
    (25 "2 P8 + dd3" "2 octaves + doubly diminished third"
     ((number Third) (quality Doubly_diminished) (additional_octaves 2))
     (Ab1-Cbbb4 A1-Cbb4 A#1-Cb4 A##1-C4 A###1-C#4 Bb1-Dbbb4 B1-Dbb4 B#1-Db4
      B##1-D4 B###1-D#4 C1-Ebbb3 C2-Ebbb4 C#1-Ebb3 C#2-Ebb4 C##1-Eb3 C##2-Eb4
      C###1-E3 C###2-E4 Db1-Fbbb3 Db2-Fbbb4 D1-Fbb3 D2-Fbb4 D#1-Fb3 D#2-Fb4
      D##1-F3 D##2-F4 D###1-F#3 D###2-F#4 Eb1-Gbbb3 Eb2-Gbbb4 E1-Gbb3 E2-Gbb4
      E#1-Gb3 E#2-Gb4 E##1-G3 E##2-G4 E###1-G#3 E###2-G#4 F1-Abbb3 F2-Abbb4
      F#1-Abb3 F#2-Abb4 F##1-Ab3 F##2-Ab4 F###1-A3 F###2-A4 G1-Bbbb3 G2-Bbbb4
      G#1-Bbb3 G#2-Bbb4 G##1-Bb3 G##2-Bb4 G###1-B3 G###2-B4))
    (25 "P8 + AA7" "octave + doubly augmented seventh"
     ((number Seventh) (quality Doubly_augmented) (additional_octaves 1))
     (Abbb1-G3 Abbb2-G4 Abb1-G#3 Abb2-G#4 Ab1-G##3 Ab2-G##4 A1-G###3 A2-G###4
      Bbbb1-A3 Bbbb2-A4 Bbb1-A#3 Bbb2-A#4 Bb1-A##3 Bb2-A##4 B1-A###3 B2-A###4
      Cbbb1-Bb2 Cbbb2-Bb3 Cbbb3-Bb4 Cbb1-B2 Cbb2-B3 Cbb3-B4 Cb1-B#2 Cb2-B#3
      Cb3-B#4 C1-B##2 C2-B##3 C3-B##4 C#1-B###2 C#2-B###3 C#3-B###4 Dbbb1-C3
      Dbbb2-C4 Dbb1-C#3 Dbb2-C#4 Db1-C##3 Db2-C##4 D1-C###3 D2-C###4 Ebbb1-D3
      Ebbb2-D4 Ebb1-D#3 Ebb2-D#4 Eb1-D##3 Eb2-D##4 E1-D###3 E2-D###4 Fbbb1-Eb3
      Fbbb2-Eb4 Fbb1-E3 Fbb2-E4 Fb1-E#3 Fb2-E#4 F1-E##3 F2-E##4 F#1-E###3
      F#2-E###4 Gbbb1-F3 Gbbb2-F4 Gbb1-F#3 Gbb2-F#4 Gb1-F##3 Gb2-F##4 G1-F###3
      G2-F###4))
    (26 "2 P8 + AA1" "2 octaves + doubly augmented unison"
     ((number Unison) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-Ab3 Abbb2-Ab4 Abb1-A3 Abb2-A4 Ab1-A#3 Ab2-A#4 A1-A##3 A2-A##4
      A#1-A###3 A#2-A###4 Bbbb1-Bb3 Bbbb2-Bb4 Bbb1-B3 Bbb2-B4 Bb1-B#3 Bb2-B#4
      B1-B##3 B2-B##4 B#1-B###3 B#2-B###4 Cbbb1-Cb3 Cbbb2-Cb4 Cbb1-C3 Cbb2-C4
      Cb1-C#3 Cb2-C#4 C1-C##3 C2-C##4 C#1-C###3 C#2-C###4 Dbbb1-Db3 Dbbb2-Db4
      Dbb1-D3 Dbb2-D4 Db1-D#3 Db2-D#4 D1-D##3 D2-D##4 D#1-D###3 D#2-D###4
      Ebbb1-Eb3 Ebbb2-Eb4 Ebb1-E3 Ebb2-E4 Eb1-E#3 Eb2-E#4 E1-E##3 E2-E##4
      E#1-E###3 E#2-E###4 Fbbb1-Fb3 Fbbb2-Fb4 Fbb1-F3 Fbb2-F4 Fb1-F#3 Fb2-F#4
      F1-F##3 F2-F##4 F#1-F###3 F#2-F###4 Gbbb1-Gb3 Gbbb2-Gb4 Gbb1-G3 Gbb2-G4
      Gb1-G#3 Gb2-G#4 G1-G##3 G2-G##4 G#1-G###3 G#2-G###4))
    (26 "2 P8 + M2" "2 octaves + major second"
     ((number Second) (quality Major) (additional_octaves 2))
     (Abbb1-Bbbb3 Abbb2-Bbbb4 Abb1-Bbb3 Abb2-Bbb4 Ab1-Bb3 Ab2-Bb4 A1-B3 A2-B4
      A#1-B#3 A#2-B#4 A##1-B##3 A##2-B##4 A###1-B###3 A###2-B###4 Bbbb1-Cbb4
      Bbb1-Cb4 Bb1-C4 B1-C#4 B#1-C##4 B##1-C###4 Cbbb1-Dbbb3 Cbbb2-Dbbb4
      Cbb1-Dbb3 Cbb2-Dbb4 Cb1-Db3 Cb2-Db4 C1-D3 C2-D4 C#1-D#3 C#2-D#4 C##1-D##3
      C##2-D##4 C###1-D###3 C###2-D###4 Dbbb1-Ebbb3 Dbbb2-Ebbb4 Dbb1-Ebb3
      Dbb2-Ebb4 Db1-Eb3 Db2-Eb4 D1-E3 D2-E4 D#1-E#3 D#2-E#4 D##1-E##3 D##2-E##4
      D###1-E###3 D###2-E###4 Ebbb1-Fbb3 Ebbb2-Fbb4 Ebb1-Fb3 Ebb2-Fb4 Eb1-F3
      Eb2-F4 E1-F#3 E2-F#4 E#1-F##3 E#2-F##4 E##1-F###3 E##2-F###4 Fbbb1-Gbbb3
      Fbbb2-Gbbb4 Fbb1-Gbb3 Fbb2-Gbb4 Fb1-Gb3 Fb2-Gb4 F1-G3 F2-G4 F#1-G#3 F#2-G#4
      F##1-G##3 F##2-G##4 F###1-G###3 F###2-G###4 Gbbb1-Abbb3 Gbbb2-Abbb4
      Gbb1-Abb3 Gbb2-Abb4 Gb1-Ab3 Gb2-Ab4 G1-A3 G2-A4 G#1-A#3 G#2-A#4 G##1-A##3
      G##2-A##4 G###1-A###3 G###2-A###4))
    (26 "2 P8 + d3" "2 octaves + diminished third"
     ((number Third) (quality Diminished) (additional_octaves 2))
     (Abb1-Cbbb4 Ab1-Cbb4 A1-Cb4 A#1-C4 A##1-C#4 A###1-C##4 Bbb1-Dbbb4 Bb1-Dbb4
      B1-Db4 B#1-D4 B##1-D#4 B###1-D##4 Cb1-Ebbb3 Cb2-Ebbb4 C1-Ebb3 C2-Ebb4
      C#1-Eb3 C#2-Eb4 C##1-E3 C##2-E4 C###1-E#3 C###2-E#4 Dbb1-Fbbb3 Dbb2-Fbbb4
      Db1-Fbb3 Db2-Fbb4 D1-Fb3 D2-Fb4 D#1-F3 D#2-F4 D##1-F#3 D##2-F#4 D###1-F##3
      D###2-F##4 Ebb1-Gbbb3 Ebb2-Gbbb4 Eb1-Gbb3 Eb2-Gbb4 E1-Gb3 E2-Gb4 E#1-G3
      E#2-G4 E##1-G#3 E##2-G#4 E###1-G##3 E###2-G##4 Fb1-Abbb3 Fb2-Abbb4 F1-Abb3
      F2-Abb4 F#1-Ab3 F#2-Ab4 F##1-A3 F##2-A4 F###1-A#3 F###2-A#4 Gb1-Bbbb3
      Gb2-Bbbb4 G1-Bbb3 G2-Bbb4 G#1-Bb3 G#2-Bb4 G##1-B3 G##2-B4 G###1-B#3
      G###2-B#4))
    (27 "2 P8 + A2" "2 octaves + augmented second"
     ((number Second) (quality Augmented) (additional_octaves 2))
     (Abbb1-Bbb3 Abbb2-Bbb4 Abb1-Bb3 Abb2-Bb4 Ab1-B3 Ab2-B4 A1-B#3 A2-B#4
      A#1-B##3 A#2-B##4 A##1-B###3 A##2-B###4 Bbbb1-Cb4 Bbb1-C4 Bb1-C#4 B1-C##4
      B#1-C###4 Cbbb1-Dbb3 Cbbb2-Dbb4 Cbb1-Db3 Cbb2-Db4 Cb1-D3 Cb2-D4 C1-D#3
      C2-D#4 C#1-D##3 C#2-D##4 C##1-D###3 C##2-D###4 Dbbb1-Ebb3 Dbbb2-Ebb4
      Dbb1-Eb3 Dbb2-Eb4 Db1-E3 Db2-E4 D1-E#3 D2-E#4 D#1-E##3 D#2-E##4 D##1-E###3
      D##2-E###4 Ebbb1-Fb3 Ebbb2-Fb4 Ebb1-F3 Ebb2-F4 Eb1-F#3 Eb2-F#4 E1-F##3
      E2-F##4 E#1-F###3 E#2-F###4 Fbbb1-Gbb3 Fbbb2-Gbb4 Fbb1-Gb3 Fbb2-Gb4 Fb1-G3
      Fb2-G4 F1-G#3 F2-G#4 F#1-G##3 F#2-G##4 F##1-G###3 F##2-G###4 Gbbb1-Abb3
      Gbbb2-Abb4 Gbb1-Ab3 Gbb2-Ab4 Gb1-A3 Gb2-A4 G1-A#3 G2-A#4 G#1-A##3 G#2-A##4
      G##1-A###3 G##2-A###4))
    (27 "2 P8 + m3" "2 octaves + minor third"
     ((number Third) (quality Minor) (additional_octaves 2))
     (Abbb1-Cbbb4 Abb1-Cbb4 Ab1-Cb4 A1-C4 A#1-C#4 A##1-C##4 A###1-C###4
      Bbbb1-Dbbb4 Bbb1-Dbb4 Bb1-Db4 B1-D4 B#1-D#4 B##1-D##4 B###1-D###4
      Cbb1-Ebbb3 Cbb2-Ebbb4 Cb1-Ebb3 Cb2-Ebb4 C1-Eb3 C2-Eb4 C#1-E3 C#2-E4
      C##1-E#3 C##2-E#4 C###1-E##3 C###2-E##4 Dbbb1-Fbbb3 Dbbb2-Fbbb4 Dbb1-Fbb3
      Dbb2-Fbb4 Db1-Fb3 Db2-Fb4 D1-F3 D2-F4 D#1-F#3 D#2-F#4 D##1-F##3 D##2-F##4
      D###1-F###3 D###2-F###4 Ebbb1-Gbbb3 Ebbb2-Gbbb4 Ebb1-Gbb3 Ebb2-Gbb4 Eb1-Gb3
      Eb2-Gb4 E1-G3 E2-G4 E#1-G#3 E#2-G#4 E##1-G##3 E##2-G##4 E###1-G###3
      E###2-G###4 Fbb1-Abbb3 Fbb2-Abbb4 Fb1-Abb3 Fb2-Abb4 F1-Ab3 F2-Ab4 F#1-A3
      F#2-A4 F##1-A#3 F##2-A#4 F###1-A##3 F###2-A##4 Gbb1-Bbbb3 Gbb2-Bbbb4
      Gb1-Bbb3 Gb2-Bbb4 G1-Bb3 G2-Bb4 G#1-B3 G#2-B4 G##1-B#3 G##2-B#4 G###1-B##3
      G###2-B##4))
    (27 "2 P8 + dd4" "2 octaves + doubly diminished fourth"
     ((number Fourth) (quality Doubly_diminished) (additional_octaves 2))
     (Ab1-Dbbb4 A1-Dbb4 A#1-Db4 A##1-D4 A###1-D#4 Bb1-Ebbb4 B1-Ebb4 B#1-Eb4
      B##1-E4 B###1-E#4 Cb1-Fbbb3 Cb2-Fbbb4 C1-Fbb3 C2-Fbb4 C#1-Fb3 C#2-Fb4
      C##1-F3 C##2-F4 C###1-F#3 C###2-F#4 Db1-Gbbb3 Db2-Gbbb4 D1-Gbb3 D2-Gbb4
      D#1-Gb3 D#2-Gb4 D##1-G3 D##2-G4 D###1-G#3 D###2-G#4 Eb1-Abbb3 Eb2-Abbb4
      E1-Abb3 E2-Abb4 E#1-Ab3 E#2-Ab4 E##1-A3 E##2-A4 E###1-A#3 E###2-A#4
      F1-Bbbb3 F2-Bbbb4 F#1-Bbb3 F#2-Bbb4 F##1-Bb3 F##2-Bb4 F###1-B3 F###2-B4
      Gb1-Cbbb4 G1-Cbb4 G#1-Cb4 G##1-C4 G###1-C#4))
    (28 "2 P8 + AA2" "2 octaves + doubly augmented second"
     ((number Second) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-Bb3 Abbb2-Bb4 Abb1-B3 Abb2-B4 Ab1-B#3 Ab2-B#4 A1-B##3 A2-B##4
      A#1-B###3 A#2-B###4 Bbbb1-C4 Bbb1-C#4 Bb1-C##4 B1-C###4 Cbbb1-Db3 Cbbb2-Db4
      Cbb1-D3 Cbb2-D4 Cb1-D#3 Cb2-D#4 C1-D##3 C2-D##4 C#1-D###3 C#2-D###4
      Dbbb1-Eb3 Dbbb2-Eb4 Dbb1-E3 Dbb2-E4 Db1-E#3 Db2-E#4 D1-E##3 D2-E##4
      D#1-E###3 D#2-E###4 Ebbb1-F3 Ebbb2-F4 Ebb1-F#3 Ebb2-F#4 Eb1-F##3 Eb2-F##4
      E1-F###3 E2-F###4 Fbbb1-Gb3 Fbbb2-Gb4 Fbb1-G3 Fbb2-G4 Fb1-G#3 Fb2-G#4
      F1-G##3 F2-G##4 F#1-G###3 F#2-G###4 Gbbb1-Ab3 Gbbb2-Ab4 Gbb1-A3 Gbb2-A4
      Gb1-A#3 Gb2-A#4 G1-A##3 G2-A##4 G#1-A###3 G#2-A###4))
    (28 "2 P8 + M3" "2 octaves + major third"
     ((number Third) (quality Major) (additional_octaves 2))
     (Abbb1-Cbb4 Abb1-Cb4 Ab1-C4 A1-C#4 A#1-C##4 A##1-C###4 Bbbb1-Dbb4 Bbb1-Db4
      Bb1-D4 B1-D#4 B#1-D##4 B##1-D###4 Cbbb1-Ebbb3 Cbbb2-Ebbb4 Cbb1-Ebb3
      Cbb2-Ebb4 Cb1-Eb3 Cb2-Eb4 C1-E3 C2-E4 C#1-E#3 C#2-E#4 C##1-E##3 C##2-E##4
      C###1-E###3 C###2-E###4 Dbbb1-Fbb3 Dbbb2-Fbb4 Dbb1-Fb3 Dbb2-Fb4 Db1-F3
      Db2-F4 D1-F#3 D2-F#4 D#1-F##3 D#2-F##4 D##1-F###3 D##2-F###4 Ebbb1-Gbb3
      Ebbb2-Gbb4 Ebb1-Gb3 Ebb2-Gb4 Eb1-G3 Eb2-G4 E1-G#3 E2-G#4 E#1-G##3 E#2-G##4
      E##1-G###3 E##2-G###4 Fbbb1-Abbb3 Fbbb2-Abbb4 Fbb1-Abb3 Fbb2-Abb4 Fb1-Ab3
      Fb2-Ab4 F1-A3 F2-A4 F#1-A#3 F#2-A#4 F##1-A##3 F##2-A##4 F###1-A###3
      F###2-A###4 Gbbb1-Bbbb3 Gbbb2-Bbbb4 Gbb1-Bbb3 Gbb2-Bbb4 Gb1-Bb3 Gb2-Bb4
      G1-B3 G2-B4 G#1-B#3 G#2-B#4 G##1-B##3 G##2-B##4 G###1-B###3 G###2-B###4))
    (28 "2 P8 + d4" "2 octaves + diminished fourth"
     ((number Fourth) (quality Diminished) (additional_octaves 2))
     (Abb1-Dbbb4 Ab1-Dbb4 A1-Db4 A#1-D4 A##1-D#4 A###1-D##4 Bbb1-Ebbb4 Bb1-Ebb4
      B1-Eb4 B#1-E4 B##1-E#4 B###1-E##4 Cbb1-Fbbb3 Cbb2-Fbbb4 Cb1-Fbb3 Cb2-Fbb4
      C1-Fb3 C2-Fb4 C#1-F3 C#2-F4 C##1-F#3 C##2-F#4 C###1-F##3 C###2-F##4
      Dbb1-Gbbb3 Dbb2-Gbbb4 Db1-Gbb3 Db2-Gbb4 D1-Gb3 D2-Gb4 D#1-G3 D#2-G4
      D##1-G#3 D##2-G#4 D###1-G##3 D###2-G##4 Ebb1-Abbb3 Ebb2-Abbb4 Eb1-Abb3
      Eb2-Abb4 E1-Ab3 E2-Ab4 E#1-A3 E#2-A4 E##1-A#3 E##2-A#4 E###1-A##3
      E###2-A##4 Fb1-Bbbb3 Fb2-Bbbb4 F1-Bbb3 F2-Bbb4 F#1-Bb3 F#2-Bb4 F##1-B3
      F##2-B4 F###1-B#3 F###2-B#4 Gbb1-Cbbb4 Gb1-Cbb4 G1-Cb4 G#1-C4 G##1-C#4
      G###1-C##4))
    (29 "2 P8 + A3" "2 octaves + augmented third"
     ((number Third) (quality Augmented) (additional_octaves 2))
     (Abbb1-Cb4 Abb1-C4 Ab1-C#4 A1-C##4 A#1-C###4 Bbbb1-Db4 Bbb1-D4 Bb1-D#4
      B1-D##4 B#1-D###4 Cbbb1-Ebb3 Cbbb2-Ebb4 Cbb1-Eb3 Cbb2-Eb4 Cb1-E3 Cb2-E4
      C1-E#3 C2-E#4 C#1-E##3 C#2-E##4 C##1-E###3 C##2-E###4 Dbbb1-Fb3 Dbbb2-Fb4
      Dbb1-F3 Dbb2-F4 Db1-F#3 Db2-F#4 D1-F##3 D2-F##4 D#1-F###3 D#2-F###4
      Ebbb1-Gb3 Ebbb2-Gb4 Ebb1-G3 Ebb2-G4 Eb1-G#3 Eb2-G#4 E1-G##3 E2-G##4
      E#1-G###3 E#2-G###4 Fbbb1-Abb3 Fbbb2-Abb4 Fbb1-Ab3 Fbb2-Ab4 Fb1-A3 Fb2-A4
      F1-A#3 F2-A#4 F#1-A##3 F#2-A##4 F##1-A###3 F##2-A###4 Gbbb1-Bbb3 Gbbb2-Bbb4
      Gbb1-Bb3 Gbb2-Bb4 Gb1-B3 Gb2-B4 G1-B#3 G2-B#4 G#1-B##3 G#2-B##4 G##1-B###3
      G##2-B###4))
    (29 "2 P8 + P4" "2 octaves + fourth"
     ((number Fourth) (quality Perfect) (additional_octaves 2))
     (Abbb1-Dbbb4 Abb1-Dbb4 Ab1-Db4 A1-D4 A#1-D#4 A##1-D##4 A###1-D###4
      Bbbb1-Ebbb4 Bbb1-Ebb4 Bb1-Eb4 B1-E4 B#1-E#4 B##1-E##4 B###1-E###4
      Cbbb1-Fbbb3 Cbbb2-Fbbb4 Cbb1-Fbb3 Cbb2-Fbb4 Cb1-Fb3 Cb2-Fb4 C1-F3 C2-F4
      C#1-F#3 C#2-F#4 C##1-F##3 C##2-F##4 C###1-F###3 C###2-F###4 Dbbb1-Gbbb3
      Dbbb2-Gbbb4 Dbb1-Gbb3 Dbb2-Gbb4 Db1-Gb3 Db2-Gb4 D1-G3 D2-G4 D#1-G#3 D#2-G#4
      D##1-G##3 D##2-G##4 D###1-G###3 D###2-G###4 Ebbb1-Abbb3 Ebbb2-Abbb4
      Ebb1-Abb3 Ebb2-Abb4 Eb1-Ab3 Eb2-Ab4 E1-A3 E2-A4 E#1-A#3 E#2-A#4 E##1-A##3
      E##2-A##4 E###1-A###3 E###2-A###4 Fbb1-Bbbb3 Fbb2-Bbbb4 Fb1-Bbb3 Fb2-Bbb4
      F1-Bb3 F2-Bb4 F#1-B3 F#2-B4 F##1-B#3 F##2-B#4 F###1-B##3 F###2-B##4
      Gbbb1-Cbbb4 Gbb1-Cbb4 Gb1-Cb4 G1-C4 G#1-C#4 G##1-C##4 G###1-C###4))
    (29 "2 P8 + dd5" "2 octaves + doubly diminished fifth"
     ((number Fifth) (quality Doubly_diminished) (additional_octaves 2))
     (Ab1-Ebbb4 A1-Ebb4 A#1-Eb4 A##1-E4 A###1-E#4 Bbb1-Fbbb4 Bb1-Fbb4 B1-Fb4
      B#1-F4 B##1-F#4 B###1-F##4 Cb1-Gbbb3 Cb2-Gbbb4 C1-Gbb3 C2-Gbb4 C#1-Gb3
      C#2-Gb4 C##1-G3 C##2-G4 C###1-G#3 C###2-G#4 Db1-Abbb3 Db2-Abbb4 D1-Abb3
      D2-Abb4 D#1-Ab3 D#2-Ab4 D##1-A3 D##2-A4 D###1-A#3 D###2-A#4 Eb1-Bbbb3
      Eb2-Bbbb4 E1-Bbb3 E2-Bbb4 E#1-Bb3 E#2-Bb4 E##1-B3 E##2-B4 E###1-B#3
      E###2-B#4 Fb1-Cbbb4 F1-Cbb4 F#1-Cb4 F##1-C4 F###1-C#4 Gb1-Dbbb4 G1-Dbb4
      G#1-Db4 G##1-D4 G###1-D#4))
    (30 "2 P8 + AA3" "2 octaves + doubly augmented third"
     ((number Third) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-C4 Abb1-C#4 Ab1-C##4 A1-C###4 Bbbb1-D4 Bbb1-D#4 Bb1-D##4 B1-D###4
      Cbbb1-Eb3 Cbbb2-Eb4 Cbb1-E3 Cbb2-E4 Cb1-E#3 Cb2-E#4 C1-E##3 C2-E##4
      C#1-E###3 C#2-E###4 Dbbb1-F3 Dbbb2-F4 Dbb1-F#3 Dbb2-F#4 Db1-F##3 Db2-F##4
      D1-F###3 D2-F###4 Ebbb1-G3 Ebbb2-G4 Ebb1-G#3 Ebb2-G#4 Eb1-G##3 Eb2-G##4
      E1-G###3 E2-G###4 Fbbb1-Ab3 Fbbb2-Ab4 Fbb1-A3 Fbb2-A4 Fb1-A#3 Fb2-A#4
      F1-A##3 F2-A##4 F#1-A###3 F#2-A###4 Gbbb1-Bb3 Gbbb2-Bb4 Gbb1-B3 Gbb2-B4
      Gb1-B#3 Gb2-B#4 G1-B##3 G2-B##4 G#1-B###3 G#2-B###4))
    (30 "2 P8 + A4" "2 octaves + augmented fourth"
     ((number Fourth) (quality Augmented) (additional_octaves 2))
     (Abbb1-Dbb4 Abb1-Db4 Ab1-D4 A1-D#4 A#1-D##4 A##1-D###4 Bbbb1-Ebb4 Bbb1-Eb4
      Bb1-E4 B1-E#4 B#1-E##4 B##1-E###4 Cbbb1-Fbb3 Cbbb2-Fbb4 Cbb1-Fb3 Cbb2-Fb4
      Cb1-F3 Cb2-F4 C1-F#3 C2-F#4 C#1-F##3 C#2-F##4 C##1-F###3 C##2-F###4
      Dbbb1-Gbb3 Dbbb2-Gbb4 Dbb1-Gb3 Dbb2-Gb4 Db1-G3 Db2-G4 D1-G#3 D2-G#4
      D#1-G##3 D#2-G##4 D##1-G###3 D##2-G###4 Ebbb1-Abb3 Ebbb2-Abb4 Ebb1-Ab3
      Ebb2-Ab4 Eb1-A3 Eb2-A4 E1-A#3 E2-A#4 E#1-A##3 E#2-A##4 E##1-A###3
      E##2-A###4 Fbbb1-Bbbb3 Fbbb2-Bbbb4 Fbb1-Bbb3 Fbb2-Bbb4 Fb1-Bb3 Fb2-Bb4
      F1-B3 F2-B4 F#1-B#3 F#2-B#4 F##1-B##3 F##2-B##4 F###1-B###3 F###2-B###4
      Gbbb1-Cbb4 Gbb1-Cb4 Gb1-C4 G1-C#4 G#1-C##4 G##1-C###4))
    (30 "2 P8 + d5" "2 octaves + diminished fifth"
     ((number Fifth) (quality Diminished) (additional_octaves 2))
     (Abb1-Ebbb4 Ab1-Ebb4 A1-Eb4 A#1-E4 A##1-E#4 A###1-E##4 Bbbb1-Fbbb4 Bbb1-Fbb4
      Bb1-Fb4 B1-F4 B#1-F#4 B##1-F##4 B###1-F###4 Cbb1-Gbbb3 Cbb2-Gbbb4 Cb1-Gbb3
      Cb2-Gbb4 C1-Gb3 C2-Gb4 C#1-G3 C#2-G4 C##1-G#3 C##2-G#4 C###1-G##3
      C###2-G##4 Dbb1-Abbb3 Dbb2-Abbb4 Db1-Abb3 Db2-Abb4 D1-Ab3 D2-Ab4 D#1-A3
      D#2-A4 D##1-A#3 D##2-A#4 D###1-A##3 D###2-A##4 Ebb1-Bbbb3 Ebb2-Bbbb4
      Eb1-Bbb3 Eb2-Bbb4 E1-Bb3 E2-Bb4 E#1-B3 E#2-B4 E##1-B#3 E##2-B#4 E###1-B##3
      E###2-B##4 Fbb1-Cbbb4 Fb1-Cbb4 F1-Cb4 F#1-C4 F##1-C#4 F###1-C##4 Gbb1-Dbbb4
      Gb1-Dbb4 G1-Db4 G#1-D4 G##1-D#4 G###1-D##4))
    (30 "2 P8 + dd6" "2 octaves + doubly diminished sixth"
     ((number Sixth) (quality Doubly_diminished) (additional_octaves 2))
     (Ab1-Fbbb4 A1-Fbb4 A#1-Fb4 A##1-F4 A###1-F#4 Bb1-Gbbb4 B1-Gbb4 B#1-Gb4
      B##1-G4 B###1-G#4 C1-Abbb3 C2-Abbb4 C#1-Abb3 C#2-Abb4 C##1-Ab3 C##2-Ab4
      C###1-A3 C###2-A4 D1-Bbbb3 D2-Bbbb4 D#1-Bbb3 D#2-Bbb4 D##1-Bb3 D##2-Bb4
      D###1-B3 D###2-B4 Eb1-Cbbb4 E1-Cbb4 E#1-Cb4 E##1-C4 E###1-C#4 F1-Dbbb4
      F#1-Dbb4 F##1-Db4 F###1-D4 G1-Ebbb4 G#1-Ebb4 G##1-Eb4 G###1-E4))
    (31 "2 P8 + AA4" "2 octaves + doubly augmented fourth"
     ((number Fourth) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-Db4 Abb1-D4 Ab1-D#4 A1-D##4 A#1-D###4 Bbbb1-Eb4 Bbb1-E4 Bb1-E#4
      B1-E##4 B#1-E###4 Cbbb1-Fb3 Cbbb2-Fb4 Cbb1-F3 Cbb2-F4 Cb1-F#3 Cb2-F#4
      C1-F##3 C2-F##4 C#1-F###3 C#2-F###4 Dbbb1-Gb3 Dbbb2-Gb4 Dbb1-G3 Dbb2-G4
      Db1-G#3 Db2-G#4 D1-G##3 D2-G##4 D#1-G###3 D#2-G###4 Ebbb1-Ab3 Ebbb2-Ab4
      Ebb1-A3 Ebb2-A4 Eb1-A#3 Eb2-A#4 E1-A##3 E2-A##4 E#1-A###3 E#2-A###4
      Fbbb1-Bbb3 Fbbb2-Bbb4 Fbb1-Bb3 Fbb2-Bb4 Fb1-B3 Fb2-B4 F1-B#3 F2-B#4
      F#1-B##3 F#2-B##4 F##1-B###3 F##2-B###4 Gbbb1-Cb4 Gbb1-C4 Gb1-C#4 G1-C##4
      G#1-C###4))
    (31 "2 P8 + P5" "2 octaves + fifth"
     ((number Fifth) (quality Perfect) (additional_octaves 2))
     (Abbb1-Ebbb4 Abb1-Ebb4 Ab1-Eb4 A1-E4 A#1-E#4 A##1-E##4 A###1-E###4
      Bbbb1-Fbb4 Bbb1-Fb4 Bb1-F4 B1-F#4 B#1-F##4 B##1-F###4 Cbbb1-Gbbb3
      Cbbb2-Gbbb4 Cbb1-Gbb3 Cbb2-Gbb4 Cb1-Gb3 Cb2-Gb4 C1-G3 C2-G4 C#1-G#3 C#2-G#4
      C##1-G##3 C##2-G##4 C###1-G###3 C###2-G###4 Dbbb1-Abbb3 Dbbb2-Abbb4
      Dbb1-Abb3 Dbb2-Abb4 Db1-Ab3 Db2-Ab4 D1-A3 D2-A4 D#1-A#3 D#2-A#4 D##1-A##3
      D##2-A##4 D###1-A###3 D###2-A###4 Ebbb1-Bbbb3 Ebbb2-Bbbb4 Ebb1-Bbb3
      Ebb2-Bbb4 Eb1-Bb3 Eb2-Bb4 E1-B3 E2-B4 E#1-B#3 E#2-B#4 E##1-B##3 E##2-B##4
      E###1-B###3 E###2-B###4 Fbbb1-Cbbb4 Fbb1-Cbb4 Fb1-Cb4 F1-C4 F#1-C#4
      F##1-C##4 F###1-C###4 Gbbb1-Dbbb4 Gbb1-Dbb4 Gb1-Db4 G1-D4 G#1-D#4 G##1-D##4
      G###1-D###4))
    (31 "2 P8 + d6" "2 octaves + diminished sixth"
     ((number Sixth) (quality Diminished) (additional_octaves 2))
     (Abb1-Fbbb4 Ab1-Fbb4 A1-Fb4 A#1-F4 A##1-F#4 A###1-F##4 Bbb1-Gbbb4 Bb1-Gbb4
      B1-Gb4 B#1-G4 B##1-G#4 B###1-G##4 Cb1-Abbb3 Cb2-Abbb4 C1-Abb3 C2-Abb4
      C#1-Ab3 C#2-Ab4 C##1-A3 C##2-A4 C###1-A#3 C###2-A#4 Db1-Bbbb3 Db2-Bbbb4
      D1-Bbb3 D2-Bbb4 D#1-Bb3 D#2-Bb4 D##1-B3 D##2-B4 D###1-B#3 D###2-B#4
      Ebb1-Cbbb4 Eb1-Cbb4 E1-Cb4 E#1-C4 E##1-C#4 E###1-C##4 Fb1-Dbbb4 F1-Dbb4
      F#1-Db4 F##1-D4 F###1-D#4 Gb1-Ebbb4 G1-Ebb4 G#1-Eb4 G##1-E4 G###1-E#4))
    (32 "2 P8 + A5" "2 octaves + augmented fifth"
     ((number Fifth) (quality Augmented) (additional_octaves 2))
     (Abbb1-Ebb4 Abb1-Eb4 Ab1-E4 A1-E#4 A#1-E##4 A##1-E###4 Bbbb1-Fb4 Bbb1-F4
      Bb1-F#4 B1-F##4 B#1-F###4 Cbbb1-Gbb3 Cbbb2-Gbb4 Cbb1-Gb3 Cbb2-Gb4 Cb1-G3
      Cb2-G4 C1-G#3 C2-G#4 C#1-G##3 C#2-G##4 C##1-G###3 C##2-G###4 Dbbb1-Abb3
      Dbbb2-Abb4 Dbb1-Ab3 Dbb2-Ab4 Db1-A3 Db2-A4 D1-A#3 D2-A#4 D#1-A##3 D#2-A##4
      D##1-A###3 D##2-A###4 Ebbb1-Bbb3 Ebbb2-Bbb4 Ebb1-Bb3 Ebb2-Bb4 Eb1-B3 Eb2-B4
      E1-B#3 E2-B#4 E#1-B##3 E#2-B##4 E##1-B###3 E##2-B###4 Fbbb1-Cbb4 Fbb1-Cb4
      Fb1-C4 F1-C#4 F#1-C##4 F##1-C###4 Gbbb1-Dbb4 Gbb1-Db4 Gb1-D4 G1-D#4
      G#1-D##4 G##1-D###4))
    (32 "2 P8 + m6" "2 octaves + minor sixth"
     ((number Sixth) (quality Minor) (additional_octaves 2))
     (Abbb1-Fbbb4 Abb1-Fbb4 Ab1-Fb4 A1-F4 A#1-F#4 A##1-F##4 A###1-F###4
      Bbbb1-Gbbb4 Bbb1-Gbb4 Bb1-Gb4 B1-G4 B#1-G#4 B##1-G##4 B###1-G###4
      Cbb1-Abbb3 Cbb2-Abbb4 Cb1-Abb3 Cb2-Abb4 C1-Ab3 C2-Ab4 C#1-A3 C#2-A4
      C##1-A#3 C##2-A#4 C###1-A##3 C###2-A##4 Dbb1-Bbbb3 Dbb2-Bbbb4 Db1-Bbb3
      Db2-Bbb4 D1-Bb3 D2-Bb4 D#1-B3 D#2-B4 D##1-B#3 D##2-B#4 D###1-B##3
      D###2-B##4 Ebbb1-Cbbb4 Ebb1-Cbb4 Eb1-Cb4 E1-C4 E#1-C#4 E##1-C##4
      E###1-C###4 Fbb1-Dbbb4 Fb1-Dbb4 F1-Db4 F#1-D4 F##1-D#4 F###1-D##4
      Gbb1-Ebbb4 Gb1-Ebb4 G1-Eb4 G#1-E4 G##1-E#4 G###1-E##4))
    (32 "2 P8 + dd7" "2 octaves + doubly diminished seventh"
     ((number Seventh) (quality Doubly_diminished) (additional_octaves 2))
     (Ab1-Gbbb4 A1-Gbb4 A#1-Gb4 A##1-G4 A###1-G#4 Bb1-Abbb4 B1-Abb4 B#1-Ab4
      B##1-A4 B###1-A#4 C1-Bbbb3 C2-Bbbb4 C#1-Bbb3 C#2-Bbb4 C##1-Bb3 C##2-Bb4
      C###1-B3 C###2-B4 Db1-Cbbb4 D1-Cbb4 D#1-Cb4 D##1-C4 D###1-C#4 Eb1-Dbbb4
      E1-Dbb4 E#1-Db4 E##1-D4 E###1-D#4 F1-Ebbb4 F#1-Ebb4 F##1-Eb4 F###1-E4
      Gb1-Fbbb4 G1-Fbb4 G#1-Fb4 G##1-F4 G###1-F#4))
    (33 "2 P8 + AA5" "2 octaves + doubly augmented fifth"
     ((number Fifth) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-Eb4 Abb1-E4 Ab1-E#4 A1-E##4 A#1-E###4 Bbbb1-F4 Bbb1-F#4 Bb1-F##4
      B1-F###4 Cbbb1-Gb3 Cbbb2-Gb4 Cbb1-G3 Cbb2-G4 Cb1-G#3 Cb2-G#4 C1-G##3
      C2-G##4 C#1-G###3 C#2-G###4 Dbbb1-Ab3 Dbbb2-Ab4 Dbb1-A3 Dbb2-A4 Db1-A#3
      Db2-A#4 D1-A##3 D2-A##4 D#1-A###3 D#2-A###4 Ebbb1-Bb3 Ebbb2-Bb4 Ebb1-B3
      Ebb2-B4 Eb1-B#3 Eb2-B#4 E1-B##3 E2-B##4 E#1-B###3 E#2-B###4 Fbbb1-Cb4
      Fbb1-C4 Fb1-C#4 F1-C##4 F#1-C###4 Gbbb1-Db4 Gbb1-D4 Gb1-D#4 G1-D##4
      G#1-D###4))
    (33 "2 P8 + M6" "2 octaves + major sixth"
     ((number Sixth) (quality Major) (additional_octaves 2))
     (Abbb1-Fbb4 Abb1-Fb4 Ab1-F4 A1-F#4 A#1-F##4 A##1-F###4 Bbbb1-Gbb4 Bbb1-Gb4
      Bb1-G4 B1-G#4 B#1-G##4 B##1-G###4 Cbbb1-Abbb3 Cbbb2-Abbb4 Cbb1-Abb3
      Cbb2-Abb4 Cb1-Ab3 Cb2-Ab4 C1-A3 C2-A4 C#1-A#3 C#2-A#4 C##1-A##3 C##2-A##4
      C###1-A###3 C###2-A###4 Dbbb1-Bbbb3 Dbbb2-Bbbb4 Dbb1-Bbb3 Dbb2-Bbb4 Db1-Bb3
      Db2-Bb4 D1-B3 D2-B4 D#1-B#3 D#2-B#4 D##1-B##3 D##2-B##4 D###1-B###3
      D###2-B###4 Ebbb1-Cbb4 Ebb1-Cb4 Eb1-C4 E1-C#4 E#1-C##4 E##1-C###4
      Fbbb1-Dbbb4 Fbb1-Dbb4 Fb1-Db4 F1-D4 F#1-D#4 F##1-D##4 F###1-D###4
      Gbbb1-Ebbb4 Gbb1-Ebb4 Gb1-Eb4 G1-E4 G#1-E#4 G##1-E##4 G###1-E###4))
    (33 "2 P8 + d7" "2 octaves + diminished seventh"
     ((number Seventh) (quality Diminished) (additional_octaves 2))
     (Abb1-Gbbb4 Ab1-Gbb4 A1-Gb4 A#1-G4 A##1-G#4 A###1-G##4 Bbb1-Abbb4 Bb1-Abb4
      B1-Ab4 B#1-A4 B##1-A#4 B###1-A##4 Cb1-Bbbb3 Cb2-Bbbb4 C1-Bbb3 C2-Bbb4
      C#1-Bb3 C#2-Bb4 C##1-B3 C##2-B4 C###1-B#3 C###2-B#4 Dbb1-Cbbb4 Db1-Cbb4
      D1-Cb4 D#1-C4 D##1-C#4 D###1-C##4 Ebb1-Dbbb4 Eb1-Dbb4 E1-Db4 E#1-D4
      E##1-D#4 E###1-D##4 Fb1-Ebbb4 F1-Ebb4 F#1-Eb4 F##1-E4 F###1-E#4 Gbb1-Fbbb4
      Gb1-Fbb4 G1-Fb4 G#1-F4 G##1-F#4 G###1-F##4))
    (34 "2 P8 + A6" "2 octaves + augmented sixth"
     ((number Sixth) (quality Augmented) (additional_octaves 2))
     (Abbb1-Fb4 Abb1-F4 Ab1-F#4 A1-F##4 A#1-F###4 Bbbb1-Gb4 Bbb1-G4 Bb1-G#4
      B1-G##4 B#1-G###4 Cbbb1-Abb3 Cbbb2-Abb4 Cbb1-Ab3 Cbb2-Ab4 Cb1-A3 Cb2-A4
      C1-A#3 C2-A#4 C#1-A##3 C#2-A##4 C##1-A###3 C##2-A###4 Dbbb1-Bbb3 Dbbb2-Bbb4
      Dbb1-Bb3 Dbb2-Bb4 Db1-B3 Db2-B4 D1-B#3 D2-B#4 D#1-B##3 D#2-B##4 D##1-B###3
      D##2-B###4 Ebbb1-Cb4 Ebb1-C4 Eb1-C#4 E1-C##4 E#1-C###4 Fbbb1-Dbb4 Fbb1-Db4
      Fb1-D4 F1-D#4 F#1-D##4 F##1-D###4 Gbbb1-Ebb4 Gbb1-Eb4 Gb1-E4 G1-E#4
      G#1-E##4 G##1-E###4))
    (34 "2 P8 + m7" "2 octaves + minor seventh"
     ((number Seventh) (quality Minor) (additional_octaves 2))
     (Abbb1-Gbbb4 Abb1-Gbb4 Ab1-Gb4 A1-G4 A#1-G#4 A##1-G##4 A###1-G###4
      Bbbb1-Abbb4 Bbb1-Abb4 Bb1-Ab4 B1-A4 B#1-A#4 B##1-A##4 B###1-A###4
      Cbb1-Bbbb3 Cbb2-Bbbb4 Cb1-Bbb3 Cb2-Bbb4 C1-Bb3 C2-Bb4 C#1-B3 C#2-B4
      C##1-B#3 C##2-B#4 C###1-B##3 C###2-B##4 Dbbb1-Cbbb4 Dbb1-Cbb4 Db1-Cb4 D1-C4
      D#1-C#4 D##1-C##4 D###1-C###4 Ebbb1-Dbbb4 Ebb1-Dbb4 Eb1-Db4 E1-D4 E#1-D#4
      E##1-D##4 E###1-D###4 Fbb1-Ebbb4 Fb1-Ebb4 F1-Eb4 F#1-E4 F##1-E#4 F###1-E##4
      Gbbb1-Fbbb4 Gbb1-Fbb4 Gb1-Fb4 G1-F4 G#1-F#4 G##1-F##4 G###1-F###4))
    (35 "2 P8 + AA6" "2 octaves + doubly augmented sixth"
     ((number Sixth) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-F4 Abb1-F#4 Ab1-F##4 A1-F###4 Bbbb1-G4 Bbb1-G#4 Bb1-G##4 B1-G###4
      Cbbb1-Ab3 Cbbb2-Ab4 Cbb1-A3 Cbb2-A4 Cb1-A#3 Cb2-A#4 C1-A##3 C2-A##4
      C#1-A###3 C#2-A###4 Dbbb1-Bb3 Dbbb2-Bb4 Dbb1-B3 Dbb2-B4 Db1-B#3 Db2-B#4
      D1-B##3 D2-B##4 D#1-B###3 D#2-B###4 Ebbb1-C4 Ebb1-C#4 Eb1-C##4 E1-C###4
      Fbbb1-Db4 Fbb1-D4 Fb1-D#4 F1-D##4 F#1-D###4 Gbbb1-Eb4 Gbb1-E4 Gb1-E#4
      G1-E##4 G#1-E###4))
    (35 "2 P8 + M7" "2 octaves + major seventh"
     ((number Seventh) (quality Major) (additional_octaves 2))
     (Abbb1-Gbb4 Abb1-Gb4 Ab1-G4 A1-G#4 A#1-G##4 A##1-G###4 Bbbb1-Abb4 Bbb1-Ab4
      Bb1-A4 B1-A#4 B#1-A##4 B##1-A###4 Cbbb1-Bbbb3 Cbbb2-Bbbb4 Cbb1-Bbb3
      Cbb2-Bbb4 Cb1-Bb3 Cb2-Bb4 C1-B3 C2-B4 C#1-B#3 C#2-B#4 C##1-B##3 C##2-B##4
      C###1-B###3 C###2-B###4 Dbbb1-Cbb4 Dbb1-Cb4 Db1-C4 D1-C#4 D#1-C##4
      D##1-C###4 Ebbb1-Dbb4 Ebb1-Db4 Eb1-D4 E1-D#4 E#1-D##4 E##1-D###4
      Fbbb1-Ebbb4 Fbb1-Ebb4 Fb1-Eb4 F1-E4 F#1-E#4 F##1-E##4 F###1-E###4
      Gbbb1-Fbb4 Gbb1-Fb4 Gb1-F4 G1-F#4 G#1-F##4 G##1-F###4))
    (36 "3 P8" "3 octaves"
     ((number Unison) (quality Perfect) (additional_octaves 3))
     (Abbb1-Abbb4 Abb1-Abb4 Ab1-Ab4 A1-A4 A#1-A#4 A##1-A##4 A###1-A###4
      Bbbb1-Bbbb4 Bbb1-Bbb4 Bb1-Bb4 B1-B4 B#1-B#4 B##1-B##4 B###1-B###4
      Cbbb1-Cbbb4 Cbb1-Cbb4 Cb1-Cb4 C1-C4 C#1-C#4 C##1-C##4 C###1-C###4
      Dbbb1-Dbbb4 Dbb1-Dbb4 Db1-Db4 D1-D4 D#1-D#4 D##1-D##4 D###1-D###4
      Ebbb1-Ebbb4 Ebb1-Ebb4 Eb1-Eb4 E1-E4 E#1-E#4 E##1-E##4 E###1-E###4
      Fbbb1-Fbbb4 Fbb1-Fbb4 Fb1-Fb4 F1-F4 F#1-F#4 F##1-F##4 F###1-F###4
      Gbbb1-Gbbb4 Gbb1-Gbb4 Gb1-Gb4 G1-G4 G#1-G#4 G##1-G##4 G###1-G###4))
    (36 "3 P8 + d2" "3 octaves + diminished second"
     ((number Second) (quality Diminished) (additional_octaves 3))
     (Ab1-Bbbb4 A1-Bbb4 A#1-Bb4 A##1-B4 A###1-B#4 Cb1-Dbbb4 C1-Dbb4 C#1-Db4
      C##1-D4 C###1-D#4 Db1-Ebbb4 D1-Ebb4 D#1-Eb4 D##1-E4 D###1-E#4 Ebb1-Fbbb4
      Eb1-Fbb4 E1-Fb4 E#1-F4 E##1-F#4 E###1-F##4 Fb1-Gbbb4 F1-Gbb4 F#1-Gb4
      F##1-G4 F###1-G#4 Gb1-Abbb4 G1-Abb4 G#1-Ab4 G##1-A4 G###1-A#4))
    (36 "2 P8 + A7" "2 octaves + augmented seventh"
     ((number Seventh) (quality Augmented) (additional_octaves 2))
     (Abbb1-Gb4 Abb1-G4 Ab1-G#4 A1-G##4 A#1-G###4 Bbbb1-Ab4 Bbb1-A4 Bb1-A#4
      B1-A##4 B#1-A###4 Cbbb1-Bbb3 Cbbb2-Bbb4 Cbb1-Bb3 Cbb2-Bb4 Cb1-B3 Cb2-B4
      C1-B#3 C2-B#4 C#1-B##3 C#2-B##4 C##1-B###3 C##2-B###4 Dbbb1-Cb4 Dbb1-C4
      Db1-C#4 D1-C##4 D#1-C###4 Ebbb1-Db4 Ebb1-D4 Eb1-D#4 E1-D##4 E#1-D###4
      Fbbb1-Ebb4 Fbb1-Eb4 Fb1-E4 F1-E#4 F#1-E##4 F##1-E###4 Gbbb1-Fb4 Gbb1-F4
      Gb1-F#4 G1-F##4 G#1-F###4))
    (37 "3 P8 + A1" "3 octaves + augmented unison"
     ((number Unison) (quality Augmented) (additional_octaves 3))
     (Abbb1-Abb4 Abb1-Ab4 Ab1-A4 A1-A#4 A#1-A##4 A##1-A###4 Bbbb1-Bbb4 Bbb1-Bb4
      Bb1-B4 B1-B#4 B#1-B##4 B##1-B###4 Cbbb1-Cbb4 Cbb1-Cb4 Cb1-C4 C1-C#4
      C#1-C##4 C##1-C###4 Dbbb1-Dbb4 Dbb1-Db4 Db1-D4 D1-D#4 D#1-D##4 D##1-D###4
      Ebbb1-Ebb4 Ebb1-Eb4 Eb1-E4 E1-E#4 E#1-E##4 E##1-E###4 Fbbb1-Fbb4 Fbb1-Fb4
      Fb1-F4 F1-F#4 F#1-F##4 F##1-F###4 Gbbb1-Gbb4 Gbb1-Gb4 Gb1-G4 G1-G#4
      G#1-G##4 G##1-G###4))
    (37 "3 P8 + m2" "3 octaves + minor second"
     ((number Second) (quality Minor) (additional_octaves 3))
     (Abb1-Bbbb4 Ab1-Bbb4 A1-Bb4 A#1-B4 A##1-B#4 A###1-B##4 Cbb1-Dbbb4 Cb1-Dbb4
      C1-Db4 C#1-D4 C##1-D#4 C###1-D##4 Dbb1-Ebbb4 Db1-Ebb4 D1-Eb4 D#1-E4
      D##1-E#4 D###1-E##4 Ebbb1-Fbbb4 Ebb1-Fbb4 Eb1-Fb4 E1-F4 E#1-F#4 E##1-F##4
      E###1-F###4 Fbb1-Gbbb4 Fb1-Gbb4 F1-Gb4 F#1-G4 F##1-G#4 F###1-G##4
      Gbb1-Abbb4 Gb1-Abb4 G1-Ab4 G#1-A4 G##1-A#4 G###1-A##4))
    (37 "3 P8 + dd3" "3 octaves + doubly diminished third"
     ((number Third) (quality Doubly_diminished) (additional_octaves 3))
     (C1-Ebbb4 C#1-Ebb4 C##1-Eb4 C###1-E4 Db1-Fbbb4 D1-Fbb4 D#1-Fb4 D##1-F4
      D###1-F#4 Eb1-Gbbb4 E1-Gbb4 E#1-Gb4 E##1-G4 E###1-G#4 F1-Abbb4 F#1-Abb4
      F##1-Ab4 F###1-A4 G1-Bbbb4 G#1-Bbb4 G##1-Bb4 G###1-B4))
    (37 "2 P8 + AA7" "2 octaves + doubly augmented seventh"
     ((number Seventh) (quality Doubly_augmented) (additional_octaves 2))
     (Abbb1-G4 Abb1-G#4 Ab1-G##4 A1-G###4 Bbbb1-A4 Bbb1-A#4 Bb1-A##4 B1-A###4
      Cbbb1-Bb3 Cbbb2-Bb4 Cbb1-B3 Cbb2-B4 Cb1-B#3 Cb2-B#4 C1-B##3 C2-B##4
      C#1-B###3 C#2-B###4 Dbbb1-C4 Dbb1-C#4 Db1-C##4 D1-C###4 Ebbb1-D4 Ebb1-D#4
      Eb1-D##4 E1-D###4 Fbbb1-Eb4 Fbb1-E4 Fb1-E#4 F1-E##4 F#1-E###4 Gbbb1-F4
      Gbb1-F#4 Gb1-F##4 G1-F###4))
    (38 "3 P8 + AA1" "3 octaves + doubly augmented unison"
     ((number Unison) (quality Doubly_augmented) (additional_octaves 3))
     (Abbb1-Ab4 Abb1-A4 Ab1-A#4 A1-A##4 A#1-A###4 Bbbb1-Bb4 Bbb1-B4 Bb1-B#4
      B1-B##4 B#1-B###4 Cbbb1-Cb4 Cbb1-C4 Cb1-C#4 C1-C##4 C#1-C###4 Dbbb1-Db4
      Dbb1-D4 Db1-D#4 D1-D##4 D#1-D###4 Ebbb1-Eb4 Ebb1-E4 Eb1-E#4 E1-E##4
      E#1-E###4 Fbbb1-Fb4 Fbb1-F4 Fb1-F#4 F1-F##4 F#1-F###4 Gbbb1-Gb4 Gbb1-G4
      Gb1-G#4 G1-G##4 G#1-G###4))
    (38 "3 P8 + M2" "3 octaves + major second"
     ((number Second) (quality Major) (additional_octaves 3))
     (Abbb1-Bbbb4 Abb1-Bbb4 Ab1-Bb4 A1-B4 A#1-B#4 A##1-B##4 A###1-B###4
      Cbbb1-Dbbb4 Cbb1-Dbb4 Cb1-Db4 C1-D4 C#1-D#4 C##1-D##4 C###1-D###4
      Dbbb1-Ebbb4 Dbb1-Ebb4 Db1-Eb4 D1-E4 D#1-E#4 D##1-E##4 D###1-E###4
      Ebbb1-Fbb4 Ebb1-Fb4 Eb1-F4 E1-F#4 E#1-F##4 E##1-F###4 Fbbb1-Gbbb4 Fbb1-Gbb4
      Fb1-Gb4 F1-G4 F#1-G#4 F##1-G##4 F###1-G###4 Gbbb1-Abbb4 Gbb1-Abb4 Gb1-Ab4
      G1-A4 G#1-A#4 G##1-A##4 G###1-A###4))
    (38 "3 P8 + d3" "3 octaves + diminished third"
     ((number Third) (quality Diminished) (additional_octaves 3))
     (Cb1-Ebbb4 C1-Ebb4 C#1-Eb4 C##1-E4 C###1-E#4 Dbb1-Fbbb4 Db1-Fbb4 D1-Fb4
      D#1-F4 D##1-F#4 D###1-F##4 Ebb1-Gbbb4 Eb1-Gbb4 E1-Gb4 E#1-G4 E##1-G#4
      E###1-G##4 Fb1-Abbb4 F1-Abb4 F#1-Ab4 F##1-A4 F###1-A#4 Gb1-Bbbb4 G1-Bbb4
      G#1-Bb4 G##1-B4 G###1-B#4))
    (39 "3 P8 + A2" "3 octaves + augmented second"
     ((number Second) (quality Augmented) (additional_octaves 3))
     (Abbb1-Bbb4 Abb1-Bb4 Ab1-B4 A1-B#4 A#1-B##4 A##1-B###4 Cbbb1-Dbb4 Cbb1-Db4
      Cb1-D4 C1-D#4 C#1-D##4 C##1-D###4 Dbbb1-Ebb4 Dbb1-Eb4 Db1-E4 D1-E#4
      D#1-E##4 D##1-E###4 Ebbb1-Fb4 Ebb1-F4 Eb1-F#4 E1-F##4 E#1-F###4 Fbbb1-Gbb4
      Fbb1-Gb4 Fb1-G4 F1-G#4 F#1-G##4 F##1-G###4 Gbbb1-Abb4 Gbb1-Ab4 Gb1-A4
      G1-A#4 G#1-A##4 G##1-A###4))
    (39 "3 P8 + m3" "3 octaves + minor third"
     ((number Third) (quality Minor) (additional_octaves 3))
     (Cbb1-Ebbb4 Cb1-Ebb4 C1-Eb4 C#1-E4 C##1-E#4 C###1-E##4 Dbbb1-Fbbb4 Dbb1-Fbb4
      Db1-Fb4 D1-F4 D#1-F#4 D##1-F##4 D###1-F###4 Ebbb1-Gbbb4 Ebb1-Gbb4 Eb1-Gb4
      E1-G4 E#1-G#4 E##1-G##4 E###1-G###4 Fbb1-Abbb4 Fb1-Abb4 F1-Ab4 F#1-A4
      F##1-A#4 F###1-A##4 Gbb1-Bbbb4 Gb1-Bbb4 G1-Bb4 G#1-B4 G##1-B#4 G###1-B##4))
    (39 "3 P8 + dd4" "3 octaves + doubly diminished fourth"
     ((number Fourth) (quality Doubly_diminished) (additional_octaves 3))
     (Cb1-Fbbb4 C1-Fbb4 C#1-Fb4 C##1-F4 C###1-F#4 Db1-Gbbb4 D1-Gbb4 D#1-Gb4
      D##1-G4 D###1-G#4 Eb1-Abbb4 E1-Abb4 E#1-Ab4 E##1-A4 E###1-A#4 F1-Bbbb4
      F#1-Bbb4 F##1-Bb4 F###1-B4))
    (40 "3 P8 + AA2" "3 octaves + doubly augmented second"
     ((number Second) (quality Doubly_augmented) (additional_octaves 3))
     (Abbb1-Bb4 Abb1-B4 Ab1-B#4 A1-B##4 A#1-B###4 Cbbb1-Db4 Cbb1-D4 Cb1-D#4
      C1-D##4 C#1-D###4 Dbbb1-Eb4 Dbb1-E4 Db1-E#4 D1-E##4 D#1-E###4 Ebbb1-F4
      Ebb1-F#4 Eb1-F##4 E1-F###4 Fbbb1-Gb4 Fbb1-G4 Fb1-G#4 F1-G##4 F#1-G###4
      Gbbb1-Ab4 Gbb1-A4 Gb1-A#4 G1-A##4 G#1-A###4))
    (40 "3 P8 + M3" "3 octaves + major third"
     ((number Third) (quality Major) (additional_octaves 3))
     (Cbbb1-Ebbb4 Cbb1-Ebb4 Cb1-Eb4 C1-E4 C#1-E#4 C##1-E##4 C###1-E###4
      Dbbb1-Fbb4 Dbb1-Fb4 Db1-F4 D1-F#4 D#1-F##4 D##1-F###4 Ebbb1-Gbb4 Ebb1-Gb4
      Eb1-G4 E1-G#4 E#1-G##4 E##1-G###4 Fbbb1-Abbb4 Fbb1-Abb4 Fb1-Ab4 F1-A4
      F#1-A#4 F##1-A##4 F###1-A###4 Gbbb1-Bbbb4 Gbb1-Bbb4 Gb1-Bb4 G1-B4 G#1-B#4
      G##1-B##4 G###1-B###4))
    (40 "3 P8 + d4" "3 octaves + diminished fourth"
     ((number Fourth) (quality Diminished) (additional_octaves 3))
     (Cbb1-Fbbb4 Cb1-Fbb4 C1-Fb4 C#1-F4 C##1-F#4 C###1-F##4 Dbb1-Gbbb4 Db1-Gbb4
      D1-Gb4 D#1-G4 D##1-G#4 D###1-G##4 Ebb1-Abbb4 Eb1-Abb4 E1-Ab4 E#1-A4
      E##1-A#4 E###1-A##4 Fb1-Bbbb4 F1-Bbb4 F#1-Bb4 F##1-B4 F###1-B#4))
    (41 "3 P8 + A3" "3 octaves + augmented third"
     ((number Third) (quality Augmented) (additional_octaves 3))
     (Cbbb1-Ebb4 Cbb1-Eb4 Cb1-E4 C1-E#4 C#1-E##4 C##1-E###4 Dbbb1-Fb4 Dbb1-F4
      Db1-F#4 D1-F##4 D#1-F###4 Ebbb1-Gb4 Ebb1-G4 Eb1-G#4 E1-G##4 E#1-G###4
      Fbbb1-Abb4 Fbb1-Ab4 Fb1-A4 F1-A#4 F#1-A##4 F##1-A###4 Gbbb1-Bbb4 Gbb1-Bb4
      Gb1-B4 G1-B#4 G#1-B##4 G##1-B###4))
    (41 "3 P8 + P4" "3 octaves + fourth"
     ((number Fourth) (quality Perfect) (additional_octaves 3))
     (Cbbb1-Fbbb4 Cbb1-Fbb4 Cb1-Fb4 C1-F4 C#1-F#4 C##1-F##4 C###1-F###4
      Dbbb1-Gbbb4 Dbb1-Gbb4 Db1-Gb4 D1-G4 D#1-G#4 D##1-G##4 D###1-G###4
      Ebbb1-Abbb4 Ebb1-Abb4 Eb1-Ab4 E1-A4 E#1-A#4 E##1-A##4 E###1-A###4
      Fbb1-Bbbb4 Fb1-Bbb4 F1-Bb4 F#1-B4 F##1-B#4 F###1-B##4))
    (41 "3 P8 + dd5" "3 octaves + doubly diminished fifth"
     ((number Fifth) (quality Doubly_diminished) (additional_octaves 3))
     (Cb1-Gbbb4 C1-Gbb4 C#1-Gb4 C##1-G4 C###1-G#4 Db1-Abbb4 D1-Abb4 D#1-Ab4
      D##1-A4 D###1-A#4 Eb1-Bbbb4 E1-Bbb4 E#1-Bb4 E##1-B4 E###1-B#4))
    (42 "3 P8 + AA3" "3 octaves + doubly augmented third"
     ((number Third) (quality Doubly_augmented) (additional_octaves 3))
     (Cbbb1-Eb4 Cbb1-E4 Cb1-E#4 C1-E##4 C#1-E###4 Dbbb1-F4 Dbb1-F#4 Db1-F##4
      D1-F###4 Ebbb1-G4 Ebb1-G#4 Eb1-G##4 E1-G###4 Fbbb1-Ab4 Fbb1-A4 Fb1-A#4
      F1-A##4 F#1-A###4 Gbbb1-Bb4 Gbb1-B4 Gb1-B#4 G1-B##4 G#1-B###4))
    (42 "3 P8 + A4" "3 octaves + augmented fourth"
     ((number Fourth) (quality Augmented) (additional_octaves 3))
     (Cbbb1-Fbb4 Cbb1-Fb4 Cb1-F4 C1-F#4 C#1-F##4 C##1-F###4 Dbbb1-Gbb4 Dbb1-Gb4
      Db1-G4 D1-G#4 D#1-G##4 D##1-G###4 Ebbb1-Abb4 Ebb1-Ab4 Eb1-A4 E1-A#4
      E#1-A##4 E##1-A###4 Fbbb1-Bbbb4 Fbb1-Bbb4 Fb1-Bb4 F1-B4 F#1-B#4 F##1-B##4
      F###1-B###4))
    (42 "3 P8 + d5" "3 octaves + diminished fifth"
     ((number Fifth) (quality Diminished) (additional_octaves 3))
     (Cbb1-Gbbb4 Cb1-Gbb4 C1-Gb4 C#1-G4 C##1-G#4 C###1-G##4 Dbb1-Abbb4 Db1-Abb4
      D1-Ab4 D#1-A4 D##1-A#4 D###1-A##4 Ebb1-Bbbb4 Eb1-Bbb4 E1-Bb4 E#1-B4
      E##1-B#4 E###1-B##4))
    (42 "3 P8 + dd6" "3 octaves + doubly diminished sixth"
     ((number Sixth) (quality Doubly_diminished) (additional_octaves 3))
     (C1-Abbb4 C#1-Abb4 C##1-Ab4 C###1-A4 D1-Bbbb4 D#1-Bbb4 D##1-Bb4 D###1-B4))
    (43 "3 P8 + AA4" "3 octaves + doubly augmented fourth"
     ((number Fourth) (quality Doubly_augmented) (additional_octaves 3))
     (Cbbb1-Fb4 Cbb1-F4 Cb1-F#4 C1-F##4 C#1-F###4 Dbbb1-Gb4 Dbb1-G4 Db1-G#4
      D1-G##4 D#1-G###4 Ebbb1-Ab4 Ebb1-A4 Eb1-A#4 E1-A##4 E#1-A###4 Fbbb1-Bbb4
      Fbb1-Bb4 Fb1-B4 F1-B#4 F#1-B##4 F##1-B###4))
    (43 "3 P8 + P5" "3 octaves + fifth"
     ((number Fifth) (quality Perfect) (additional_octaves 3))
     (Cbbb1-Gbbb4 Cbb1-Gbb4 Cb1-Gb4 C1-G4 C#1-G#4 C##1-G##4 C###1-G###4
      Dbbb1-Abbb4 Dbb1-Abb4 Db1-Ab4 D1-A4 D#1-A#4 D##1-A##4 D###1-A###4
      Ebbb1-Bbbb4 Ebb1-Bbb4 Eb1-Bb4 E1-B4 E#1-B#4 E##1-B##4 E###1-B###4))
    (43 "3 P8 + d6" "3 octaves + diminished sixth"
     ((number Sixth) (quality Diminished) (additional_octaves 3))
     (Cb1-Abbb4 C1-Abb4 C#1-Ab4 C##1-A4 C###1-A#4 Db1-Bbbb4 D1-Bbb4 D#1-Bb4
      D##1-B4 D###1-B#4))
    (44 "3 P8 + A5" "3 octaves + augmented fifth"
     ((number Fifth) (quality Augmented) (additional_octaves 3))
     (Cbbb1-Gbb4 Cbb1-Gb4 Cb1-G4 C1-G#4 C#1-G##4 C##1-G###4 Dbbb1-Abb4 Dbb1-Ab4
      Db1-A4 D1-A#4 D#1-A##4 D##1-A###4 Ebbb1-Bbb4 Ebb1-Bb4 Eb1-B4 E1-B#4
      E#1-B##4 E##1-B###4))
    (44 "3 P8 + m6" "3 octaves + minor sixth"
     ((number Sixth) (quality Minor) (additional_octaves 3))
     (Cbb1-Abbb4 Cb1-Abb4 C1-Ab4 C#1-A4 C##1-A#4 C###1-A##4 Dbb1-Bbbb4 Db1-Bbb4
      D1-Bb4 D#1-B4 D##1-B#4 D###1-B##4))
    (44 "3 P8 + dd7" "3 octaves + doubly diminished seventh"
     ((number Seventh) (quality Doubly_diminished) (additional_octaves 3))
     (C1-Bbbb4 C#1-Bbb4 C##1-Bb4 C###1-B4))
    (45 "3 P8 + AA5" "3 octaves + doubly augmented fifth"
     ((number Fifth) (quality Doubly_augmented) (additional_octaves 3))
     (Cbbb1-Gb4 Cbb1-G4 Cb1-G#4 C1-G##4 C#1-G###4 Dbbb1-Ab4 Dbb1-A4 Db1-A#4
      D1-A##4 D#1-A###4 Ebbb1-Bb4 Ebb1-B4 Eb1-B#4 E1-B##4 E#1-B###4))
    (45 "3 P8 + M6" "3 octaves + major sixth"
     ((number Sixth) (quality Major) (additional_octaves 3))
     (Cbbb1-Abbb4 Cbb1-Abb4 Cb1-Ab4 C1-A4 C#1-A#4 C##1-A##4 C###1-A###4
      Dbbb1-Bbbb4 Dbb1-Bbb4 Db1-Bb4 D1-B4 D#1-B#4 D##1-B##4 D###1-B###4))
    (45 "3 P8 + d7" "3 octaves + diminished seventh"
     ((number Seventh) (quality Diminished) (additional_octaves 3))
     (Cb1-Bbbb4 C1-Bbb4 C#1-Bb4 C##1-B4 C###1-B#4))
    (46 "3 P8 + A6" "3 octaves + augmented sixth"
     ((number Sixth) (quality Augmented) (additional_octaves 3))
     (Cbbb1-Abb4 Cbb1-Ab4 Cb1-A4 C1-A#4 C#1-A##4 C##1-A###4 Dbbb1-Bbb4 Dbb1-Bb4
      Db1-B4 D1-B#4 D#1-B##4 D##1-B###4))
    (46 "3 P8 + m7" "3 octaves + minor seventh"
     ((number Seventh) (quality Minor) (additional_octaves 3))
     (Cbb1-Bbbb4 Cb1-Bbb4 C1-Bb4 C#1-B4 C##1-B#4 C###1-B##4))
    (47 "3 P8 + AA6" "3 octaves + doubly augmented sixth"
     ((number Sixth) (quality Doubly_augmented) (additional_octaves 3))
     (Cbbb1-Ab4 Cbb1-A4 Cb1-A#4 C1-A##4 C#1-A###4 Dbbb1-Bb4 Dbb1-B4 Db1-B#4
      D1-B##4 D#1-B###4))
    (47 "3 P8 + M7" "3 octaves + major seventh"
     ((number Seventh) (quality Major) (additional_octaves 3))
     (Cbbb1-Bbbb4 Cbb1-Bbb4 Cb1-Bb4 C1-B4 C#1-B#4 C##1-B##4 C###1-B###4))
    (48 "3 P8 + A7" "3 octaves + augmented seventh"
     ((number Seventh) (quality Augmented) (additional_octaves 3))
     (Cbbb1-Bbb4 Cbb1-Bb4 Cb1-B4 C1-B#4 C#1-B##4 C##1-B###4))
    (49 "3 P8 + AA7" "3 octaves + doubly augmented seventh"
     ((number Seventh) (quality Doubly_augmented) (additional_octaves 3))
     (Cbbb1-Bb4 Cbb1-B4 Cb1-B#4 C1-B##4 C#1-B###4)) |}]
;;
