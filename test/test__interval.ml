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
         Interval.{ number; quality })))
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
    let canonical_interval = Interval.to_name canonical_interval in
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
    (11 "major seventh" (AA6 M7 d8 dd9))
    (12 octave (A7 P8 d9))
    (13 "minor ninth" (AA7 A8 m9 dd10))
    (14 "major ninth" (AA8 M9 d10))
    (15 "minor tenth" (A9 m10 dd11))
    (16 "major tenth" (AA9 M10 d11))
    (17 eleventh (A10 P11 dd12))
    (18 "augmented eleventh" (AA10 A11 d12 dd13))
    (19 twelfth (AA11 P12 d13))
    (20 "minor thirteenth" (A12 m13 dd14))
    (21 "major thirteenth" (AA12 M13 d14))
    (22 "minor fourteeth" (A13 m14 dd15))
    (23 "major fourteeth" (AA13 M14 d15))
    (24 "double octave" (A14 P15)) |}]
;;

let%expect_test "compute" =
  let inputs =
    let open List.Let_syntax in
    let%bind l1 = Note.Letter_name.all in
    let%bind l2 = Note.Letter_name.all in
    let%bind s1 = Note.Symbol.all in
    let%bind s2 = Note.Symbol.all in
    let%bind plus_one_octave = Bool.all in
    return
      ( { Note.letter_name = l1; symbol = s1 }
      , { Note.letter_name = l2; symbol = s2 }
      , plus_one_octave )
  in
  let table = Hashtbl.create (module Interval) in
  List.iter inputs ~f:(fun (from, to_, plus_one_octave) ->
    match Interval.compute ~from ~to_ ~plus_one_octave () with
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
        , (Interval.to_name interval : string)
        , (intervals : string list)]);
  [%expect {|
    (0 P1 unison
     (Abbb-Abbb Abb-Abb Ab-Ab A-A A#-A# A##-A## A###-A### Bbbb-Bbbb Bbb-Bbb Bb-Bb
      B-B B#-B# B##-B## B###-B### Cbbb-Cbbb Cbb-Cbb Cb-Cb C-C C#-C# C##-C##
      C###-C### Dbbb-Dbbb Dbb-Dbb Db-Db D-D D#-D# D##-D## D###-D### Ebbb-Ebbb
      Ebb-Ebb Eb-Eb E-E E#-E# E##-E## E###-E### Fbbb-Fbbb Fbb-Fbb Fb-Fb F-F F#-F#
      F##-F## F###-F### Gbbb-Gbbb Gbb-Gbb Gb-Gb G-G G#-G# G##-G## G###-G###))
    (0 d2 "diminished second"
     (Ab-Bbbb A-Bbb A#-Bb A##-B A###-B# Bbb-Cbbb Bb-Cbb B-Cb B#-C B##-C# B###-C##
      Cb-Dbbb C-Dbb C#-Db C##-D C###-D# Db-Ebbb D-Ebb D#-Eb D##-E D###-E#
      Ebb-Fbbb Eb-Fbb E-Fb E#-F E##-F# E###-F## Fb-Gbbb F-Gbb F#-Gb F##-G F###-G#
      Gb-Abbb G-Abb G#-Ab G##-A G###-A#))
    (1 A1 "augmented unison"
     (Abbb-Abb Abb-Ab Ab-A A-A# A#-A## A##-A### Bbbb-Bbb Bbb-Bb Bb-B B-B# B#-B##
      B##-B### Cbbb-Cbb Cbb-Cb Cb-C C-C# C#-C## C##-C### Dbbb-Dbb Dbb-Db Db-D
      D-D# D#-D## D##-D### Ebbb-Ebb Ebb-Eb Eb-E E-E# E#-E## E##-E### Fbbb-Fbb
      Fbb-Fb Fb-F F-F# F#-F## F##-F### Gbbb-Gbb Gbb-Gb Gb-G G-G# G#-G## G##-G###))
    (1 m2 "minor second"
     (Abb-Bbbb Ab-Bbb A-Bb A#-B A##-B# A###-B## Bbbb-Cbbb Bbb-Cbb Bb-Cb B-C B#-C#
      B##-C## B###-C### Cbb-Dbbb Cb-Dbb C-Db C#-D C##-D# C###-D## Dbb-Ebbb Db-Ebb
      D-Eb D#-E D##-E# D###-E## Ebbb-Fbbb Ebb-Fbb Eb-Fb E-F E#-F# E##-F##
      E###-F### Fbb-Gbbb Fb-Gbb F-Gb F#-G F##-G# F###-G## Gbb-Abbb Gb-Abb G-Ab
      G#-A G##-A# G###-A##))
    (1 dd3 "doubly diminished third"
     (Ab-Cbbb A-Cbb A#-Cb A##-C A###-C# Bb-Dbbb B-Dbb B#-Db B##-D B###-D# C-Ebbb
      C#-Ebb C##-Eb C###-E Db-Fbbb D-Fbb D#-Fb D##-F D###-F# Eb-Gbbb E-Gbb E#-Gb
      E##-G E###-G# F-Abbb F#-Abb F##-Ab F###-A G-Bbbb G#-Bbb G##-Bb G###-B))
    (2 AA1 "doubly augmented unison"
     (Abbb-Ab Abb-A Ab-A# A-A## A#-A### Bbbb-Bb Bbb-B Bb-B# B-B## B#-B### Cbbb-Cb
      Cbb-C Cb-C# C-C## C#-C### Dbbb-Db Dbb-D Db-D# D-D## D#-D### Ebbb-Eb Ebb-E
      Eb-E# E-E## E#-E### Fbbb-Fb Fbb-F Fb-F# F-F## F#-F### Gbbb-Gb Gbb-G Gb-G#
      G-G## G#-G###))
    (2 M2 "major second"
     (Abbb-Bbbb Abb-Bbb Ab-Bb A-B A#-B# A##-B## A###-B### Bbbb-Cbb Bbb-Cb Bb-C
      B-C# B#-C## B##-C### Cbbb-Dbbb Cbb-Dbb Cb-Db C-D C#-D# C##-D## C###-D###
      Dbbb-Ebbb Dbb-Ebb Db-Eb D-E D#-E# D##-E## D###-E### Ebbb-Fbb Ebb-Fb Eb-F
      E-F# E#-F## E##-F### Fbbb-Gbbb Fbb-Gbb Fb-Gb F-G F#-G# F##-G## F###-G###
      Gbbb-Abbb Gbb-Abb Gb-Ab G-A G#-A# G##-A## G###-A###))
    (2 d3 "diminished third"
     (Abb-Cbbb Ab-Cbb A-Cb A#-C A##-C# A###-C## Bbb-Dbbb Bb-Dbb B-Db B#-D B##-D#
      B###-D## Cb-Ebbb C-Ebb C#-Eb C##-E C###-E# Dbb-Fbbb Db-Fbb D-Fb D#-F D##-F#
      D###-F## Ebb-Gbbb Eb-Gbb E-Gb E#-G E##-G# E###-G## Fb-Abbb F-Abb F#-Ab
      F##-A F###-A# Gb-Bbbb G-Bbb G#-Bb G##-B G###-B#))
    (3 A2 "augmented second"
     (Abbb-Bbb Abb-Bb Ab-B A-B# A#-B## A##-B### Bbbb-Cb Bbb-C Bb-C# B-C## B#-C###
      Cbbb-Dbb Cbb-Db Cb-D C-D# C#-D## C##-D### Dbbb-Ebb Dbb-Eb Db-E D-E# D#-E##
      D##-E### Ebbb-Fb Ebb-F Eb-F# E-F## E#-F### Fbbb-Gbb Fbb-Gb Fb-G F-G# F#-G##
      F##-G### Gbbb-Abb Gbb-Ab Gb-A G-A# G#-A## G##-A###))
    (3 m3 "minor third"
     (Abbb-Cbbb Abb-Cbb Ab-Cb A-C A#-C# A##-C## A###-C### Bbbb-Dbbb Bbb-Dbb Bb-Db
      B-D B#-D# B##-D## B###-D### Cbb-Ebbb Cb-Ebb C-Eb C#-E C##-E# C###-E##
      Dbbb-Fbbb Dbb-Fbb Db-Fb D-F D#-F# D##-F## D###-F### Ebbb-Gbbb Ebb-Gbb Eb-Gb
      E-G E#-G# E##-G## E###-G### Fbb-Abbb Fb-Abb F-Ab F#-A F##-A# F###-A##
      Gbb-Bbbb Gb-Bbb G-Bb G#-B G##-B# G###-B##))
    (3 dd4 "doubly diminished fourth"
     (Ab-Dbbb A-Dbb A#-Db A##-D A###-D# Bb-Ebbb B-Ebb B#-Eb B##-E B###-E# Cb-Fbbb
      C-Fbb C#-Fb C##-F C###-F# Db-Gbbb D-Gbb D#-Gb D##-G D###-G# Eb-Abbb E-Abb
      E#-Ab E##-A E###-A# F-Bbbb F#-Bbb F##-Bb F###-B Gb-Cbbb G-Cbb G#-Cb G##-C
      G###-C#))
    (4 AA2 "doubly augmented second"
     (Abbb-Bb Abb-B Ab-B# A-B## A#-B### Bbbb-C Bbb-C# Bb-C## B-C### Cbbb-Db Cbb-D
      Cb-D# C-D## C#-D### Dbbb-Eb Dbb-E Db-E# D-E## D#-E### Ebbb-F Ebb-F# Eb-F##
      E-F### Fbbb-Gb Fbb-G Fb-G# F-G## F#-G### Gbbb-Ab Gbb-A Gb-A# G-A## G#-A###))
    (4 M3 "major third"
     (Abbb-Cbb Abb-Cb Ab-C A-C# A#-C## A##-C### Bbbb-Dbb Bbb-Db Bb-D B-D# B#-D##
      B##-D### Cbbb-Ebbb Cbb-Ebb Cb-Eb C-E C#-E# C##-E## C###-E### Dbbb-Fbb
      Dbb-Fb Db-F D-F# D#-F## D##-F### Ebbb-Gbb Ebb-Gb Eb-G E-G# E#-G## E##-G###
      Fbbb-Abbb Fbb-Abb Fb-Ab F-A F#-A# F##-A## F###-A### Gbbb-Bbbb Gbb-Bbb Gb-Bb
      G-B G#-B# G##-B## G###-B###))
    (4 d4 "diminished fourth"
     (Abb-Dbbb Ab-Dbb A-Db A#-D A##-D# A###-D## Bbb-Ebbb Bb-Ebb B-Eb B#-E B##-E#
      B###-E## Cbb-Fbbb Cb-Fbb C-Fb C#-F C##-F# C###-F## Dbb-Gbbb Db-Gbb D-Gb
      D#-G D##-G# D###-G## Ebb-Abbb Eb-Abb E-Ab E#-A E##-A# E###-A## Fb-Bbbb
      F-Bbb F#-Bb F##-B F###-B# Gbb-Cbbb Gb-Cbb G-Cb G#-C G##-C# G###-C##))
    (5 A3 "augmented third"
     (Abbb-Cb Abb-C Ab-C# A-C## A#-C### Bbbb-Db Bbb-D Bb-D# B-D## B#-D###
      Cbbb-Ebb Cbb-Eb Cb-E C-E# C#-E## C##-E### Dbbb-Fb Dbb-F Db-F# D-F## D#-F###
      Ebbb-Gb Ebb-G Eb-G# E-G## E#-G### Fbbb-Abb Fbb-Ab Fb-A F-A# F#-A## F##-A###
      Gbbb-Bbb Gbb-Bb Gb-B G-B# G#-B## G##-B###))
    (5 P4 fourth
     (Abbb-Dbbb Abb-Dbb Ab-Db A-D A#-D# A##-D## A###-D### Bbbb-Ebbb Bbb-Ebb Bb-Eb
      B-E B#-E# B##-E## B###-E### Cbbb-Fbbb Cbb-Fbb Cb-Fb C-F C#-F# C##-F##
      C###-F### Dbbb-Gbbb Dbb-Gbb Db-Gb D-G D#-G# D##-G## D###-G### Ebbb-Abbb
      Ebb-Abb Eb-Ab E-A E#-A# E##-A## E###-A### Fbb-Bbbb Fb-Bbb F-Bb F#-B F##-B#
      F###-B## Gbbb-Cbbb Gbb-Cbb Gb-Cb G-C G#-C# G##-C## G###-C###))
    (5 dd5 "doubly diminished fifth"
     (Ab-Ebbb A-Ebb A#-Eb A##-E A###-E# Bbb-Fbbb Bb-Fbb B-Fb B#-F B##-F# B###-F##
      Cb-Gbbb C-Gbb C#-Gb C##-G C###-G# Db-Abbb D-Abb D#-Ab D##-A D###-A# Eb-Bbbb
      E-Bbb E#-Bb E##-B E###-B# Fb-Cbbb F-Cbb F#-Cb F##-C F###-C# Gb-Dbbb G-Dbb
      G#-Db G##-D G###-D#))
    (6 AA3 "doubly augmented third"
     (Abbb-C Abb-C# Ab-C## A-C### Bbbb-D Bbb-D# Bb-D## B-D### Cbbb-Eb Cbb-E Cb-E#
      C-E## C#-E### Dbbb-F Dbb-F# Db-F## D-F### Ebbb-G Ebb-G# Eb-G## E-G###
      Fbbb-Ab Fbb-A Fb-A# F-A## F#-A### Gbbb-Bb Gbb-B Gb-B# G-B## G#-B###))
    (6 A4 "augmented fourth"
     (Abbb-Dbb Abb-Db Ab-D A-D# A#-D## A##-D### Bbbb-Ebb Bbb-Eb Bb-E B-E# B#-E##
      B##-E### Cbbb-Fbb Cbb-Fb Cb-F C-F# C#-F## C##-F### Dbbb-Gbb Dbb-Gb Db-G
      D-G# D#-G## D##-G### Ebbb-Abb Ebb-Ab Eb-A E-A# E#-A## E##-A### Fbbb-Bbbb
      Fbb-Bbb Fb-Bb F-B F#-B# F##-B## F###-B### Gbbb-Cbb Gbb-Cb Gb-C G-C# G#-C##
      G##-C###))
    (6 d5 "diminished fifth"
     (Abb-Ebbb Ab-Ebb A-Eb A#-E A##-E# A###-E## Bbbb-Fbbb Bbb-Fbb Bb-Fb B-F B#-F#
      B##-F## B###-F### Cbb-Gbbb Cb-Gbb C-Gb C#-G C##-G# C###-G## Dbb-Abbb Db-Abb
      D-Ab D#-A D##-A# D###-A## Ebb-Bbbb Eb-Bbb E-Bb E#-B E##-B# E###-B##
      Fbb-Cbbb Fb-Cbb F-Cb F#-C F##-C# F###-C## Gbb-Dbbb Gb-Dbb G-Db G#-D G##-D#
      G###-D##))
    (6 dd6 "doubly diminished sixth"
     (Ab-Fbbb A-Fbb A#-Fb A##-F A###-F# Bb-Gbbb B-Gbb B#-Gb B##-G B###-G# C-Abbb
      C#-Abb C##-Ab C###-A D-Bbbb D#-Bbb D##-Bb D###-B Eb-Cbbb E-Cbb E#-Cb E##-C
      E###-C# F-Dbbb F#-Dbb F##-Db F###-D G-Ebbb G#-Ebb G##-Eb G###-E))
    (7 AA4 "doubly augmented fourth"
     (Abbb-Db Abb-D Ab-D# A-D## A#-D### Bbbb-Eb Bbb-E Bb-E# B-E## B#-E### Cbbb-Fb
      Cbb-F Cb-F# C-F## C#-F### Dbbb-Gb Dbb-G Db-G# D-G## D#-G### Ebbb-Ab Ebb-A
      Eb-A# E-A## E#-A### Fbbb-Bbb Fbb-Bb Fb-B F-B# F#-B## F##-B### Gbbb-Cb Gbb-C
      Gb-C# G-C## G#-C###))
    (7 P5 fifth
     (Abbb-Ebbb Abb-Ebb Ab-Eb A-E A#-E# A##-E## A###-E### Bbbb-Fbb Bbb-Fb Bb-F
      B-F# B#-F## B##-F### Cbbb-Gbbb Cbb-Gbb Cb-Gb C-G C#-G# C##-G## C###-G###
      Dbbb-Abbb Dbb-Abb Db-Ab D-A D#-A# D##-A## D###-A### Ebbb-Bbbb Ebb-Bbb Eb-Bb
      E-B E#-B# E##-B## E###-B### Fbbb-Cbbb Fbb-Cbb Fb-Cb F-C F#-C# F##-C##
      F###-C### Gbbb-Dbbb Gbb-Dbb Gb-Db G-D G#-D# G##-D## G###-D###))
    (7 d6 "diminished sixth"
     (Abb-Fbbb Ab-Fbb A-Fb A#-F A##-F# A###-F## Bbb-Gbbb Bb-Gbb B-Gb B#-G B##-G#
      B###-G## Cb-Abbb C-Abb C#-Ab C##-A C###-A# Db-Bbbb D-Bbb D#-Bb D##-B
      D###-B# Ebb-Cbbb Eb-Cbb E-Cb E#-C E##-C# E###-C## Fb-Dbbb F-Dbb F#-Db F##-D
      F###-D# Gb-Ebbb G-Ebb G#-Eb G##-E G###-E#))
    (8 A5 "augmented fifth"
     (Abbb-Ebb Abb-Eb Ab-E A-E# A#-E## A##-E### Bbbb-Fb Bbb-F Bb-F# B-F## B#-F###
      Cbbb-Gbb Cbb-Gb Cb-G C-G# C#-G## C##-G### Dbbb-Abb Dbb-Ab Db-A D-A# D#-A##
      D##-A### Ebbb-Bbb Ebb-Bb Eb-B E-B# E#-B## E##-B### Fbbb-Cbb Fbb-Cb Fb-C
      F-C# F#-C## F##-C### Gbbb-Dbb Gbb-Db Gb-D G-D# G#-D## G##-D###))
    (8 m6 "minor sixth"
     (Abbb-Fbbb Abb-Fbb Ab-Fb A-F A#-F# A##-F## A###-F### Bbbb-Gbbb Bbb-Gbb Bb-Gb
      B-G B#-G# B##-G## B###-G### Cbb-Abbb Cb-Abb C-Ab C#-A C##-A# C###-A##
      Dbb-Bbbb Db-Bbb D-Bb D#-B D##-B# D###-B## Ebbb-Cbbb Ebb-Cbb Eb-Cb E-C E#-C#
      E##-C## E###-C### Fbb-Dbbb Fb-Dbb F-Db F#-D F##-D# F###-D## Gbb-Ebbb Gb-Ebb
      G-Eb G#-E G##-E# G###-E##))
    (8 dd7 "doubly diminished seventh"
     (Ab-Gbbb A-Gbb A#-Gb A##-G A###-G# Bb-Abbb B-Abb B#-Ab B##-A B###-A# C-Bbbb
      C#-Bbb C##-Bb C###-B Db-Cbbb D-Cbb D#-Cb D##-C D###-C# Eb-Dbbb E-Dbb E#-Db
      E##-D E###-D# F-Ebbb F#-Ebb F##-Eb F###-E Gb-Fbbb G-Fbb G#-Fb G##-F
      G###-F#))
    (9 AA5 "doubly augmented fifth"
     (Abbb-Eb Abb-E Ab-E# A-E## A#-E### Bbbb-F Bbb-F# Bb-F## B-F### Cbbb-Gb Cbb-G
      Cb-G# C-G## C#-G### Dbbb-Ab Dbb-A Db-A# D-A## D#-A### Ebbb-Bb Ebb-B Eb-B#
      E-B## E#-B### Fbbb-Cb Fbb-C Fb-C# F-C## F#-C### Gbbb-Db Gbb-D Gb-D# G-D##
      G#-D###))
    (9 M6 "major sixth"
     (Abbb-Fbb Abb-Fb Ab-F A-F# A#-F## A##-F### Bbbb-Gbb Bbb-Gb Bb-G B-G# B#-G##
      B##-G### Cbbb-Abbb Cbb-Abb Cb-Ab C-A C#-A# C##-A## C###-A### Dbbb-Bbbb
      Dbb-Bbb Db-Bb D-B D#-B# D##-B## D###-B### Ebbb-Cbb Ebb-Cb Eb-C E-C# E#-C##
      E##-C### Fbbb-Dbbb Fbb-Dbb Fb-Db F-D F#-D# F##-D## F###-D### Gbbb-Ebbb
      Gbb-Ebb Gb-Eb G-E G#-E# G##-E## G###-E###))
    (9 d7 "diminished seventh"
     (Abb-Gbbb Ab-Gbb A-Gb A#-G A##-G# A###-G## Bbb-Abbb Bb-Abb B-Ab B#-A B##-A#
      B###-A## Cb-Bbbb C-Bbb C#-Bb C##-B C###-B# Dbb-Cbbb Db-Cbb D-Cb D#-C D##-C#
      D###-C## Ebb-Dbbb Eb-Dbb E-Db E#-D E##-D# E###-D## Fb-Ebbb F-Ebb F#-Eb
      F##-E F###-E# Gbb-Fbbb Gb-Fbb G-Fb G#-F G##-F# G###-F##))
    (10 A6 "augmented sixth"
     (Abbb-Fb Abb-F Ab-F# A-F## A#-F### Bbbb-Gb Bbb-G Bb-G# B-G## B#-G###
      Cbbb-Abb Cbb-Ab Cb-A C-A# C#-A## C##-A### Dbbb-Bbb Dbb-Bb Db-B D-B# D#-B##
      D##-B### Ebbb-Cb Ebb-C Eb-C# E-C## E#-C### Fbbb-Dbb Fbb-Db Fb-D F-D# F#-D##
      F##-D### Gbbb-Ebb Gbb-Eb Gb-E G-E# G#-E## G##-E###))
    (10 m7 "minor seventh"
     (Abbb-Gbbb Abb-Gbb Ab-Gb A-G A#-G# A##-G## A###-G### Bbbb-Abbb Bbb-Abb Bb-Ab
      B-A B#-A# B##-A## B###-A### Cbb-Bbbb Cb-Bbb C-Bb C#-B C##-B# C###-B##
      Dbbb-Cbbb Dbb-Cbb Db-Cb D-C D#-C# D##-C## D###-C### Ebbb-Dbbb Ebb-Dbb Eb-Db
      E-D E#-D# E##-D## E###-D### Fbb-Ebbb Fb-Ebb F-Eb F#-E F##-E# F###-E##
      Gbbb-Fbbb Gbb-Fbb Gb-Fb G-F G#-F# G##-F## G###-F###))
    (10 dd8 "doubly diminished octave"
     (Ab-Abbb A-Abb A#-Ab A##-A A###-A# Bb-Bbbb B-Bbb B#-Bb B##-B B###-B# Cb-Cbbb
      C-Cbb C#-Cb C##-C C###-C# Db-Dbbb D-Dbb D#-Db D##-D D###-D# Eb-Ebbb E-Ebb
      E#-Eb E##-E E###-E# Fb-Fbbb F-Fbb F#-Fb F##-F F###-F# Gb-Gbbb G-Gbb G#-Gb
      G##-G G###-G#))
    (11 AA6 "doubly augmented sixth"
     (Abbb-F Abb-F# Ab-F## A-F### Bbbb-G Bbb-G# Bb-G## B-G### Cbbb-Ab Cbb-A Cb-A#
      C-A## C#-A### Dbbb-Bb Dbb-B Db-B# D-B## D#-B### Ebbb-C Ebb-C# Eb-C## E-C###
      Fbbb-Db Fbb-D Fb-D# F-D## F#-D### Gbbb-Eb Gbb-E Gb-E# G-E## G#-E###))
    (11 M7 "major seventh"
     (Abbb-Gbb Abb-Gb Ab-G A-G# A#-G## A##-G### Bbbb-Abb Bbb-Ab Bb-A B-A# B#-A##
      B##-A### Cbbb-Bbbb Cbb-Bbb Cb-Bb C-B C#-B# C##-B## C###-B### Dbbb-Cbb
      Dbb-Cb Db-C D-C# D#-C## D##-C### Ebbb-Dbb Ebb-Db Eb-D E-D# E#-D## E##-D###
      Fbbb-Ebbb Fbb-Ebb Fb-Eb F-E F#-E# F##-E## F###-E### Gbbb-Fbb Gbb-Fb Gb-F
      G-F# G#-F## G##-F###))
    (11 d8 "diminished octave"
     (Abb-Abbb Ab-Abb A-Ab A#-A A##-A# A###-A## Bbb-Bbbb Bb-Bbb B-Bb B#-B B##-B#
      B###-B## Cbb-Cbbb Cb-Cbb C-Cb C#-C C##-C# C###-C## Dbb-Dbbb Db-Dbb D-Db
      D#-D D##-D# D###-D## Ebb-Ebbb Eb-Ebb E-Eb E#-E E##-E# E###-E## Fbb-Fbbb
      Fb-Fbb F-Fb F#-F F##-F# F###-F## Gbb-Gbbb Gb-Gbb G-Gb G#-G G##-G# G###-G##))
    (11 dd9 "doubly diminished ninth"
     (A-Bbbb A#-Bbb A##-Bb A###-B Bb-Cbbb B-Cbb B#-Cb B##-C B###-C# C-Dbbb C#-Dbb
      C##-Db C###-D D-Ebbb D#-Ebb D##-Eb D###-E Eb-Fbbb E-Fbb E#-Fb E##-F E###-F#
      F-Gbbb F#-Gbb F##-Gb F###-G G-Abbb G#-Abb G##-Ab G###-A))
    (12 A7 "augmented seventh"
     (Abbb-Gb Abb-G Ab-G# A-G## A#-G### Bbbb-Ab Bbb-A Bb-A# B-A## B#-A###
      Cbbb-Bbb Cbb-Bb Cb-B C-B# C#-B## C##-B### Dbbb-Cb Dbb-C Db-C# D-C## D#-C###
      Ebbb-Db Ebb-D Eb-D# E-D## E#-D### Fbbb-Ebb Fbb-Eb Fb-E F-E# F#-E## F##-E###
      Gbbb-Fb Gbb-F Gb-F# G-F## G#-F###))
    (12 P8 octave
     (Abbb-Abbb Abb-Abb Ab-Ab A-A A#-A# A##-A## A###-A### Bbbb-Bbbb Bbb-Bbb Bb-Bb
      B-B B#-B# B##-B## B###-B### Cbbb-Cbbb Cbb-Cbb Cb-Cb C-C C#-C# C##-C##
      C###-C### Dbbb-Dbbb Dbb-Dbb Db-Db D-D D#-D# D##-D## D###-D### Ebbb-Ebbb
      Ebb-Ebb Eb-Eb E-E E#-E# E##-E## E###-E### Fbbb-Fbbb Fbb-Fbb Fb-Fb F-F F#-F#
      F##-F## F###-F### Gbbb-Gbbb Gbb-Gbb Gb-Gb G-G G#-G# G##-G## G###-G###))
    (12 d9 "diminished ninth"
     (Ab-Bbbb A-Bbb A#-Bb A##-B A###-B# Bbb-Cbbb Bb-Cbb B-Cb B#-C B##-C# B###-C##
      Cb-Dbbb C-Dbb C#-Db C##-D C###-D# Db-Ebbb D-Ebb D#-Eb D##-E D###-E#
      Ebb-Fbbb Eb-Fbb E-Fb E#-F E##-F# E###-F## Fb-Gbbb F-Gbb F#-Gb F##-G F###-G#
      Gb-Abbb G-Abb G#-Ab G##-A G###-A#))
    (13 AA7 "doubly augmented seventh"
     (Abbb-G Abb-G# Ab-G## A-G### Bbbb-A Bbb-A# Bb-A## B-A### Cbbb-Bb Cbb-B Cb-B#
      C-B## C#-B### Dbbb-C Dbb-C# Db-C## D-C### Ebbb-D Ebb-D# Eb-D## E-D###
      Fbbb-Eb Fbb-E Fb-E# F-E## F#-E### Gbbb-F Gbb-F# Gb-F## G-F###))
    (13 A8 "augmented octave"
     (Abbb-Abb Abb-Ab Ab-A A-A# A#-A## A##-A### Bbbb-Bbb Bbb-Bb Bb-B B-B# B#-B##
      B##-B### Cbbb-Cbb Cbb-Cb Cb-C C-C# C#-C## C##-C### Dbbb-Dbb Dbb-Db Db-D
      D-D# D#-D## D##-D### Ebbb-Ebb Ebb-Eb Eb-E E-E# E#-E## E##-E### Fbbb-Fbb
      Fbb-Fb Fb-F F-F# F#-F## F##-F### Gbbb-Gbb Gbb-Gb Gb-G G-G# G#-G## G##-G###))
    (13 m9 "minor ninth"
     (Abb-Bbbb Ab-Bbb A-Bb A#-B A##-B# A###-B## Bbbb-Cbbb Bbb-Cbb Bb-Cb B-C B#-C#
      B##-C## B###-C### Cbb-Dbbb Cb-Dbb C-Db C#-D C##-D# C###-D## Dbb-Ebbb Db-Ebb
      D-Eb D#-E D##-E# D###-E## Ebbb-Fbbb Ebb-Fbb Eb-Fb E-F E#-F# E##-F##
      E###-F### Fbb-Gbbb Fb-Gbb F-Gb F#-G F##-G# F###-G## Gbb-Abbb Gb-Abb G-Ab
      G#-A G##-A# G###-A##))
    (13 dd10 "doubly diminished tenth"
     (Ab-Cbbb A-Cbb A#-Cb A##-C A###-C# Bb-Dbbb B-Dbb B#-Db B##-D B###-D# C-Ebbb
      C#-Ebb C##-Eb C###-E Db-Fbbb D-Fbb D#-Fb D##-F D###-F# Eb-Gbbb E-Gbb E#-Gb
      E##-G E###-G# F-Abbb F#-Abb F##-Ab F###-A G-Bbbb G#-Bbb G##-Bb G###-B))
    (14 AA8 "doubly augmented octave"
     (Abbb-Ab Abb-A Ab-A# A-A## A#-A### Bbbb-Bb Bbb-B Bb-B# B-B## B#-B### Cbbb-Cb
      Cbb-C Cb-C# C-C## C#-C### Dbbb-Db Dbb-D Db-D# D-D## D#-D### Ebbb-Eb Ebb-E
      Eb-E# E-E## E#-E### Fbbb-Fb Fbb-F Fb-F# F-F## F#-F### Gbbb-Gb Gbb-G Gb-G#
      G-G## G#-G###))
    (14 M9 "major ninth"
     (Abbb-Bbbb Abb-Bbb Ab-Bb A-B A#-B# A##-B## A###-B### Bbbb-Cbb Bbb-Cb Bb-C
      B-C# B#-C## B##-C### Cbbb-Dbbb Cbb-Dbb Cb-Db C-D C#-D# C##-D## C###-D###
      Dbbb-Ebbb Dbb-Ebb Db-Eb D-E D#-E# D##-E## D###-E### Ebbb-Fbb Ebb-Fb Eb-F
      E-F# E#-F## E##-F### Fbbb-Gbbb Fbb-Gbb Fb-Gb F-G F#-G# F##-G## F###-G###
      Gbbb-Abbb Gbb-Abb Gb-Ab G-A G#-A# G##-A## G###-A###))
    (14 d10 "diminished tenth"
     (Abb-Cbbb Ab-Cbb A-Cb A#-C A##-C# A###-C## Bbb-Dbbb Bb-Dbb B-Db B#-D B##-D#
      B###-D## Cb-Ebbb C-Ebb C#-Eb C##-E C###-E# Dbb-Fbbb Db-Fbb D-Fb D#-F D##-F#
      D###-F## Ebb-Gbbb Eb-Gbb E-Gb E#-G E##-G# E###-G## Fb-Abbb F-Abb F#-Ab
      F##-A F###-A# Gb-Bbbb G-Bbb G#-Bb G##-B G###-B#))
    (15 A9 "augmented ninth"
     (Abbb-Bbb Abb-Bb Ab-B A-B# A#-B## A##-B### Bbbb-Cb Bbb-C Bb-C# B-C## B#-C###
      Cbbb-Dbb Cbb-Db Cb-D C-D# C#-D## C##-D### Dbbb-Ebb Dbb-Eb Db-E D-E# D#-E##
      D##-E### Ebbb-Fb Ebb-F Eb-F# E-F## E#-F### Fbbb-Gbb Fbb-Gb Fb-G F-G# F#-G##
      F##-G### Gbbb-Abb Gbb-Ab Gb-A G-A# G#-A## G##-A###))
    (15 m10 "minor tenth"
     (Abbb-Cbbb Abb-Cbb Ab-Cb A-C A#-C# A##-C## A###-C### Bbbb-Dbbb Bbb-Dbb Bb-Db
      B-D B#-D# B##-D## B###-D### Cbb-Ebbb Cb-Ebb C-Eb C#-E C##-E# C###-E##
      Dbbb-Fbbb Dbb-Fbb Db-Fb D-F D#-F# D##-F## D###-F### Ebbb-Gbbb Ebb-Gbb Eb-Gb
      E-G E#-G# E##-G## E###-G### Fbb-Abbb Fb-Abb F-Ab F#-A F##-A# F###-A##
      Gbb-Bbbb Gb-Bbb G-Bb G#-B G##-B# G###-B##))
    (15 dd11 "doubly diminished eleventh"
     (Ab-Dbbb A-Dbb A#-Db A##-D A###-D# Bb-Ebbb B-Ebb B#-Eb B##-E B###-E# Cb-Fbbb
      C-Fbb C#-Fb C##-F C###-F# Db-Gbbb D-Gbb D#-Gb D##-G D###-G# Eb-Abbb E-Abb
      E#-Ab E##-A E###-A# F-Bbbb F#-Bbb F##-Bb F###-B Gb-Cbbb G-Cbb G#-Cb G##-C
      G###-C#))
    (16 AA9 "doubly augmented ninth"
     (Abbb-Bb Abb-B Ab-B# A-B## A#-B### Bbbb-C Bbb-C# Bb-C## B-C### Cbbb-Db Cbb-D
      Cb-D# C-D## C#-D### Dbbb-Eb Dbb-E Db-E# D-E## D#-E### Ebbb-F Ebb-F# Eb-F##
      E-F### Fbbb-Gb Fbb-G Fb-G# F-G## F#-G### Gbbb-Ab Gbb-A Gb-A# G-A## G#-A###))
    (16 M10 "major tenth"
     (Abbb-Cbb Abb-Cb Ab-C A-C# A#-C## A##-C### Bbbb-Dbb Bbb-Db Bb-D B-D# B#-D##
      B##-D### Cbbb-Ebbb Cbb-Ebb Cb-Eb C-E C#-E# C##-E## C###-E### Dbbb-Fbb
      Dbb-Fb Db-F D-F# D#-F## D##-F### Ebbb-Gbb Ebb-Gb Eb-G E-G# E#-G## E##-G###
      Fbbb-Abbb Fbb-Abb Fb-Ab F-A F#-A# F##-A## F###-A### Gbbb-Bbbb Gbb-Bbb Gb-Bb
      G-B G#-B# G##-B## G###-B###))
    (16 d11 "diminished eleventh"
     (Abb-Dbbb Ab-Dbb A-Db A#-D A##-D# A###-D## Bbb-Ebbb Bb-Ebb B-Eb B#-E B##-E#
      B###-E## Cbb-Fbbb Cb-Fbb C-Fb C#-F C##-F# C###-F## Dbb-Gbbb Db-Gbb D-Gb
      D#-G D##-G# D###-G## Ebb-Abbb Eb-Abb E-Ab E#-A E##-A# E###-A## Fb-Bbbb
      F-Bbb F#-Bb F##-B F###-B# Gbb-Cbbb Gb-Cbb G-Cb G#-C G##-C# G###-C##))
    (17 A10 "augmented tenth"
     (Abbb-Cb Abb-C Ab-C# A-C## A#-C### Bbbb-Db Bbb-D Bb-D# B-D## B#-D###
      Cbbb-Ebb Cbb-Eb Cb-E C-E# C#-E## C##-E### Dbbb-Fb Dbb-F Db-F# D-F## D#-F###
      Ebbb-Gb Ebb-G Eb-G# E-G## E#-G### Fbbb-Abb Fbb-Ab Fb-A F-A# F#-A## F##-A###
      Gbbb-Bbb Gbb-Bb Gb-B G-B# G#-B## G##-B###))
    (17 P11 eleventh
     (Abbb-Dbbb Abb-Dbb Ab-Db A-D A#-D# A##-D## A###-D### Bbbb-Ebbb Bbb-Ebb Bb-Eb
      B-E B#-E# B##-E## B###-E### Cbbb-Fbbb Cbb-Fbb Cb-Fb C-F C#-F# C##-F##
      C###-F### Dbbb-Gbbb Dbb-Gbb Db-Gb D-G D#-G# D##-G## D###-G### Ebbb-Abbb
      Ebb-Abb Eb-Ab E-A E#-A# E##-A## E###-A### Fbb-Bbbb Fb-Bbb F-Bb F#-B F##-B#
      F###-B## Gbbb-Cbbb Gbb-Cbb Gb-Cb G-C G#-C# G##-C## G###-C###))
    (17 dd12 "doubly diminished twelfth"
     (Ab-Ebbb A-Ebb A#-Eb A##-E A###-E# Bbb-Fbbb Bb-Fbb B-Fb B#-F B##-F# B###-F##
      Cb-Gbbb C-Gbb C#-Gb C##-G C###-G# Db-Abbb D-Abb D#-Ab D##-A D###-A# Eb-Bbbb
      E-Bbb E#-Bb E##-B E###-B# Fb-Cbbb F-Cbb F#-Cb F##-C F###-C# Gb-Dbbb G-Dbb
      G#-Db G##-D G###-D#))
    (18 AA10 "doubly augmented tenth"
     (Abbb-C Abb-C# Ab-C## A-C### Bbbb-D Bbb-D# Bb-D## B-D### Cbbb-Eb Cbb-E Cb-E#
      C-E## C#-E### Dbbb-F Dbb-F# Db-F## D-F### Ebbb-G Ebb-G# Eb-G## E-G###
      Fbbb-Ab Fbb-A Fb-A# F-A## F#-A### Gbbb-Bb Gbb-B Gb-B# G-B## G#-B###))
    (18 A11 "augmented eleventh"
     (Abbb-Dbb Abb-Db Ab-D A-D# A#-D## A##-D### Bbbb-Ebb Bbb-Eb Bb-E B-E# B#-E##
      B##-E### Cbbb-Fbb Cbb-Fb Cb-F C-F# C#-F## C##-F### Dbbb-Gbb Dbb-Gb Db-G
      D-G# D#-G## D##-G### Ebbb-Abb Ebb-Ab Eb-A E-A# E#-A## E##-A### Fbbb-Bbbb
      Fbb-Bbb Fb-Bb F-B F#-B# F##-B## F###-B### Gbbb-Cbb Gbb-Cb Gb-C G-C# G#-C##
      G##-C###))
    (18 d12 "diminished twelfth"
     (Abb-Ebbb Ab-Ebb A-Eb A#-E A##-E# A###-E## Bbbb-Fbbb Bbb-Fbb Bb-Fb B-F B#-F#
      B##-F## B###-F### Cbb-Gbbb Cb-Gbb C-Gb C#-G C##-G# C###-G## Dbb-Abbb Db-Abb
      D-Ab D#-A D##-A# D###-A## Ebb-Bbbb Eb-Bbb E-Bb E#-B E##-B# E###-B##
      Fbb-Cbbb Fb-Cbb F-Cb F#-C F##-C# F###-C## Gbb-Dbbb Gb-Dbb G-Db G#-D G##-D#
      G###-D##))
    (18 dd13 "doubly diminished thirteenth"
     (Ab-Fbbb A-Fbb A#-Fb A##-F A###-F# Bb-Gbbb B-Gbb B#-Gb B##-G B###-G# C-Abbb
      C#-Abb C##-Ab C###-A D-Bbbb D#-Bbb D##-Bb D###-B Eb-Cbbb E-Cbb E#-Cb E##-C
      E###-C# F-Dbbb F#-Dbb F##-Db F###-D G-Ebbb G#-Ebb G##-Eb G###-E))
    (19 AA11 "doubly augmented eleventh"
     (Abbb-Db Abb-D Ab-D# A-D## A#-D### Bbbb-Eb Bbb-E Bb-E# B-E## B#-E### Cbbb-Fb
      Cbb-F Cb-F# C-F## C#-F### Dbbb-Gb Dbb-G Db-G# D-G## D#-G### Ebbb-Ab Ebb-A
      Eb-A# E-A## E#-A### Fbbb-Bbb Fbb-Bb Fb-B F-B# F#-B## F##-B### Gbbb-Cb Gbb-C
      Gb-C# G-C## G#-C###))
    (19 P12 twelfth
     (Abbb-Ebbb Abb-Ebb Ab-Eb A-E A#-E# A##-E## A###-E### Bbbb-Fbb Bbb-Fb Bb-F
      B-F# B#-F## B##-F### Cbbb-Gbbb Cbb-Gbb Cb-Gb C-G C#-G# C##-G## C###-G###
      Dbbb-Abbb Dbb-Abb Db-Ab D-A D#-A# D##-A## D###-A### Ebbb-Bbbb Ebb-Bbb Eb-Bb
      E-B E#-B# E##-B## E###-B### Fbbb-Cbbb Fbb-Cbb Fb-Cb F-C F#-C# F##-C##
      F###-C### Gbbb-Dbbb Gbb-Dbb Gb-Db G-D G#-D# G##-D## G###-D###))
    (19 d13 "diminished thirteenth"
     (Abb-Fbbb Ab-Fbb A-Fb A#-F A##-F# A###-F## Bbb-Gbbb Bb-Gbb B-Gb B#-G B##-G#
      B###-G## Cb-Abbb C-Abb C#-Ab C##-A C###-A# Db-Bbbb D-Bbb D#-Bb D##-B
      D###-B# Ebb-Cbbb Eb-Cbb E-Cb E#-C E##-C# E###-C## Fb-Dbbb F-Dbb F#-Db F##-D
      F###-D# Gb-Ebbb G-Ebb G#-Eb G##-E G###-E#))
    (20 A12 "augmented twelfth"
     (Abbb-Ebb Abb-Eb Ab-E A-E# A#-E## A##-E### Bbbb-Fb Bbb-F Bb-F# B-F## B#-F###
      Cbbb-Gbb Cbb-Gb Cb-G C-G# C#-G## C##-G### Dbbb-Abb Dbb-Ab Db-A D-A# D#-A##
      D##-A### Ebbb-Bbb Ebb-Bb Eb-B E-B# E#-B## E##-B### Fbbb-Cbb Fbb-Cb Fb-C
      F-C# F#-C## F##-C### Gbbb-Dbb Gbb-Db Gb-D G-D# G#-D## G##-D###))
    (20 m13 "minor thirteenth"
     (Abbb-Fbbb Abb-Fbb Ab-Fb A-F A#-F# A##-F## A###-F### Bbbb-Gbbb Bbb-Gbb Bb-Gb
      B-G B#-G# B##-G## B###-G### Cbb-Abbb Cb-Abb C-Ab C#-A C##-A# C###-A##
      Dbb-Bbbb Db-Bbb D-Bb D#-B D##-B# D###-B## Ebbb-Cbbb Ebb-Cbb Eb-Cb E-C E#-C#
      E##-C## E###-C### Fbb-Dbbb Fb-Dbb F-Db F#-D F##-D# F###-D## Gbb-Ebbb Gb-Ebb
      G-Eb G#-E G##-E# G###-E##))
    (20 dd14 "doubly diminished fourteeth"
     (Ab-Gbbb A-Gbb A#-Gb A##-G A###-G# Bb-Abbb B-Abb B#-Ab B##-A B###-A# C-Bbbb
      C#-Bbb C##-Bb C###-B Db-Cbbb D-Cbb D#-Cb D##-C D###-C# Eb-Dbbb E-Dbb E#-Db
      E##-D E###-D# F-Ebbb F#-Ebb F##-Eb F###-E Gb-Fbbb G-Fbb G#-Fb G##-F
      G###-F#))
    (21 AA12 "doubly augmented twelfth"
     (Abbb-Eb Abb-E Ab-E# A-E## A#-E### Bbbb-F Bbb-F# Bb-F## B-F### Cbbb-Gb Cbb-G
      Cb-G# C-G## C#-G### Dbbb-Ab Dbb-A Db-A# D-A## D#-A### Ebbb-Bb Ebb-B Eb-B#
      E-B## E#-B### Fbbb-Cb Fbb-C Fb-C# F-C## F#-C### Gbbb-Db Gbb-D Gb-D# G-D##
      G#-D###))
    (21 M13 "major thirteenth"
     (Abbb-Fbb Abb-Fb Ab-F A-F# A#-F## A##-F### Bbbb-Gbb Bbb-Gb Bb-G B-G# B#-G##
      B##-G### Cbbb-Abbb Cbb-Abb Cb-Ab C-A C#-A# C##-A## C###-A### Dbbb-Bbbb
      Dbb-Bbb Db-Bb D-B D#-B# D##-B## D###-B### Ebbb-Cbb Ebb-Cb Eb-C E-C# E#-C##
      E##-C### Fbbb-Dbbb Fbb-Dbb Fb-Db F-D F#-D# F##-D## F###-D### Gbbb-Ebbb
      Gbb-Ebb Gb-Eb G-E G#-E# G##-E## G###-E###))
    (21 d14 "diminished fourteeth"
     (Abb-Gbbb Ab-Gbb A-Gb A#-G A##-G# A###-G## Bbb-Abbb Bb-Abb B-Ab B#-A B##-A#
      B###-A## Cb-Bbbb C-Bbb C#-Bb C##-B C###-B# Dbb-Cbbb Db-Cbb D-Cb D#-C D##-C#
      D###-C## Ebb-Dbbb Eb-Dbb E-Db E#-D E##-D# E###-D## Fb-Ebbb F-Ebb F#-Eb
      F##-E F###-E# Gbb-Fbbb Gb-Fbb G-Fb G#-F G##-F# G###-F##))
    (22 A13 "augmented thirteenth"
     (Abbb-Fb Abb-F Ab-F# A-F## A#-F### Bbbb-Gb Bbb-G Bb-G# B-G## B#-G###
      Cbbb-Abb Cbb-Ab Cb-A C-A# C#-A## C##-A### Dbbb-Bbb Dbb-Bb Db-B D-B# D#-B##
      D##-B### Ebbb-Cb Ebb-C Eb-C# E-C## E#-C### Fbbb-Dbb Fbb-Db Fb-D F-D# F#-D##
      F##-D### Gbbb-Ebb Gbb-Eb Gb-E G-E# G#-E## G##-E###))
    (22 m14 "minor fourteeth"
     (Abbb-Gbbb Abb-Gbb Ab-Gb A-G A#-G# A##-G## A###-G### Bbbb-Abbb Bbb-Abb Bb-Ab
      B-A B#-A# B##-A## B###-A### Cbb-Bbbb Cb-Bbb C-Bb C#-B C##-B# C###-B##
      Dbbb-Cbbb Dbb-Cbb Db-Cb D-C D#-C# D##-C## D###-C### Ebbb-Dbbb Ebb-Dbb Eb-Db
      E-D E#-D# E##-D## E###-D### Fbb-Ebbb Fb-Ebb F-Eb F#-E F##-E# F###-E##
      Gbbb-Fbbb Gbb-Fbb Gb-Fb G-F G#-F# G##-F## G###-F###))
    (22 dd15 "doubly diminished double octave"
     (Ab-Abbb A-Abb A#-Ab A##-A A###-A# Bb-Bbbb B-Bbb B#-Bb B##-B B###-B# Cb-Cbbb
      C-Cbb C#-Cb C##-C C###-C# Db-Dbbb D-Dbb D#-Db D##-D D###-D# Eb-Ebbb E-Ebb
      E#-Eb E##-E E###-E# Fb-Fbbb F-Fbb F#-Fb F##-F F###-F# Gb-Gbbb G-Gbb G#-Gb
      G##-G G###-G#))
    (23 AA13 "doubly augmented thirteenth"
     (Abbb-F Abb-F# Ab-F## A-F### Bbbb-G Bbb-G# Bb-G## B-G### Cbbb-Ab Cbb-A Cb-A#
      C-A## C#-A### Dbbb-Bb Dbb-B Db-B# D-B## D#-B### Ebbb-C Ebb-C# Eb-C## E-C###
      Fbbb-Db Fbb-D Fb-D# F-D## F#-D### Gbbb-Eb Gbb-E Gb-E# G-E## G#-E###))
    (23 M14 "major fourteeth"
     (Abbb-Gbb Abb-Gb Ab-G A-G# A#-G## A##-G### Bbbb-Abb Bbb-Ab Bb-A B-A# B#-A##
      B##-A### Cbbb-Bbbb Cbb-Bbb Cb-Bb C-B C#-B# C##-B## C###-B### Dbbb-Cbb
      Dbb-Cb Db-C D-C# D#-C## D##-C### Ebbb-Dbb Ebb-Db Eb-D E-D# E#-D## E##-D###
      Fbbb-Ebbb Fbb-Ebb Fb-Eb F-E F#-E# F##-E## F###-E### Gbbb-Fbb Gbb-Fb Gb-F
      G-F# G#-F## G##-F###))
    (23 d15 "diminished double octave"
     (Abb-Abbb Ab-Abb A-Ab A#-A A##-A# A###-A## Bbb-Bbbb Bb-Bbb B-Bb B#-B B##-B#
      B###-B## Cbb-Cbbb Cb-Cbb C-Cb C#-C C##-C# C###-C## Dbb-Dbbb Db-Dbb D-Db
      D#-D D##-D# D###-D## Ebb-Ebbb Eb-Ebb E-Eb E#-E E##-E# E###-E## Fbb-Fbbb
      Fb-Fbb F-Fb F#-F F##-F# F###-F## Gbb-Gbbb Gb-Gbb G-Gb G#-G G##-G# G###-G##))
    (24 A14 "augmented fourteeth"
     (Abbb-Gb Abb-G Ab-G# A-G## A#-G### Bbbb-Ab Bbb-A Bb-A# B-A## B#-A###
      Cbbb-Bbb Cbb-Bb Cb-B C-B# C#-B## C##-B### Dbbb-Cb Dbb-C Db-C# D-C## D#-C###
      Ebbb-Db Ebb-D Eb-D# E-D## E#-D### Fbbb-Ebb Fbb-Eb Fb-E F-E# F#-E## F##-E###
      Gbbb-Fb Gbb-F Gb-F# G-F## G#-F###))
    (25 AA14 "doubly augmented fourteeth"
     (Abbb-G Abb-G# Ab-G## A-G### Bbbb-A Bbb-A# Bb-A## B-A### Cbbb-Bb Cbb-B Cb-B#
      C-B## C#-B### Dbbb-C Dbb-C# Db-C## D-C### Ebbb-D Ebb-D# Eb-D## E-D###
      Fbbb-Eb Fbb-E Fb-E# F-E## F#-E### Gbbb-F Gbb-F# Gb-F## G-F###)) |}]
;;
