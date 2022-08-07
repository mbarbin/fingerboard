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
    (0 "perfect unison" (P1 d2))
    (1 "minor second" (A1 m2 dd3))
    (2 "major second" (AA1 M2 d3))
    (3 "minor third" (A2 m3 dd4))
    (4 "major third" (AA2 M3 d4))
    (5 "perfect fourth" (A3 P4 dd5))
    (6 "augmented fourth" (AA3 A4 d5 dd6))
    (7 "perfect fifth" (AA4 P5 d6))
    (8 "minor sixth" (A5 m6 dd7))
    (9 "major sixth" (AA5 M6 d7))
    (10 "minor seventh" (A6 m7 dd8))
    (11 "major seventh" (AA6 M7 d8 dd9))
    (12 "perfect octave" (A7 P8 d9))
    (13 "minor ninth" (AA7 A8 m9 dd10))
    (14 "major ninth" (AA8 M9 d10))
    (15 "minor tenth" (A9 m10 dd11))
    (16 "major tenth" (AA9 M10 d11))
    (17 "perfect eleventh" (A10 P11 dd12))
    (18 "augmented eleventh" (AA10 A11 d12 dd13))
    (19 "perfect twelfth" (AA11 P12 d13))
    (20 "minor thirteenth" (A12 m13 dd14))
    (21 "major thirteenth" (AA12 M13 d14))
    (22 "minor fourteeth" (A13 m14 dd15))
    (23 "major fourteeth" (AA13 M14 d15))
    (24 "perfect double octave" (A14 P15)) |}]
;;
