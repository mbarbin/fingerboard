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

(* Exploration of edo systems.

   An edo-n system is a tuning system that divides the octave into [n] equal
   parts. Not all the [n] degrees are meant to be in use, rather only the steps
   that approximate the intervals we want to use (usually less than 30 per
   octave, sometimes less).

   In particular, we're interested in two things:

   - Finding programatically all meantone EDO with [n <= 100].

   - Knowning that edo-53 allows such a good approximation of just and
     pythagorean intervals, find it programatically, and determine whether there
     are other EDO systems that have similar properties, with [n <= 100].

   The purpose is to understand where do well-known edo systems come from, as
   well as making sure we're not forgetting important ones.

   The result of this study shows:

   - The meantones are: 12, 19, 31, 43, 50, 55.

   - 53 is the best non-meantone available to approximate pythagorean and just
     intervals, with a few others with lower number of divisions that are also
     potentially interesting: 41, 46, 34. *)

(* In this study we do not consider edo systems with a disivor greater than this
   upper limit.

   Changing this limit to [200] for example, shows:

   - no other meantone systems are found
   - EDO-118 (11, 18, 20) is a better non-meantone approximator than 53. *)
let divisor_upper_limit = 100

module Edo_system : sig
  (** In this study, we designate an edo system by a solution to the equation
      that says:

      {v
        3 Major Whole Tons + 2 Minor Whole Tons + 2 Diatonic Semitons
        =
        1 Octave
      v}

      Major and Minor Whole Tons may be the same interval, in which case we call
      this edo system a "meantone" system.

      A [t] values tells for each of the 3 intervals the number of divisions
      that will be used to approximate it within the system. *)
  type t =
    { diatonic_semi_ton : int
    ; minor_whole_ton : int
    ; major_whole_ton : int
    }
  [@@deriving equal]

  val to_string_hum : t -> string

  (** Return the divisor of that system. This is the [N] value in ["EDO-N"]. *)
  val divisor : t -> int

  (** Given a [t], all multiple of [t] are equivalent edo systems. [simplify t]
      returns the canonical represent of its class, that is one that may be
      not be further simplified. *)
  val simplify : t -> t
end = struct
  type t =
    { diatonic_semi_ton : int
    ; minor_whole_ton : int
    ; major_whole_ton : int
    }
  [@@deriving equal]

  let divisor { diatonic_semi_ton; minor_whole_ton; major_whole_ton } =
    (3 * major_whole_ton) + (2 * minor_whole_ton) + (2 * diatonic_semi_ton)
  ;;

  let%expect_test "divisor" =
    let t = { diatonic_semi_ton = 1; minor_whole_ton = 2; major_whole_ton = 2 } in
    print_dyn (divisor t |> Dyn.int);
    [%expect {| 12 |}];
    ()
  ;;

  let rec simplify t =
    let min = Int.min t.diatonic_semi_ton (Int.min t.minor_whole_ton t.major_whole_ton) in
    let rec loop i =
      if i > min
      then t
      else if
        t.diatonic_semi_ton % i = 0
        && t.minor_whole_ton % i = 0
        && t.major_whole_ton % i = 0
      then
        simplify
          { diatonic_semi_ton = t.diatonic_semi_ton / i
          ; minor_whole_ton = t.minor_whole_ton / i
          ; major_whole_ton = t.major_whole_ton / i
          }
      else loop (i + 1)
    in
    loop 2
  ;;

  let to_string_hum ({ diatonic_semi_ton; minor_whole_ton; major_whole_ton } as t) =
    let n = divisor t in
    Printf.sprintf
      "EDO-%2d (%2d, %2d, %2d)"
      n
      diatonic_semi_ton
      minor_whole_ton
      major_whole_ton
  ;;
end

module Reference_interval : sig
  (** In this study, we are going to compare how some reference intervals are approximated
      by different EDO systems. *)
  type t =
    | Octave
    | Pythagorean_major_sixth
    | Just_major_sixth
    | Just_minor_sixth
    | Pythagorean_minor_sixth
    | Fifth
    | Fourth
    | Pythagorean_major_third
    | Just_major_third
    | Just_minor_third
    | Pythagorean_minor_third
    | Pythagorean_major_second
    | Just_minor_ton
    | Pythagorean_chromatic_semiton
    | Just_diatonic_semiton
    | Pythagorean_diatonic_semiton
  [@@deriving enumerate, equal]

  val constructor_name : t -> string
  val acoustic_interval : t -> Acoustic_interval.t

  (** Returns the number of divisions that approximate the given interval the
      best within the given edo system. *)
  val edo_approximation : t -> edo_system:Edo_system.t -> int
end = struct
  type t =
    | Octave
    | Pythagorean_major_sixth
    | Just_major_sixth
    | Just_minor_sixth
    | Pythagorean_minor_sixth
    | Fifth
    | Fourth
    | Pythagorean_major_third
    | Just_major_third
    | Just_minor_third
    | Pythagorean_minor_third
    | Pythagorean_major_second
    | Just_minor_ton
    | Pythagorean_chromatic_semiton
    | Just_diatonic_semiton
    | Pythagorean_diatonic_semiton
  [@@deriving enumerate, equal]

  let constructor_name = function
    | Octave -> "Octave"
    | Pythagorean_major_sixth -> "Pythagorean_major_sixth"
    | Just_major_sixth -> "Just_major_sixth"
    | Just_minor_sixth -> "Just_minor_sixth"
    | Pythagorean_minor_sixth -> "Pythagorean_minor_sixth"
    | Fifth -> "Fifth"
    | Fourth -> "Fourth"
    | Pythagorean_major_third -> "Pythagorean_major_third"
    | Just_major_third -> "Just_major_third"
    | Just_minor_third -> "Just_minor_third"
    | Pythagorean_minor_third -> "Pythagorean_minor_third"
    | Pythagorean_major_second -> "Pythagorean_major_second"
    | Just_minor_ton -> "Just_minor_ton"
    | Pythagorean_chromatic_semiton -> "Pythagorean_chromatic_semiton"
    | Just_diatonic_semiton -> "Just_diatonic_semiton"
    | Pythagorean_diatonic_semiton -> "Pythagorean_diatonic_semiton"
  ;;

  let acoustic_interval = function
    | Octave -> Acoustic_interval.octave
    | Pythagorean_major_sixth ->
      Acoustic_interval.pythagorean
        { number = Sixth; quality = Major; additional_octaves = 0 }
    | Just_major_sixth -> Acoustic_interval.just_major_sixth
    | Just_minor_sixth -> Acoustic_interval.just_minor_sixth
    | Pythagorean_minor_sixth ->
      Acoustic_interval.pythagorean
        { number = Sixth; quality = Minor; additional_octaves = 0 }
    | Fifth ->
      Acoustic_interval.pythagorean
        { number = Fifth; quality = Perfect; additional_octaves = 0 }
    | Fourth ->
      Acoustic_interval.pythagorean
        { number = Fourth; quality = Perfect; additional_octaves = 0 }
    | Pythagorean_major_third ->
      Acoustic_interval.pythagorean
        { number = Third; quality = Major; additional_octaves = 0 }
    | Just_major_third -> Acoustic_interval.just_major_third
    | Just_minor_third -> Acoustic_interval.just_minor_third
    | Pythagorean_minor_third ->
      Acoustic_interval.pythagorean
        { number = Third; quality = Minor; additional_octaves = 0 }
    | Pythagorean_major_second ->
      Acoustic_interval.pythagorean
        { number = Second; quality = Major; additional_octaves = 0 }
    | Just_minor_ton -> Acoustic_interval.just_minor_ton
    | Pythagorean_chromatic_semiton -> Acoustic_interval.pythagorean_chromatic_semiton
    | Just_diatonic_semiton -> Acoustic_interval.just_diatonic_semiton
    | Pythagorean_diatonic_semiton -> Acoustic_interval.pythagorean_diatonic_semiton
  ;;

  let edo_approximation t ~edo_system =
    let divisor = Edo_system.divisor edo_system in
    let interval_in_cents = Acoustic_interval.to_cents (acoustic_interval t) in
    Float.iround_exn ~dir:`Nearest (Float.of_int divisor *. interval_in_cents /. 1200.)
  ;;
end

module Comparison_table : sig
  val print : Edo_system.t list -> unit
end = struct
  let make_comparison_column edo_system =
    let divisor = Edo_system.divisor edo_system in
    let name = Printf.sprintf "E%d" divisor in
    let divisor_length = String.length (Int.to_string divisor) in
    Print_table.Column.make ~align:Right ~header:name (fun (t : Reference_interval.t) ->
      let number_of_divisions = Reference_interval.edo_approximation t ~edo_system in
      let acoustic_interval =
        Acoustic_interval.equal_division_of_the_octave ~divisor ~number_of_divisions
      in
      let cents =
        Acoustic_interval.to_cents acoustic_interval |> Float.iround_exn ~dir:`Nearest
      in
      Print_table.Cell.text
        (Printf.sprintf "%4d - %*d" cents divisor_length number_of_divisions))
  ;;

  let print_group edo_systems =
    let columns =
      let exact_column =
        Print_table.Column.make
          ~align:Right
          ~header:"Exact"
          (fun (t : Reference_interval.t) ->
             Reference_interval.acoustic_interval t
             |> Acoustic_interval.to_cents
             |> Float.iround_exn ~dir:`Nearest
             |> Int.to_string
             |> fun i -> Print_table.Cell.text i)
      in
      Print_table.O.
        [ [ Column.make ~header:"Interval" (fun (t : Reference_interval.t) ->
              Cell.text
                (String.map (Reference_interval.constructor_name t) ~f:(function
                   | '_' -> ' '
                   | c -> c)))
          ]
        ; exact_column :: List.map edo_systems ~f:make_comparison_column
        ]
      |> List.concat
    in
    Print_table.to_string_text (Print_table.make ~columns ~rows:Reference_interval.all)
    |> print_endline
  ;;

  let print edo_systems =
    let edo_systems = List.groupi edo_systems ~break:(fun i _ _ -> i % 3 = 0) in
    List.iter edo_systems ~f:print_group
  ;;
end

let approximate_number_of_divisions ~divisor ~acoustic_interval =
  let interval_in_cents = Acoustic_interval.to_cents acoustic_interval in
  Float.iround_exn ~dir:`Nearest (Float.of_int divisor *. interval_in_cents /. 1200.)
;;

let major_just_scale_intervals =
  [| Acoustic_interval.pythagorean
       { number = Second; quality = Major; additional_octaves = 0 }
   ; Acoustic_interval.just_major_third
   ; Acoustic_interval.pythagorean
       { number = Fourth; quality = Perfect; additional_octaves = 0 }
   ; Acoustic_interval.pythagorean
       { number = Fifth; quality = Perfect; additional_octaves = 0 }
   ; Acoustic_interval.just_major_sixth
   ; Acoustic_interval.compound
       [ Acoustic_interval.pythagorean
           { number = Fifth; quality = Perfect; additional_octaves = 0 }
       ; Acoustic_interval.just_major_third
       ]
  |]
;;

let is_raw_candidate n =
  let approximates =
    Array.map major_just_scale_intervals ~f:(fun acoustic_interval ->
      approximate_number_of_divisions ~divisor:n ~acoustic_interval)
  in
  let intervals =
    Array.mapi approximates ~f:(fun i n -> n - if i = 0 then 0 else approximates.(i - 1))
    |> Array.to_list
    |> List.sort_then_dedup ~compare:Int.compare
  in
  let ( let* ) x f = Option.bind x ~f in
  let* candidate =
    (* We remove candidates that make use of more than 2 different kinds of Ton
       for this study. *)
    match intervals with
    | [ diatonic_semi_ton; whole_ton ] ->
      Some
        { Edo_system.diatonic_semi_ton
        ; minor_whole_ton = whole_ton
        ; major_whole_ton = whole_ton
        }
    | [ diatonic_semi_ton; minor_whole_ton; major_whole_ton ] ->
      Some { Edo_system.diatonic_semi_ton; minor_whole_ton; major_whole_ton }
    | [] | [ _ ] | _ :: _ :: _ :: _ -> None
  in
  let is_canonical = Edo_system.equal (Edo_system.simplify candidate) candidate in
  let divisor = Edo_system.divisor candidate in
  if
    divisor = n
    && is_canonical
    && 0 < candidate.diatonic_semi_ton
    && candidate.diatonic_semi_ton < candidate.minor_whole_ton
    && candidate.minor_whole_ton <= candidate.major_whole_ton
  then Some candidate
  else None
;;

let raw_candidates =
  List.filter_map (List.range 1 divisor_upper_limit) ~f:is_raw_candidate
;;

let meantones, non_meantones =
  List.partition raw_candidates ~f:(fun t -> t.minor_whole_ton = t.major_whole_ton)
;;

let%expect_test "meantones" =
  List.iter meantones ~f:(fun t -> print_endline (Edo_system.to_string_hum t));
  [%expect
    {|
    EDO-12 ( 1,  2,  2)
    EDO-19 ( 2,  3,  3)
    EDO-31 ( 3,  5,  5)
    EDO-43 ( 4,  7,  7)
    EDO-50 ( 5,  8,  8)
    EDO-55 ( 5,  9,  9)
     |}];
  Comparison_table.print meantones;
  [%expect
    {|
    ┌───────────────────────────────┬───────┬───────────┬───────────┬───────────┐
    │ Interval                      │ Exact │       E12 │       E19 │       E31 │
    ├───────────────────────────────┼───────┼───────────┼───────────┼───────────┤
    │ Octave                        │  1200 │ 1200 - 12 │ 1200 - 19 │ 1200 - 31 │
    │ Pythagorean major sixth       │   906 │  900 -  9 │  884 - 14 │  890 - 23 │
    │ Just major sixth              │   884 │  900 -  9 │  884 - 14 │  890 - 23 │
    │ Just minor sixth              │   814 │  800 -  8 │  821 - 13 │  813 - 21 │
    │ Pythagorean minor sixth       │   792 │  800 -  8 │  821 - 13 │  774 - 20 │
    │ Fifth                         │   702 │  700 -  7 │  695 - 11 │  697 - 18 │
    │ Fourth                        │   498 │  500 -  5 │  505 -  8 │  503 - 13 │
    │ Pythagorean major third       │   408 │  400 -  4 │  379 -  6 │  426 - 11 │
    │ Just major third              │   386 │  400 -  4 │  379 -  6 │  387 - 10 │
    │ Just minor third              │   316 │  300 -  3 │  316 -  5 │  310 -  8 │
    │ Pythagorean minor third       │   294 │  300 -  3 │  316 -  5 │  310 -  8 │
    │ Pythagorean major second      │   204 │  200 -  2 │  189 -  3 │  194 -  5 │
    │ Just minor ton                │   182 │  200 -  2 │  189 -  3 │  194 -  5 │
    │ Pythagorean chromatic semiton │   114 │  100 -  1 │  126 -  2 │  116 -  3 │
    │ Just diatonic semiton         │   112 │  100 -  1 │  126 -  2 │  116 -  3 │
    │ Pythagorean diatonic semiton  │    90 │  100 -  1 │   63 -  1 │   77 -  2 │
    └───────────────────────────────┴───────┴───────────┴───────────┴───────────┘

    ┌───────────────────────────────┬───────┬───────────┬───────────┬───────────┐
    │ Interval                      │ Exact │       E43 │       E50 │       E55 │
    ├───────────────────────────────┼───────┼───────────┼───────────┼───────────┤
    │ Octave                        │  1200 │ 1200 - 43 │ 1200 - 50 │ 1200 - 55 │
    │ Pythagorean major sixth       │   906 │  893 - 32 │  912 - 38 │  916 - 42 │
    │ Just major sixth              │   884 │  893 - 32 │  888 - 37 │  895 - 41 │
    │ Just minor sixth              │   814 │  809 - 29 │  816 - 34 │  807 - 37 │
    │ Pythagorean minor sixth       │   792 │  781 - 28 │  792 - 33 │  785 - 36 │
    │ Fifth                         │   702 │  698 - 25 │  696 - 29 │  698 - 32 │
    │ Fourth                        │   498 │  502 - 18 │  504 - 21 │  502 - 23 │
    │ Pythagorean major third       │   408 │  419 - 15 │  408 - 17 │  415 - 19 │
    │ Just major third              │   386 │  391 - 14 │  384 - 16 │  393 - 18 │
    │ Just minor third              │   316 │  307 - 11 │  312 - 13 │  305 - 14 │
    │ Pythagorean minor third       │   294 │  307 - 11 │  288 - 12 │  284 - 13 │
    │ Pythagorean major second      │   204 │  195 -  7 │  192 -  8 │  196 -  9 │
    │ Just minor ton                │   182 │  195 -  7 │  192 -  8 │  175 -  8 │
    │ Pythagorean chromatic semiton │   114 │  112 -  4 │  120 -  5 │  109 -  5 │
    │ Just diatonic semiton         │   112 │  112 -  4 │  120 -  5 │  109 -  5 │
    │ Pythagorean diatonic semiton  │    90 │   84 -  3 │   96 -  4 │   87 -  4 │
    └───────────────────────────────┴───────┴───────────┴───────────┴───────────┘ |}];
  ()
;;

(* In this context, we are interested in non-meantones edo system only insofar
   as they allow to approximate pythagorean and just intervals. This is the
   counterpart to allowing 2 different whole tons.

   Thus here we are going to do 2 things:

   1. Reject non-meantones that do not allow distinguishing all the intervals we
   are trying to approximate.

   2. Classify them according to how good the approximation is.
*)

let all_non_meantones =
  List.map non_meantones ~f:Edo_system.to_string_hum |> String.concat ~sep:"\n"
;;

let%expect_test "all non-meantones" =
  print_endline all_non_meantones;
  [%expect
    {|
    EDO-15 ( 1,  2,  3)
    EDO-22 ( 2,  3,  4)
    EDO-27 ( 2,  4,  5)
    EDO-29 ( 3,  4,  5)
    EDO-34 ( 3,  5,  6)
    EDO-39 ( 3,  6,  7)
    EDO-41 ( 4,  6,  7)
    EDO-46 ( 4,  7,  8)
    EDO-53 ( 5,  8,  9)
    EDO-56 ( 5,  8, 10)
    EDO-58 ( 5,  9, 10)
    EDO-60 ( 6,  9, 10)
    EDO-63 ( 6,  9, 11)
    EDO-65 ( 6, 10, 11)
    EDO-70 ( 6, 11, 12)
    EDO-72 ( 7, 11, 12)
    EDO-75 ( 7, 11, 13)
    EDO-77 ( 7, 12, 13)
    EDO-80 ( 7, 12, 14)
    EDO-84 ( 8, 13, 14)
    EDO-87 ( 8, 13, 15)
    EDO-89 ( 8, 14, 15)
    EDO-94 ( 9, 14, 16)
    EDO-96 ( 9, 15, 16)
    EDO-99 ( 9, 15, 17)
     |}];
  ()
;;

let allow_distinction_of_intervals_of_interest (edo_system : Edo_system.t) =
  let to_distinguish =
    Reference_interval.
      [| Pythagorean_major_sixth
       ; Just_major_sixth
       ; Just_minor_sixth
       ; Pythagorean_minor_sixth
       ; Pythagorean_major_third
       ; Just_major_third
       ; Just_minor_third
       ; Pythagorean_minor_third
      |]
  in
  let approximates =
    Array.map to_distinguish ~f:(fun interval ->
      Reference_interval.edo_approximation interval ~edo_system)
  in
  let number_of_approximations =
    approximates |> Array.to_list |> List.sort_then_dedup ~compare |> List.length
  in
  number_of_approximations = Array.length to_distinguish
;;

let non_meantones =
  non_meantones |> List.filter ~f:allow_distinction_of_intervals_of_interest
;;

let%expect_test "removed" =
  let filtered_non_meantones =
    List.map non_meantones ~f:Edo_system.to_string_hum |> String.concat ~sep:"\n"
  in
  Expect_test_patdiff.print_patdiff all_non_meantones filtered_non_meantones ~context:0;
  [%expect
    {|
    -1,3 +1,0
    -|EDO-15 ( 1,  2,  3)
    -|EDO-22 ( 2,  3,  4)
    -|EDO-27 ( 2,  4,  5)
    -6,1 +3,0
    -|EDO-39 ( 3,  6,  7) |}];
  ()
;;

let interval_error_in_cents n =
  let approximates =
    Array.map major_just_scale_intervals ~f:(fun acoustic_interval ->
      approximate_number_of_divisions ~divisor:n ~acoustic_interval)
  in
  Array.map2_exn
    major_just_scale_intervals
    approximates
    ~f:(fun interval number_of_divisions ->
      let approximation =
        Acoustic_interval.equal_division_of_the_octave ~divisor:n ~number_of_divisions
      in
      Float.abs
        (Acoustic_interval.to_cents interval -. Acoustic_interval.to_cents approximation))
;;

let mean_squarred_error n =
  let errors = interval_error_in_cents n |> Array.map ~f:(fun e -> e *. e) in
  Array.sum (module Float) errors ~f:Fn.id /. Float.of_int (Array.length errors)
;;

let non_meantones =
  non_meantones
  |> List.map ~f:(fun t ->
    let error = mean_squarred_error (Edo_system.divisor t) in
    error, t)
  |> List.sort ~compare:(Comparable.lift Float.compare ~f:fst)
;;

let%expect_test "non-meantones errors" =
  List.iter non_meantones ~f:(fun (e, t) ->
    print_endline (Printf.sprintf "%06.2f - %s" e (Edo_system.to_string_hum t)));
  [%expect
    {|
    001.00 - EDO-53 ( 5,  8,  9)
    001.18 - EDO-65 ( 6, 10, 11)
    002.77 - EDO-99 ( 9, 15, 17)
    002.98 - EDO-87 ( 8, 13, 15)
    005.28 - EDO-84 ( 8, 13, 14)
    005.60 - EDO-94 ( 9, 14, 16)
    005.80 - EDO-96 ( 9, 15, 16)
    006.01 - EDO-77 ( 7, 12, 13)
    008.25 - EDO-75 ( 7, 11, 13)
    009.54 - EDO-72 ( 7, 11, 12)
    011.96 - EDO-89 ( 8, 14, 15)
    017.28 - EDO-41 ( 4,  6,  7)
    019.16 - EDO-80 ( 7, 12, 14)
    020.09 - EDO-46 ( 4,  7,  8)
    022.41 - EDO-34 ( 3,  5,  6)
    024.88 - EDO-63 ( 6,  9, 11)
    025.03 - EDO-60 ( 6,  9, 10)
    026.02 - EDO-58 ( 5,  9, 10)
    031.13 - EDO-70 ( 6, 11, 12)
    036.06 - EDO-56 ( 5,  8, 10)
    099.58 - EDO-29 ( 3,  4,  5) |}];
  ()
;;

(* Now that we've established that edo-53 is the best approximator, we remove
   from the list edo that have a higher number of divisions, and have worst
   approximation.

   The ones with n < 53, albeit worst approximator, are kept for comparison and
   because they may be of interest in that they would offer simpler mental
   models (so, there is a potential tradeof). They need to be listened carefully
   to judge of their musical properties. *)

let non_meantones =
  let best_approximator = List.hd non_meantones |> snd |> Edo_system.divisor in
  List.filter non_meantones ~f:(fun (_, t) -> Edo_system.divisor t <= best_approximator)
;;

let%expect_test "non-meantones" =
  List.iter non_meantones ~f:(fun (e, t) ->
    print_endline (Printf.sprintf "%06.2f - %s" e (Edo_system.to_string_hum t)));
  [%expect
    {|
    001.00 - EDO-53 ( 5,  8,  9)
    017.28 - EDO-41 ( 4,  6,  7)
    020.09 - EDO-46 ( 4,  7,  8)
    022.41 - EDO-34 ( 3,  5,  6)
    099.58 - EDO-29 ( 3,  4,  5) |}];
  Comparison_table.print (List.map non_meantones ~f:snd);
  [%expect
    {|
    ┌───────────────────────────────┬───────┬───────────┬───────────┬───────────┐
    │ Interval                      │ Exact │       E53 │       E41 │       E46 │
    ├───────────────────────────────┼───────┼───────────┼───────────┼───────────┤
    │ Octave                        │  1200 │ 1200 - 53 │ 1200 - 41 │ 1200 - 46 │
    │ Pythagorean major sixth       │   906 │  906 - 40 │  907 - 31 │  913 - 35 │
    │ Just major sixth              │   884 │  883 - 39 │  878 - 30 │  887 - 34 │
    │ Just minor sixth              │   814 │  815 - 36 │  820 - 28 │  809 - 31 │
    │ Pythagorean minor sixth       │   792 │  792 - 35 │  790 - 27 │  783 - 30 │
    │ Fifth                         │   702 │  702 - 31 │  702 - 24 │  704 - 27 │
    │ Fourth                        │   498 │  498 - 22 │  498 - 17 │  496 - 19 │
    │ Pythagorean major third       │   408 │  408 - 18 │  410 - 14 │  417 - 16 │
    │ Just major third              │   386 │  385 - 17 │  380 - 13 │  391 - 15 │
    │ Just minor third              │   316 │  317 - 14 │  322 - 11 │  313 - 12 │
    │ Pythagorean minor third       │   294 │  294 - 13 │  293 - 10 │  287 - 11 │
    │ Pythagorean major second      │   204 │  204 -  9 │  205 -  7 │  209 -  8 │
    │ Just minor ton                │   182 │  181 -  8 │  176 -  6 │  183 -  7 │
    │ Pythagorean chromatic semiton │   114 │  113 -  5 │  117 -  4 │  104 -  4 │
    │ Just diatonic semiton         │   112 │  113 -  5 │  117 -  4 │  104 -  4 │
    │ Pythagorean diatonic semiton  │    90 │   91 -  4 │   88 -  3 │   78 -  3 │
    └───────────────────────────────┴───────┴───────────┴───────────┴───────────┘

    ┌───────────────────────────────┬───────┬───────────┬───────────┐
    │ Interval                      │ Exact │       E34 │       E29 │
    ├───────────────────────────────┼───────┼───────────┼───────────┤
    │ Octave                        │  1200 │ 1200 - 34 │ 1200 - 29 │
    │ Pythagorean major sixth       │   906 │  918 - 26 │  910 - 22 │
    │ Just major sixth              │   884 │  882 - 25 │  869 - 21 │
    │ Just minor sixth              │   814 │  812 - 23 │  828 - 20 │
    │ Pythagorean minor sixth       │   792 │  776 - 22 │  786 - 19 │
    │ Fifth                         │   702 │  706 - 20 │  703 - 17 │
    │ Fourth                        │   498 │  494 - 14 │  497 - 12 │
    │ Pythagorean major third       │   408 │  424 - 12 │  414 - 10 │
    │ Just major third              │   386 │  388 - 11 │  372 -  9 │
    │ Just minor third              │   316 │  318 -  9 │  331 -  8 │
    │ Pythagorean minor third       │   294 │  282 -  8 │  290 -  7 │
    │ Pythagorean major second      │   204 │  212 -  6 │  207 -  5 │
    │ Just minor ton                │   182 │  176 -  5 │  166 -  4 │
    │ Pythagorean chromatic semiton │   114 │  106 -  3 │  124 -  3 │
    │ Just diatonic semiton         │   112 │  106 -  3 │  124 -  3 │
    │ Pythagorean diatonic semiton  │    90 │  106 -  3 │   83 -  2 │
    └───────────────────────────────┴───────┴───────────┴───────────┘ |}];
  ()
;;
