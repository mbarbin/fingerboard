(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

type t =
  { name : string
  ; at_octave : int
  ; basis_acoustic_interval_to_the_open_string : Acoustic_interval.t
  }

let compare t ({ name; at_octave; basis_acoustic_interval_to_the_open_string } as t2)
  : Ordering.t
  =
  if phys_equal t t2
  then Eq
  else (
    match String.compare t.name name with
    | (Lt | Gt) as r -> r
    | Eq ->
      (match Int.compare t.at_octave at_octave with
       | (Lt | Gt) as r -> r
       | Eq ->
         Acoustic_interval.compare
           t.basis_acoustic_interval_to_the_open_string
           basis_acoustic_interval_to_the_open_string))
;;

let equal t1 t2 = Ordering.is_eq (compare t1 t2)

let to_dyn { name; at_octave; basis_acoustic_interval_to_the_open_string } =
  Dyn.record
    [ "name", name |> Dyn.string
    ; "at_octave", at_octave |> Dyn.int
    ; ( "basis_acoustic_interval_to_the_open_string"
      , basis_acoustic_interval_to_the_open_string |> Acoustic_interval.to_dyn )
    ]
;;

let name t = t.name

let to_string t =
  if t.at_octave = 0 then t.name else Printf.sprintf "%s-%d" t.name t.at_octave
;;

let acoustic_interval_to_the_open_string t =
  t.basis_acoustic_interval_to_the_open_string
  :: List.init t.at_octave ~f:(Fun.const Acoustic_interval.octave)
  |> List.reduce ~f:Acoustic_interval.add
  |> Option.value ~default:Acoustic_interval.unison
;;

let ascii_table_columns =
  Print_table.O.
    [ Column.make ~align:Right ~header:"Pos" (fun t -> Cell.text (to_string t))
    ; Column.make ~align:Right ~header:"Cents" (fun t ->
        let acoustic_interval = acoustic_interval_to_the_open_string t in
        let cents = Acoustic_interval.to_cents acoustic_interval in
        Cell.text (Cents.to_string_nearest cents))
    ; Column.make ~align:Right ~header:"Interval" (fun t ->
        let acoustic_interval = acoustic_interval_to_the_open_string t in
        Cell.text (Acoustic_interval.to_string acoustic_interval))
    ]
;;

let at_octave t ~octave = { t with at_octave = octave }

let create_exn ~name ~acoustic_interval_to_the_open_string =
  (match
     Acoustic_interval.compare
       acoustic_interval_to_the_open_string
       Acoustic_interval.octave
   with
   | Lt -> ()
   | Eq | Gt ->
     Code_error.raise
       "Interval out of bounds."
       [ "name", name |> Dyn.string
       ; ( "acoustic_interval_to_the_open_string"
         , acoustic_interval_to_the_open_string |> Acoustic_interval.to_dyn )
       ; ( "in_cents"
         , Acoustic_interval.to_cents acoustic_interval_to_the_open_string |> Dyn.float )
       ]);
  { name
  ; at_octave = 0
  ; basis_acoustic_interval_to_the_open_string = acoustic_interval_to_the_open_string
  }
;;

let open_string =
  create_exn ~name:"0" ~acoustic_interval_to_the_open_string:Acoustic_interval.unison
;;
