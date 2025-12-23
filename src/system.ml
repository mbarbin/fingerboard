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

module Vibrating_string : sig
  type t =
    { open_string : Note.t
    ; mutable pitch : Frequency.t
    ; roman_numeral : Roman_numeral.t
    }

  val to_dyn : t -> Dyn.t
end = struct
  type t =
    { open_string : Note.t
    ; mutable pitch : Frequency.t
    ; roman_numeral : Roman_numeral.t
    }

  let to_dyn { open_string; pitch; roman_numeral } =
    Dyn.record
      [ "open_string", open_string |> Note.to_dyn
      ; "pitch", pitch |> Frequency.to_dyn
      ; "roman_numeral", roman_numeral |> Roman_numeral.to_dyn
      ]
  ;;
end

type t =
  { vibrating_strings : Vibrating_string.t array
  ; intervals_going_down : Characterized_interval.t array
  ; mutable fingerboard_positions : Fingerboard_position.t list
  }

let to_dyn { vibrating_strings; intervals_going_down; fingerboard_positions } =
  Dyn.record
    (List.concat
       [ [ "vibrating_strings", vibrating_strings |> Dyn.array Vibrating_string.to_dyn
         ; ( "intervals_going_down"
           , intervals_going_down |> Dyn.array Characterized_interval.to_dyn )
         ]
       ; (if List.is_empty fingerboard_positions
          then []
          else
            [ ( "fingerboard_positions"
              , fingerboard_positions |> Dyn.list Fingerboard_position.to_dyn )
            ])
       ])
;;

let to_ascii_tables { vibrating_strings; intervals_going_down; fingerboard_positions } =
  let vibrating_strings =
    let columns =
      Print_table.O.
        [ Column.make
            ~align:Right
            ~header:"String"
            (fun (_, { Vibrating_string.open_string = _; pitch = _; roman_numeral }) ->
               Cell.text (Roman_numeral.to_string roman_numeral))
        ; Column.make ~header:"Note" (fun (_, (t : Vibrating_string.t)) ->
            Cell.text (Note.to_string t.open_string))
        ; Column.make ~align:Right ~header:"Pitch" (fun (_, (t : Vibrating_string.t)) ->
            Cell.text (Printf.sprintf "%0.2f" (Frequency.to_float t.pitch)))
        ; Column.make ~header:"Interval" (fun (i, _) ->
            if i >= Array.length intervals_going_down
            then Cell.empty
            else (
              let { Characterized_interval.interval; acoustic_interval } =
                intervals_going_down.(i)
              in
              Cell.text
                (Printf.sprintf
                   "%s - %s"
                   (Interval.to_string interval)
                   (Acoustic_interval.to_string acoustic_interval))))
        ; Column.make ~align:Right ~header:"Cents" (fun (i, _) ->
            if i >= Array.length intervals_going_down
            then Cell.empty
            else (
              let { Characterized_interval.acoustic_interval; _ } =
                intervals_going_down.(i)
              in
              Cell.text
                (Cents.to_string_nearest (Acoustic_interval.to_cents acoustic_interval))))
        ]
    in
    Print_table.make
      ~columns
      ~rows:(Array.to_list vibrating_strings |> List.mapi ~f:(fun i v -> i, v))
  and fingerboard_positions =
    Print_table.make
      ~columns:Fingerboard_position.ascii_table_columns
      ~rows:fingerboard_positions
  in
  [ vibrating_strings; fingerboard_positions ]
  |> List.map ~f:Print_table.to_string_text
  |> String.concat ~sep:"\n"
;;

let create ~high_vibrating_string ~pitch ~intervals_going_down =
  let high_vibrating_string =
    { Vibrating_string.open_string = high_vibrating_string
    ; pitch
    ; roman_numeral = Roman_numeral.one
    }
  in
  let other_strings =
    Array.fold_map
      intervals_going_down
      ~init:high_vibrating_string
      ~f:(fun previous_string { Characterized_interval.interval; acoustic_interval } ->
        let v =
          { Vibrating_string.open_string =
              previous_string.open_string |> Interval.shift_down interval |> Option.get
          ; pitch =
              previous_string.pitch |> Acoustic_interval.shift_down acoustic_interval
          ; roman_numeral = Roman_numeral.succ_exn previous_string.roman_numeral
          }
        in
        v, v)
    |> snd
  in
  { vibrating_strings = Array.concat [ [| high_vibrating_string |]; other_strings ]
  ; intervals_going_down
  ; fingerboard_positions = []
  }
;;

let reset_pitch t roman_numeral ~pitch =
  let index = Roman_numeral.to_int roman_numeral |> Int.pred in
  t.vibrating_strings.(index).pitch <- pitch;
  (* Tune going up. *)
  for i = index - 1 downto 0 do
    t.vibrating_strings.(i).pitch
    <- t.vibrating_strings.(i + 1).pitch
       |> Acoustic_interval.shift_up t.intervals_going_down.(i).acoustic_interval
  done;
  (* Tune going down. *)
  for i = index + 1 to Array.length t.vibrating_strings - 1 do
    t.vibrating_strings.(i).pitch
    <- t.vibrating_strings.(i - 1).pitch
       |> Acoustic_interval.shift_down t.intervals_going_down.(i - 1).acoustic_interval
  done;
  ()
;;

let vibrating_string_exn (t : t) string_number =
  let index = Roman_numeral.to_int string_number - 1 in
  if index < 0 || index >= Array.length t.vibrating_strings
  then (
    let available = Array.map t.vibrating_strings ~f:(fun t -> t.roman_numeral) in
    Code_error.raise
      "String number out of bounds."
      [ "string_number", string_number |> Roman_numeral.to_dyn
      ; "available", available |> Dyn.array Roman_numeral.to_dyn
      ])
  else t.vibrating_strings.(index)
;;

let pitch (t : t) { Fingerboard_location.fingerboard_position; string_number } =
  let vibrating_string = vibrating_string_exn t string_number in
  let interval =
    Fingerboard_position.acoustic_interval_to_the_open_string fingerboard_position
  in
  Acoustic_interval.shift_up interval vibrating_string.pitch
;;

let acoustic_interval
      (t : t)
      ~from:{ Fingerboard_location.fingerboard_position = p1; string_number = s1 }
      ~to_:{ Fingerboard_location.fingerboard_position = p2; string_number = s2 }
  =
  let (_ : Vibrating_string.t) = vibrating_string_exn t s1 in
  let (_ : Vibrating_string.t) = vibrating_string_exn t s2 in
  let i1 = Roman_numeral.to_int s1
  and i2 = Roman_numeral.to_int s2 in
  let interval_between_strings = ref Acoustic_interval.unison in
  for i = min i1 i2 to max i1 i2 - 1 do
    interval_between_strings
    := Acoustic_interval.add
         !interval_between_strings
         t.intervals_going_down.(i - 1).acoustic_interval
  done;
  Acoustic_interval.remove
    (Acoustic_interval.add
       !interval_between_strings
       (Fingerboard_position.acoustic_interval_to_the_open_string p2))
    (Fingerboard_position.acoustic_interval_to_the_open_string p1)
;;

let fingerboard_positions t = t.fingerboard_positions

let find_fingerboard_position (t : t) ~name =
  List.find t.fingerboard_positions ~f:(fun fingerboard_position ->
    String.equal name (Fingerboard_position.name fingerboard_position))
;;

let find_fingerboard_position_exn t ~name =
  match find_fingerboard_position t ~name with
  | Some x -> x
  | None ->
    Code_error.raise "Fingerboard_position not found." [ "name", name |> Dyn.string ]
;;

let add_fingerboard_position_exn
      ?(on_n_octaves = 3)
      (t : t)
      (fingerboard_position : Fingerboard_position.t)
  =
  if
    Acoustic_interval.compare
      (Fingerboard_position.acoustic_interval_to_the_open_string fingerboard_position)
      Acoustic_interval.octave
    > 0
  then
    Code_error.raise
      "Interval out of bounds."
      [ "fingerboard_position", fingerboard_position |> Fingerboard_position.to_dyn ];
  let name = Fingerboard_position.name fingerboard_position in
  (match find_fingerboard_position t ~name with
   | None -> ()
   | Some existing_fingerboard_position ->
     Code_error.raise
       "Duplicated fingerboard position's name."
       [ "name", name |> Dyn.string
       ; "fingerboard_position", fingerboard_position |> Fingerboard_position.to_dyn
       ; ( "existing_fingerboard_position"
         , existing_fingerboard_position |> Fingerboard_position.to_dyn )
       ]);
  let fingerboard_positions =
    List.init on_n_octaves ~f:(fun i ->
      Fingerboard_position.at_octave fingerboard_position ~octave:i)
    @ t.fingerboard_positions
    |> List.sort
         ~compare:
           (Comparable.lift
              Acoustic_interval.compare
              ~f:Fingerboard_position.acoustic_interval_to_the_open_string)
  in
  t.fingerboard_positions <- fingerboard_positions
;;

let exists_fingerboard_position t fingerboard_position =
  List.exists t.fingerboard_positions ~f:(fun p ->
    Fingerboard_position.equal p fingerboard_position)
;;

let exists_fingerboard_location
      t
      { Fingerboard_location.fingerboard_position; string_number }
  =
  let index = Roman_numeral.to_int string_number - 1 in
  index >= 0
  && index < Array.length t.vibrating_strings
  && exists_fingerboard_position t fingerboard_position
;;

let find_next_located_note
      (t : t)
      { Located_note.note; fingerboard_location }
      (characterized_interval : Characterized_interval.t)
  =
  let ( let* ) x f = Option.bind x ~f in
  let index = Roman_numeral.to_int fingerboard_location.string_number in
  let* fingerboard_location =
    List.find_map (List.init index ~f:Fn.id) ~f:(fun index ->
      let string_number = Roman_numeral.of_int_exn (index + 1) in
      match
        List.find t.fingerboard_positions ~f:(fun fingerboard_position ->
          match
            acoustic_interval
              t
              ~from:fingerboard_location
              ~to_:{ fingerboard_position; string_number }
          with
          | None -> false
          | Some found_interval ->
            Acoustic_interval.equal
              found_interval
              characterized_interval.acoustic_interval)
      with
      | None -> None
      | Some fingerboard_position ->
        Some { Fingerboard_location.fingerboard_position; string_number })
  in
  let* note = Interval.shift_up characterized_interval.interval note in
  Option.some { Located_note.note; fingerboard_location }
;;

let open_string t string_number =
  let ( let* ) x f = Option.bind x ~f in
  let index = Roman_numeral.to_int string_number - 1 in
  let* vibrating_string =
    if index >= 0 && index < Array.length t.vibrating_strings
    then Option.some t.vibrating_strings.(index)
    else None
  in
  let* fingerboard_position =
    match t.fingerboard_positions with
    | [] -> None
    | fingerboard_position :: _ ->
      if
        Acoustic_interval.equal
          Acoustic_interval.unison
          (Fingerboard_position.acoustic_interval_to_the_open_string fingerboard_position)
      then Option.some fingerboard_position
      else None
  in
  Option.some
    { Located_note.note = vibrating_string.open_string
    ; fingerboard_location = { fingerboard_position; string_number }
    }
;;

let make_scale t ~characterized_scale ~from ~to_ =
  let rec aux acc scale (located_note : Located_note.t) =
    if Option.is_some (Interval.compute ~from:to_ ~to_:located_note.note ())
    then acc
    else (
      match scale with
      | [] -> aux acc characterized_scale located_note
      | hd :: tl ->
        (match find_next_located_note t located_note hd with
         | None -> acc
         | Some next_located_note -> aux (next_located_note :: acc) tl next_located_note))
  in
  aux [ from ] [] from |> List.rev
;;

let find_same_note_one_string_down t { Located_note.note; fingerboard_location } =
  let exception No_string_down in
  match
    let string_number =
      let index = Roman_numeral.to_int fingerboard_location.string_number in
      if index >= Array.length t.vibrating_strings
      then Stdlib.raise_notrace No_string_down
      else Roman_numeral.of_int_exn (index + 1)
    in
    match
      List.find t.fingerboard_positions ~f:(fun fingerboard_position ->
        match
          acoustic_interval
            t
            ~from:{ fingerboard_position; string_number }
            ~to_:fingerboard_location
        with
        | None -> false
        | Some found_interval ->
          Acoustic_interval.equal found_interval Acoustic_interval.unison)
    with
    | None -> None
    | Some fingerboard_position ->
      Some
        { Located_note.note
        ; fingerboard_location = { fingerboard_position; string_number }
        }
  with
  | res -> res
  | exception No_string_down -> None
;;

module Double_stops = struct
  type system = t
  type t = Double_stop.t list

  let to_ascii_table (system : system) double_stops =
    let columns =
      let open Print_table.O in
      let common_columns ~name ~(f : Double_stop.t -> Located_note.t) =
        [ Column.make ~header:name (fun t -> Cell.text (Note.to_string (f t).note))
        ; Column.make ~header:"String" (fun t ->
            Cell.text (Roman_numeral.to_string (f t).fingerboard_location.string_number))
        ; Column.make ~header:"Pos" (fun t ->
            Cell.text
              (Fingerboard_position.to_string
                 (f t).fingerboard_location.fingerboard_position))
        ; Column.make ~align:Right ~header:"Cents" (fun t ->
            let acoustic_interval =
              Fingerboard_position.acoustic_interval_to_the_open_string
                (f t).fingerboard_location.fingerboard_position
            in
            let cents = Acoustic_interval.to_cents acoustic_interval in
            Cell.text (Cents.to_string_nearest cents))
        ]
      in
      [ common_columns ~name:"Low" ~f:(fun (t : Double_stop.t) -> t.low_note)
      ; common_columns ~name:"High" ~f:(fun (t : Double_stop.t) -> t.high_note)
      ; [ Column.make ~header:"Interval" (fun (t : Double_stop.t) ->
            let interval =
              Interval.compute ~from:t.low_note.note ~to_:t.high_note.note ()
              |> Option.get
            in
            let acoustic_interval =
              acoustic_interval
                system
                ~from:t.low_note.fingerboard_location
                ~to_:t.high_note.fingerboard_location
              |> Option.get
            in
            Cell.text
              (Printf.sprintf
                 "%s - %s"
                 (Interval.to_string interval)
                 (Acoustic_interval.to_string acoustic_interval)))
        ; Column.make ~align:Right ~header:"Cents" (fun (t : Double_stop.t) ->
            let acoustic_interval =
              acoustic_interval
                system
                ~from:t.low_note.fingerboard_location
                ~to_:t.high_note.fingerboard_location
              |> Option.get
            in
            Cell.text
              (Cents.to_string_nearest (Acoustic_interval.to_cents acoustic_interval)))
        ]
      ]
      |> List.concat
    in
    Print_table.to_string_text (Print_table.make ~columns ~rows:double_stops)
  ;;

  module Adjustment = struct
    type t =
      { from : Acoustic_interval.t
      ; to_ : Acoustic_interval.t
      }

    module Choice_criteria = struct
      (* The type is minted in such a way that the compare function
         must prioritize the lower values. *)
      type t =
        { exists_open_string_with_that_note : bool
        ; degree_priority : int
        }
      [@@deriving compare]

      let of_located_note (system : system) ~tonic (located_note : Located_note.t) =
        { exists_open_string_with_that_note =
            Array.exists system.vibrating_strings ~f:(fun v ->
              Note.equal
                v.open_string
                { located_note.note with
                  octave_designation = v.open_string.octave_designation
                })
        ; degree_priority =
            (match Interval.compute ~from:tonic ~to_:located_note.note () with
             | None -> 10
             | Some interval ->
               (match interval.number with
                | Second | Third | Sixth -> 1
                | Seventh -> 2
                | Fourth | Fifth -> 3
                | Unison | Octave -> 4))
        }
      ;;
    end
  end

  let adjust (system : system) ~tonic ~adjustment:{ Adjustment.from; to_ } (t : t) =
    List.map t ~f:(fun ({ Double_stop.low_note; high_note } as double_stop) ->
      let actual_interval =
        acoustic_interval
          system
          ~from:low_note.fingerboard_location
          ~to_:high_note.fingerboard_location
        |> Option.get
      in
      if not (Acoustic_interval.equal actual_interval from)
      then double_stop
      else (
        let adjusted_low_note =
          let string_number = low_note.fingerboard_location.string_number in
          match
            List.find system.fingerboard_positions ~f:(fun fingerboard_position ->
              match
                acoustic_interval
                  system
                  ~from:{ fingerboard_position; string_number }
                  ~to_:high_note.fingerboard_location
              with
              | None -> false
              | Some found_interval -> Acoustic_interval.equal found_interval to_)
          with
          | None -> None
          | Some fingerboard_position ->
            Some
              { low_note with
                fingerboard_location = { fingerboard_position; string_number }
              }
        in
        let adjusted_high_note =
          let string_number = high_note.fingerboard_location.string_number in
          match
            List.find system.fingerboard_positions ~f:(fun fingerboard_position ->
              match
                acoustic_interval
                  system
                  ~from:low_note.fingerboard_location
                  ~to_:{ fingerboard_position; string_number }
              with
              | None -> false
              | Some found_interval -> Acoustic_interval.equal found_interval to_)
          with
          | None -> None
          | Some fingerboard_position ->
            Some
              { high_note with
                fingerboard_location = { fingerboard_position; string_number }
              }
        in
        match adjusted_low_note, adjusted_high_note with
        | None, None -> (* No adjustment available. *) double_stop
        | Some low_note, None -> { Double_stop.low_note; high_note }
        | None, Some high_note -> { Double_stop.low_note; high_note }
        | Some adjusted_low_note, Some adjusted_high_note ->
          (match
             Adjustment.Choice_criteria.compare
               (Adjustment.Choice_criteria.of_located_note system ~tonic low_note)
               (Adjustment.Choice_criteria.of_located_note system ~tonic high_note)
             |> Ordering.of_int
           with
           | Less | Equal -> { Double_stop.low_note = adjusted_low_note; high_note }
           | Greater -> { Double_stop.low_note; high_note = adjusted_high_note })))
  ;;

  let make_scale ?adjustment (t : system) ~characterized_scale ~interval_number ~from ~to_
    =
    let scale = make_scale t ~characterized_scale ~from ~to_ in
    let double_stops =
      let index = Interval.Number.to_int interval_number - 1 in
      let rec aux acc = function
        | [] -> acc
        | low_note :: tl as scale ->
          (match List.nth scale index with
           | None -> acc
           | Some high_note ->
             let acc =
               let double_stop =
                 let low_index =
                   Roman_numeral.to_int
                     low_note.Located_note.fingerboard_location.string_number
                 and high_index =
                   Roman_numeral.to_int
                     high_note.Located_note.fingerboard_location.string_number
                 in
                 if low_index = high_index
                 then
                   find_same_note_one_string_down t low_note
                   |> Option.map ~f:(fun low_note -> { Double_stop.low_note; high_note })
                 else if low_index = high_index + 2
                 then
                   find_same_note_one_string_down t high_note
                   |> Option.map ~f:(fun high_note -> { Double_stop.low_note; high_note })
                 else Some { Double_stop.low_note; high_note }
               in
               match double_stop with
               | None -> acc
               | Some double_stop -> double_stop :: acc
             in
             aux acc tl)
      in
      aux [] scale |> List.rev
    in
    match adjustment with
    | None -> double_stops
    | Some adjustment -> adjust t ~tonic:from.note ~adjustment double_stops
  ;;
end
