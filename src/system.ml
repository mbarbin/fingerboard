open! Core

module Vibrating_string : sig
  type t =
    { open_string : Note.t
    ; mutable pitch : Frequency.t
    ; roman_numeral : Roman_numeral.t
    }
  [@@deriving sexp_of]
end = struct
  type t =
    { open_string : Note.t
    ; mutable pitch : Frequency.t
    ; roman_numeral : Roman_numeral.t
    }
  [@@deriving sexp_of]
end

type t =
  { vibrating_strings : Vibrating_string.t array
  ; intervals_going_down : Characterized_interval.t array
  ; mutable fingerboard_positions : Fingerboard_position.t list
       [@sexp_drop_if List.is_empty]
  }
[@@deriving sexp_of]

let to_ascii_tables { vibrating_strings; intervals_going_down; fingerboard_positions } =
  let vibrating_strings =
    let columns =
      Ascii_table.Column.
        [ create_attr
            ~align:Right
            "String"
            (fun (_, { Vibrating_string.open_string = _; pitch = _; roman_numeral }) ->
            [], Roman_numeral.to_string roman_numeral)
        ; create_attr "Note" (fun (_, (t : Vibrating_string.t)) ->
            [], Note.to_string t.open_string)
        ; create_attr ~align:Right "Pitch" (fun (_, (t : Vibrating_string.t)) ->
            [], sprintf "%0.2f" (Frequency.to_float t.pitch))
        ; create_attr "Interval" (fun (i, _) ->
            if i >= Array.length intervals_going_down
            then [], ""
            else (
              let { Characterized_interval.interval; acoustic_interval } =
                intervals_going_down.(i)
              in
              ( []
              , sprintf
                  "%s - %s"
                  (Interval.to_string interval)
                  (Acoustic_interval.to_string acoustic_interval) )))
        ; create_attr "Cents" (fun (i, _) ->
            if i >= Array.length intervals_going_down
            then [], ""
            else (
              let { Characterized_interval.acoustic_interval; _ } =
                intervals_going_down.(i)
              in
              [], Cents.to_string_nearest (Acoustic_interval.to_cents acoustic_interval)))
        ]
    in
    Ascii_table.to_string
      columns
      (Array.to_list vibrating_strings |> List.mapi ~f:(fun i v -> i, v))
  and fingerboard_positions =
    Ascii_table.to_string Fingerboard_position.ascii_table_columns fingerboard_positions
  in
  [ vibrating_strings; fingerboard_positions ] |> String.concat ~sep:"\n"
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
            previous_string.open_string
            |> Interval.shift_down interval
            |> Option.value_exn ~here:[%here]
        ; pitch = previous_string.pitch |> Acoustic_interval.shift_down acoustic_interval
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
  let index = Roman_numeral.to_int roman_numeral |> pred in
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
    raise_s
      [%sexp
        "String number out of bounds"
        , [%here]
        , { string_number : Roman_numeral.t; available : Roman_numeral.t array }])
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
  | None -> raise_s [%sexp "Fingerboard_position not found", [%here], { name : string }]
;;

let add_fingerboard_position_exn
  ?(on_n_octaves = 3)
  (t : t)
  (fingerboard_position : Fingerboard_position.t)
  =
  if Acoustic_interval.compare
       (Fingerboard_position.acoustic_interval_to_the_open_string fingerboard_position)
       Acoustic_interval.octave
     > 0
  then
    raise_s
      [%sexp
        "Interval out of bounds"
        , [%here]
        , { fingerboard_position : Fingerboard_position.t }];
  let name = Fingerboard_position.name fingerboard_position in
  (match find_fingerboard_position t ~name with
   | None -> ()
   | Some existing_fingerboard_position ->
     raise_s
       [%sexp
         "Duplicated fingerboard position's name"
         , [%here]
         , { name : string
           ; fingerboard_position : Fingerboard_position.t
           ; existing_fingerboard_position : Fingerboard_position.t
           }]);
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
  let open Option.Let_syntax in
  let index = Roman_numeral.to_int fingerboard_location.string_number in
  let%bind fingerboard_location =
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
  let%bind note = Interval.shift_up characterized_interval.interval note in
  return { Located_note.note; fingerboard_location }
;;

let open_string t string_number =
  let open Option.Let_syntax in
  let index = Roman_numeral.to_int string_number - 1 in
  let%bind vibrating_string =
    if index >= 0 && index < Array.length t.vibrating_strings
    then return t.vibrating_strings.(index)
    else None
  in
  let%bind fingerboard_position =
    match List.hd t.fingerboard_positions with
    | None -> None
    | Some fingerboard_position ->
      if Acoustic_interval.equal
           Acoustic_interval.unison
           (Fingerboard_position.acoustic_interval_to_the_open_string
              fingerboard_position)
      then return fingerboard_position
      else None
  in
  return
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
  with_return (fun { return } ->
    let string_number =
      let index = Roman_numeral.to_int fingerboard_location.string_number in
      if index >= Array.length t.vibrating_strings
      then return None
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
        })
;;

module Double_stops = struct
  type system = t
  type t = Double_stop.t list

  let to_ascii_table (system : system) double_stops =
    let columns =
      let open Ascii_table.Column in
      let common_columns ~name ~(f : Double_stop.t -> Located_note.t) =
        [ create_attr name (fun t -> [], Note.to_string (f t).note)
        ; create_attr "String" (fun t ->
            [], Roman_numeral.to_string (f t).fingerboard_location.string_number)
        ; create_attr "Pos" (fun t ->
            ( []
            , Fingerboard_position.to_string
                (f t).fingerboard_location.fingerboard_position ))
        ; create_attr "Cents" (fun t ->
            let acoustic_interval =
              Fingerboard_position.acoustic_interval_to_the_open_string
                (f t).fingerboard_location.fingerboard_position
            in
            let cents = Acoustic_interval.to_cents acoustic_interval in
            [], Cents.to_string_nearest cents)
        ]
      in
      [ common_columns ~name:"Low" ~f:(fun (t : Double_stop.t) -> t.low_note)
      ; common_columns ~name:"High" ~f:(fun (t : Double_stop.t) -> t.high_note)
      ; [ create_attr "Interval" (fun (t : Double_stop.t) ->
            let interval =
              Interval.compute ~from:t.low_note.note ~to_:t.high_note.note ()
              |> Option.value_exn ~here:[%here]
            in
            let acoustic_interval =
              acoustic_interval
                system
                ~from:t.low_note.fingerboard_location
                ~to_:t.high_note.fingerboard_location
              |> Option.value_exn ~here:[%here]
            in
            ( []
            , sprintf
                "%s - %s"
                (Interval.to_string interval)
                (Acoustic_interval.to_string acoustic_interval) ))
        ; create_attr "Cents" (fun (t : Double_stop.t) ->
            let acoustic_interval =
              acoustic_interval
                system
                ~from:t.low_note.fingerboard_location
                ~to_:t.high_note.fingerboard_location
              |> Option.value_exn ~here:[%here]
            in
            [], Cents.to_string_nearest (Acoustic_interval.to_cents acoustic_interval))
        ]
      ]
      |> List.concat
    in
    Ascii_table.to_string columns double_stops
  ;;

  let make_scale (t : system) ~characterized_scale ~interval_number ~from ~to_ =
    let scale = make_scale t ~characterized_scale ~from ~to_ in
    let double_stops =
      let rec aux acc = function
        | [] -> acc
        | low_note :: tl as scale ->
          (match List.nth scale (interval_number - 1) with
           | None -> acc
           | Some high_note ->
             let acc =
               let low_note =
                 if Roman_numeral.equal
                      low_note.Located_note.fingerboard_location.string_number
                      high_note.Located_note.fingerboard_location.string_number
                 then find_same_note_one_string_down t low_note
                 else Some low_note
               in
               match low_note with
               | None -> acc
               | Some low_note -> Double_stop.{ low_note; high_note } :: acc
             in
             aux acc tl)
      in
      aux [] scale |> List.rev
    in
    double_stops
  ;;
end
