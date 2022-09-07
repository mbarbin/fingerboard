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
  }
[@@deriving sexp_of]

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

let add_fingerboard_position_exn (_ : t) (_ : Fingerboard_position.t) : unit =
  assert false
;;

let fingerboard_positions (_ : t) : Fingerboard_position.t list = assert false

let find_fingerboard_position_exn (_ : t) ~name:(_ : string) : Fingerboard_position.t =
  assert false
;;
