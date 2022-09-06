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
  ; intervals_going_down : (Interval.t * Acoustic_interval.t) array
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
      ~f:(fun previous_string (interval, acoustic_interval) ->
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
         |> Acoustic_interval.shift_up (snd t.intervals_going_down.(i))
  done;
  (* Tune going down. *)
  for i = index + 1 to Array.length t.vibrating_strings - 1 do
    t.vibrating_strings.(i).pitch
      <- t.vibrating_strings.(i - 1).pitch
         |> Acoustic_interval.shift_down (snd t.intervals_going_down.(i - 1))
  done;
  ()
;;
