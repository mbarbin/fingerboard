open! Core
open! Cemper

let create () =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create
      ~len:3
      (Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:
           (Acoustic_interval.equal_division_of_the_octave
              ~divisor:53
              ~number_of_divisions:31))
  in
  System.create ~high_vibrating_string:a ~pitch ~intervals_going_down
;;

let t =
  lazy
    (let t = create () in
     t)
;;
