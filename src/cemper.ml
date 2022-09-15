open! Core
module Acoustic_interval = Acoustic_interval
module Characterized_interval = Characterized_interval
module Fingerboard_location = Fingerboard_location
module Fingerboard_position = Fingerboard_position
module Frequency = Frequency
module Interval = Interval
module Located_note = Located_note
module Natural_ratio = Natural_ratio
module Note = Note
module Roman_numeral = Roman_numeral
module Scale = Scale
module System = System

let hello_world = [%sexp "Hello, World!"]

let print_cmd =
  Command.basic
    ~summary:"print hello world"
    (let%map_open.Command () = return () in
     fun () -> print_s hello_world)
;;

let main = Command.group ~summary:"" [ "print", print_cmd ]
