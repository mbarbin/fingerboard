open! Core
module Acoustic_interval = Acoustic_interval
module Interval = Interval
module Natural_ratio = Natural_ratio
module Note = Note
module Scale = Scale

let hello_world = [%sexp "Hello, World!"]

let print_cmd =
  Command.basic
    ~summary:"print hello world"
    (let%map_open.Command () = return () in
     fun () -> print_s hello_world)
;;

let main = Command.group ~summary:"" [ "print", print_cmd ]
