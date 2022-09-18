open! Core

type t =
  { note : Note.t
  ; fingerboard_location : Fingerboard_location.t
  }
[@@deriving sexp_of]

module Abbrev = struct
  type t = string * string * Roman_numeral.t [@@deriving sexp_of]
end

let to_abbrev { note; fingerboard_location = { fingerboard_position; string_number } } =
  Note.to_string note, Fingerboard_position.to_string fingerboard_position, string_number
;;

module Scale_abbrev = struct
  type t = (Roman_numeral.t * (string * string) list) list [@@deriving sexp_of]
end

let to_scale_abbrev ts =
  List.group ts ~break:(fun t1 t2 ->
    not
      (Roman_numeral.equal
         t1.fingerboard_location.string_number
         t2.fingerboard_location.string_number))
  |> List.map ~f:(fun ts ->
       let string_number = (List.hd_exn ts).fingerboard_location.string_number in
       ( string_number
       , List.map ts ~f:(fun t ->
           ( Note.to_string t.note
           , Fingerboard_position.to_string t.fingerboard_location.fingerboard_position ))
       ))
;;
