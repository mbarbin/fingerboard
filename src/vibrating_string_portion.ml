open! Core

type t = { acoustic_interval_to_the_open_string : Acoustic_interval.t }
[@@deriving sexp_of]

let compare t1 t2 =
  Float.compare
    (Acoustic_interval.to_cents t2.acoustic_interval_to_the_open_string)
    (Acoustic_interval.to_cents t1.acoustic_interval_to_the_open_string)
;;

let equal t1 t2 =
  Float.equal
    (Acoustic_interval.to_cents t1.acoustic_interval_to_the_open_string)
    (Acoustic_interval.to_cents t2.acoustic_interval_to_the_open_string)
;;

let hash t =
  Float.hash (Acoustic_interval.to_cents t.acoustic_interval_to_the_open_string)
;;

let hash_fold_t state t =
  Float.hash_fold_t
    state
    (Acoustic_interval.to_cents t.acoustic_interval_to_the_open_string)
;;

let open_string = { acoustic_interval_to_the_open_string = Acoustic_interval.unison }
let acoustic_interval_to_the_open_string t = t.acoustic_interval_to_the_open_string

let of_acoustic_interval_to_the_open_string acoustic_interval_to_the_open_string =
  { acoustic_interval_to_the_open_string }
;;
