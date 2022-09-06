open! Core

type t = float [@@deriving compare, equal, hash, sexp_of]

let of_float_exn f =
  if Float.compare f 0. < 0 then raise_s [%sexp "Out of bounds", [%here], (f : float)];
  f
;;

let to_float f = f
let a4_440 = 440.
