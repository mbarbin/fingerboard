open! Core

type t =
  | I
  | II
  | III
  | IV
[@@deriving compare, equal, enumerate, hash, sexp_of]

let to_string t = Sexp.to_string [%sexp (t : t)]

let to_int = function
  | I -> 1
  | II -> 2
  | III -> 3
  | IV -> 4
;;

let of_int_exn = function
  | 1 -> I
  | 2 -> II
  | 3 -> III
  | 4 -> IV
  | i -> raise_s [%sexp "Out of bounds", [%here], (i : int)]
;;

let one = I
let succ_exn t = of_int_exn (succ (to_int t))
