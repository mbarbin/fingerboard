open! Core

(** Manipulating small integers displayed as roman numerals. By
   convention, roman numerals are used to designate the vibrating
   strings of the instruments, with I (1) being the higher string, and
   increasing numbers going down. For example, the lower C string on
   the 4-strings cello is IV (4).

    Roman numeral can also be used to designate the degree of the
   notes in a scale, with I beeing the tonic, V the dominant, etc. *)

type t =
  | I
  | II
  | III
  | IV
  | V
  | VI
  | VII
[@@deriving compare, equal, enumerate, hash, sexp_of]

val to_string : t -> string

(** Because we don't need to handle instruments with a very large
   number of strings, only a few roman numerals are representable with
   this module. An overflow will cause the function to raise. *)
val of_int_exn : int -> t

val one : t
val succ_exn : t -> t
val to_int : t -> int
