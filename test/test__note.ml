open! Core
open! Cemper

let%expect_test "semitons_step" =
  let rec aux start acc index =
    if Note.Letter_name.equal start index && acc > 0
    then acc
    else (
      let acc = acc + Note.Letter_name.semitons_step ~from:index in
      aux start acc (Note.Letter_name.next index))
  in
  let number_of_semitons_per_octave = aux A 0 A in
  print_s [%sexp { number_of_semitons_per_octave : int }];
  [%expect {| ((number_of_semitons_per_octave 12)) |}]
;;
