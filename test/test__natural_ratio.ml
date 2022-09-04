open! Core
open! Cemper

let%expect_test "natural_ratio" =
  let ( /^ ) a b = Natural_ratio.create_exn ~numerator:a ~denominator:b in
  let ( // ) = Natural_ratio.divide in
  let ( ** ) = Natural_ratio.multiply in
  let one = 1 /^ 1 in
  let two = 2 /^ 1 in
  print_string (Natural_ratio.to_string (one // two));
  [%expect {| 1 / 2 |}];
  print_string (Natural_ratio.to_string ((9 /^ 8) ** (1 /^ 3)));
  [%expect {| 9 / 24 |}];
  print_s [%sexp (Natural_ratio.equal (9 /^ 24) (3 /^ 8) : bool)];
  [%expect {| true |}];
  print_string Natural_ratio.(to_string (inverse (9 /^ 8)));
  [%expect {| 8 / 9 |}];
  ()
;;

let%expect_test "reduced" =
  let ( ^^ ) a b = Natural_ratio.Reduced.create_exn ~prime:a ~exponent:b in
  let ( // ) = Natural_ratio.Reduced.divide in
  let ( ** ) = Natural_ratio.Reduced.multiply in
  let one = 1 ^^ 1 in
  let two = 2 ^^ 1 in
  let three = 3 ^^ 1 in
  let four = 2 ^^ 2 in
  let five = 5 ^^ 1 in
  print_string (Natural_ratio.Reduced.to_string (one // two));
  [%expect {| 1 / 2 |}];
  print_string (Natural_ratio.Reduced.to_string ((two ** three ** four) // five));
  [%expect {| (2 ^ 3 * 3) / 5 |}];
  print_string (Natural_ratio.Reduced.to_string (((3 ^^ 2) // (2 ^^ 3)) ** (3 ^^ -1)));
  [%expect {| 3 / 2 ^ 3 |}];
  print_string Natural_ratio.Reduced.(to_string (inverse ((3 ^^ 2) // (2 ^^ 3))));
  [%expect {| 2 ^ 3 / 3 ^ 2 |}];
  print_string
    (Natural_ratio.to_string
       (Natural_ratio.Reduced.to_natural_ratio ((3 ^^ 2) // (2 ^^ 3))));
  [%expect {| 9 / 8 |}];
  ()
;;
