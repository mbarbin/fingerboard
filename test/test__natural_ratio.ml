(**********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*                                                                                *)
(*  This file is part of Fingerboard.                                             *)
(*                                                                                *)
(*  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*  the terms of the GNU Affero General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or any later version.   *)
(*                                                                                *)
(*  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*  for more details.                                                             *)
(*                                                                                *)
(*  You should have received a copy of the GNU Affero General Public License      *)
(*  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(**********************************************************************************)

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
  let one = Natural_ratio.Reduced.one in
  let two = 2 ^^ 1 in
  let three = 3 ^^ 1 in
  let four = 2 ^^ 2 in
  let five = 5 ^^ 1 in
  print_string (Natural_ratio.Reduced.to_string (one // two));
  [%expect {| 1 / 2 |}];
  print_string (Natural_ratio.Reduced.to_string ((two ** three ** four) // five));
  [%expect {| (2^3 * 3) / 5 |}];
  print_string (Natural_ratio.Reduced.to_string (((3 ^^ 2) // (2 ^^ 3)) ** (3 ^^ -1)));
  [%expect {| 3 / 2^3 |}];
  print_string Natural_ratio.Reduced.(to_string (inverse ((3 ^^ 2) // (2 ^^ 3))));
  [%expect {| 2^3 / 3^2 |}];
  print_string
    (Natural_ratio.to_string
       (Natural_ratio.Reduced.to_natural_ratio ((3 ^^ 2) // (2 ^^ 3))));
  [%expect {| 9 / 8 |}];
  ()
;;

let%expect_test "reduce" =
  let ( // ) a b = a, b in
  [ 9 // 8
  ; 1 // 3
  ; 1 // 1
  ; 17 // 1
  ; 34 // 15
  ; 64 // 129
  ; 2146 // 3048
  ; 99 // 2480
  ; 4900 // 5634
  ]
  |> List.iter ~f:(fun (a, b) ->
    let natural_ratio = Natural_ratio.create_exn ~numerator:a ~denominator:b in
    let reduced =
      Natural_ratio.Reduced.of_small_natural_ratio_exn ~numerator:a ~denominator:b
    in
    print_s
      [%sexp
        (Natural_ratio.to_string natural_ratio : string)
        , (Natural_ratio.Reduced.to_string reduced : string)]);
  [%expect
    {|
    ("9 / 8" "3^2 / 2^3")
    ("1 / 3" "1 / 3")
    (1 1)
    (17 17)
    ("34 / 15" "(2 * 17) / (3 * 5)")
    ("64 / 129" "2^6 / (3 * 43)")
    ("2146 / 3048" "(29 * 37) / (2^2 * 3 * 127)")
    ("99 / 2480" "(3^2 * 11) / (2^4 * 5 * 31)")
    ("4900 / 5634" "(2 * 5^2 * 7^2) / (3^2 * 313)") |}];
  ()
;;
