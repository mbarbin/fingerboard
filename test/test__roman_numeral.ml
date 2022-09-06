open! Core
open! Cemper

let%expect_test "round-trip" =
  List.iter Roman_numeral.all ~f:(fun n1 ->
    let n2 = n1 |> Roman_numeral.to_int |> Roman_numeral.of_int_exn in
    assert (Roman_numeral.equal n1 n2));
  [%expect {||}];
  ()
;;
