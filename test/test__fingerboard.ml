open! Core

let%expect_test "hello" =
  print_s Fingerboard.hello_world;
  [%expect {| "Hello, World!" |}]
;;
