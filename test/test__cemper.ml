open! Core

let%expect_test "hello" =
  print_s Cemper.hello_world;
  [%expect {| "Hello, World!" |}]
;;
