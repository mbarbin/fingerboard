(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

let%expect_test "phys_equal" =
  let x = ref 0 in
  let y = ref 0 in
  require (phys_equal x x);
  require (not (phys_equal x y));
  ()
;;

let%expect_test "print_dyn" =
  print_dyn (Dyn.record [ "a", Dyn.int 1; "b", Dyn.list Dyn.string [ "x"; "y" ] ]);
  [%expect {| { a = 1; b = [ "x"; "y" ] } |}];
  ()
;;

let%expect_test "require" =
  require true;
  require_does_raise (fun () -> require false);
  [%expect {| ("Require failed.", {}) |}];
  (match require_does_raise (fun () -> ()) with
   | () -> assert false
   | exception e -> print_endline (Printexc.to_string e));
  [%expect {| ("Did not raise.", {}) |}];
  ()
;;

let%expect_test "require_equal" =
  require_equal (module Ordering) Lt Lt;
  require_does_raise (fun () -> require_equal (module Ordering) Lt Gt);
  [%expect {| ("Values are not equal.", { v1 = Lt; v2 = Gt }) |}];
  ()
;;
