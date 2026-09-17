(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

let%expect_test "to_dyn" =
  List.iter [ Ordering.Lt; Eq; Gt ] ~f:(fun t -> print_dyn (t |> Ordering.to_dyn));
  [%expect
    {|
    Lt
    Eq
    Gt
    |}];
  ()
;;

let%expect_test "equal" =
  require (Ordering.equal Lt Lt);
  require (Ordering.equal Eq Eq);
  require (Ordering.equal Gt Gt);
  require (not (Ordering.equal Lt Eq));
  require (not (Ordering.equal Eq Gt));
  require (not (Ordering.equal Gt Lt));
  ()
;;
