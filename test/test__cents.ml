(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

let%expect_test "iround_exn" =
  let test f = print_endline (Cents.iround_exn f |> Int.to_string) in
  test 0.;
  [%expect {| 0 |}];
  test 0.4;
  [%expect {| 0 |}];
  test 0.5;
  [%expect {| 1 |}];
  test 0.9;
  [%expect {| 1 |}];
  test 0x0.570020d1941ffp-1022;
  [%expect {| 0 |}];
  test (-36.7);
  [%expect {| -37 |}];
  test (-36.4);
  [%expect {| -36 |}];
  test (-36.5);
  [%expect {| -37 |}];
  require_does_raise (fun () -> test Float.nan);
  [%expect {| ("Cents.iround_exn: unexpected float value", { t = nan }) |}];
  require_does_raise (fun () -> test Float.infinity);
  [%expect {| ("Cents.iround_exn: unexpected float value", { t = infinity }) |}];
  ()
;;
