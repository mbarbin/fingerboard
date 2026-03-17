(*********************************************************************************)
(*  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*********************************************************************************)

(* This is to silence `dune build @unused-libs` and keeping intended deps. *)
open! Fingerboard_stdlib

let%expect_test "empty" =
  ();
  [%expect {||}];
  ()
;;
