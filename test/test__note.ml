(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

let%expect_test "semitons_step" =
  let rec aux start acc index =
    if Note.Letter_name.equal start index && acc > 0
    then acc
    else (
      let acc = acc + Note.Letter_name.semitons_step ~from:index in
      aux start acc (Note.Letter_name.succ index))
  in
  let number_of_semitons_per_octave = aux A 0 A in
  print_dyn
    (Dyn.Record [ "number_of_semitons_per_octave", Dyn.int number_of_semitons_per_octave ]);
  [%expect {| { number_of_semitons_per_octave = 12 } |}]
;;
