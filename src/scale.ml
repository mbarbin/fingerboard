(*********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                *)
(*  SPDX-FileCopyrightText: 2022-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                                   *)
(*********************************************************************************)

type t = { consecutive_intervals : Interval.t array }

let create consecutive_intervals = { consecutive_intervals }

let fold_map_opt t ~init ~f =
  let res = ref [] in
  let rec aux acc i =
    res := acc :: !res;
    if i < Array.length t
    then (
      match f t.(i) acc with
      | None -> ()
      | Some acc -> aux acc (Int.succ i))
  in
  aux init 0;
  Array.of_list (List.rev !res)
;;

let ascending t ~from =
  fold_map_opt t.consecutive_intervals ~init:from ~f:Interval.shift_up
;;

let descending t ~from =
  fold_map_opt (t.consecutive_intervals |> Array.rev) ~init:from ~f:Interval.shift_down
;;

let major =
  lazy
    (let ton = Interval.{ number = Second; quality = Major; additional_octaves = 0 } in
     let semiton =
       Interval.{ number = Second; quality = Minor; additional_octaves = 0 }
     in
     create [| ton; ton; semiton; ton; ton; ton; semiton |])
;;

let minor =
  lazy
    (let ton = Interval.{ number = Second; quality = Major; additional_octaves = 0 } in
     let semiton =
       Interval.{ number = Second; quality = Minor; additional_octaves = 0 }
     in
     create [| ton; semiton; ton; ton; semiton; ton; ton |])
;;
