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
