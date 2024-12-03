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

module Degree_kind = struct
  type t =
    { pythagorean : bool
    ; zarlinean : bool
    }
  [@@deriving equal, sexp_of]

  let null = { pythagorean = false; zarlinean = false }
  let count_bool b = if b then 1 else 0
  let count { pythagorean; zarlinean } = count_bool pythagorean + count_bool zarlinean

  let to_int { pythagorean; zarlinean } =
    (count_bool zarlinean * 2) + count_bool pythagorean
  ;;

  let compare t1 t2 = Int.compare (to_int t1) (to_int t2)

  let to_string t =
    match t with
    | { pythagorean = true; zarlinean = false } -> "p"
    | { pythagorean = false; zarlinean = true } -> "z"
    | { pythagorean = true; zarlinean = true } -> "p,z"
    | { pythagorean = false; zarlinean = false } -> ""
  ;;
end

type t = Degree_kind.t array [@@deriving compare, equal, sexp_of]

module For_comparison = struct
  type nonrec t = int array * t [@@deriving compare]

  let of_t t =
    let changing = Array.map t ~f:(fun d -> 3 - Degree_kind.count d) in
    changing, t
  ;;
end

let compare t1 t2 = For_comparison.(compare (of_t t1) (of_t t2))

let check t =
  With_return.with_return (fun { return } ->
    let len = Array.length t in
    if len <> 7
    then return (Or_error.error_s [%sexp "unexpected length", [%here], (t : t)]);
    for i = 0 to 6 do
      if Degree_kind.count t.(i) < 1
      then
        return
          (Or_error.error_s
             [%sexp "unexpected degree", [%here], { i : int }, (t.(i) : Degree_kind.t)])
    done;
    if Array.count t ~f:(fun d -> Degree_kind.count d > 1) > 1
    then return (Or_error.error_s [%sexp "too many changing degrees", [%here], (t : t)]);
    for i = 0 to 6 do
      let low_note = t.(i)
      and high_note = t.((i + 2) % len) in
      let low_p = low_note.pythagorean && high_note.zarlinean
      and high_p = low_note.zarlinean && high_note.pythagorean in
      if not (low_p || high_p)
      then
        return
          (Or_error.error_s
             [%sexp
               "unsolvable degree"
             , [%here]
             , { i : int }
             , { low_note : Degree_kind.t; high_note : Degree_kind.t }]);
      if low_p && high_p
      then
        return
          (Or_error.error_s
             [%sexp
               "multiple choices"
             , [%here]
             , { i : int }
             , { low_note : Degree_kind.t; high_note : Degree_kind.t }]);
      ()
    done;
    Ok ())
;;

let all () =
  let all = Queue.create () in
  let copy t i ~f =
    let t' = Array.copy t in
    t'.(i) <- f t.(i);
    t'
  in
  let len = 7 in
  let t = Array.create ~len Degree_kind.null in
  let rec aux t i =
    match Degree_kind.count t.(i) with
    | 0 ->
      aux (copy t i ~f:(fun d -> { d with pythagorean = true })) i;
      aux (copy t i ~f:(fun d -> { d with zarlinean = true })) i
    | _ ->
      let rec scale (t : t) i =
        let high_index = (i + 2) % len in
        let low_note = t.(i)
        and high_note = t.(high_index) in
        let low_p = low_note.pythagorean && high_note.zarlinean
        and high_p = low_note.zarlinean && high_note.pythagorean in
        if low_p || high_p
        then if i = 6 then Queue.enqueue all t else scale t (i + 1)
        else (
          if not low_note.pythagorean
          then scale (copy t i ~f:(fun d -> { d with pythagorean = true })) i;
          if not low_note.zarlinean
          then scale (copy t i ~f:(fun d -> { d with zarlinean = true })) i;
          if not high_note.pythagorean
          then scale (copy t high_index ~f:(fun d -> { d with pythagorean = true })) i;
          if not high_note.zarlinean
          then scale (copy t high_index ~f:(fun d -> { d with zarlinean = true })) i)
      in
      scale t i
  in
  aux t 0;
  let all = Queue.to_list all in
  List.filter_map all ~f:(fun t ->
    match check t with
    | Ok () -> Some t
    | Error _ -> None)
  |> List.sort_and_group ~compare
  |> List.map ~f:List.hd_exn
;;

let all = lazy (all ())

let to_ascii_tables t =
  let degrees =
    let columns =
      List.mapi (Array.to_list t) ~f:(fun i _t ->
        Ascii_table.Column.create_attr
          Roman_numeral.(to_string (of_int_exn (i + 1)))
          (fun t -> [], Degree_kind.to_string t.(i)))
    in
    Ascii_table.to_string columns [ t ]
  in
  [ degrees ] |> String.concat ~sep:"\n"
;;
