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

  let to_dyn { pythagorean; zarlinean } =
    Dyn.record
      [ "pythagorean", pythagorean |> Dyn.bool; "zarlinean", zarlinean |> Dyn.bool ]
  ;;

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

type t = Degree_kind.t array

let to_dyn t = Dyn.array Degree_kind.to_dyn t

let array_compare cmp a b : Ordering.t =
  let exception Result of Ordering.t in
  if phys_equal a b
  then Eq
  else (
    match Int.compare (Array.length a) (Array.length b) with
    | (Lt | Gt) as r -> r
    | Eq ->
      (match
         Array.iter2 a b ~f:(fun a b ->
           match (cmp a b : Ordering.t) with
           | Eq -> ()
           | (Lt | Gt) as r -> raise_notrace (Result r))
       with
       | () -> Eq
       | exception Result r -> r))
;;

let compare a b = array_compare Degree_kind.compare a b
let equal t1 t2 = Ordering.is_eq (compare t1 t2)

module For_comparison = struct
  type nonrec t = int array * t

  let compare ((i1, t1) as a1) ((i2, t2) as a2) : Ordering.t =
    if phys_equal a1 a2
    then Eq
    else (
      match array_compare Int.compare i1 i2 with
      | (Lt | Gt) as r -> r
      | Eq -> compare t1 t2)
  ;;

  let of_t t : t =
    let changing = Array.map t ~f:(fun d -> 3 - Degree_kind.count d) in
    changing, t
  ;;
end

module With_compare = struct
  type nonrec t = t

  let compare (t1 : t) t2 =
    For_comparison.compare (For_comparison.of_t t1) (For_comparison.of_t t2)
  ;;
end

let compare = With_compare.compare

let check t =
  let exception Invalid_entry of (string * Dyn.t) in
  match
    let len = Array.length t in
    if len <> 7 then Stdlib.raise_notrace (Invalid_entry ("Unexpected length.", to_dyn t));
    for i = 0 to 6 do
      if Degree_kind.count t.(i) < 1
      then
        Stdlib.raise_notrace
          (Invalid_entry
             ("Unexpected degree.", Dyn.Tuple [ Dyn.int i; Degree_kind.to_dyn t.(i) ]))
    done;
    let () =
      let changing_degrees = ref 0 in
      Array.iter t ~f:(fun d -> if Degree_kind.count d > 1 then Int.incr changing_degrees);
      if !changing_degrees > 1
      then Stdlib.raise_notrace (Invalid_entry ("Too many changing degrees.", to_dyn t))
    in
    for i = 0 to 6 do
      let low_note = t.(i)
      and high_note = t.((i + 2) mod len) in
      let low_p = low_note.pythagorean && high_note.zarlinean
      and high_p = low_note.zarlinean && high_note.pythagorean in
      if not (low_p || high_p)
      then
        Stdlib.raise_notrace
          (Invalid_entry
             ( "Unsolvable degree."
             , Dyn.Tuple
                 [ Dyn.int i; Degree_kind.to_dyn low_note; Degree_kind.to_dyn high_note ]
             ));
      if low_p && high_p
      then
        Stdlib.raise_notrace
          (Invalid_entry
             ( "Multiple choices."
             , Dyn.Tuple
                 [ Dyn.int i; Degree_kind.to_dyn low_note; Degree_kind.to_dyn high_note ]
             ));
      ()
    done
  with
  | () -> Ok ()
  | exception Invalid_entry err -> Error err
;;

let is_valid t =
  match check t with
  | Ok () -> true
  | Error (_ : string * Dyn.t) -> false
;;

let all () =
  let all = ref [] in
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
        let high_index = (i + 2) mod len in
        let low_note = t.(i)
        and high_note = t.(high_index) in
        let low_p = low_note.pythagorean && high_note.zarlinean
        and high_p = low_note.zarlinean && high_note.pythagorean in
        if low_p || high_p
        then if i = 6 then all := t :: !all else scale t (i + 1)
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
  List.filter !all ~f:is_valid
  |> List.sort ~compare
  |> List.group ~break:(fun x y -> not (compare x y |> Ordering.is_eq))
  |> List.map ~f:List.hd
;;

let all = lazy (all ())

let to_ascii_tables t =
  let degrees =
    let columns =
      List.mapi (Array.to_list t) ~f:(fun i _t ->
        Print_table.Column.make
          ~header:Roman_numeral.(to_string (of_int_exn (i + 1)))
          (fun t -> Print_table.Cell.text (Degree_kind.to_string t.(i))))
    in
    Print_table.to_string_text (Print_table.make ~columns ~rows:[ t ])
  in
  [ degrees ] |> String.concat ~sep:"\n"
;;
