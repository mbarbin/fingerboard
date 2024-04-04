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

let atom_to_name t sexp_of_t =
  let atom =
    match [%sexp_of: t] t with
    | Sexp.Atom s -> s
    | List _ -> raise_s [%sexp "Unexpected sexp", [%here], (t : t)]
  in
  atom
  |> String.uncapitalize
  |> String.map ~f:(function
    | '_' -> ' '
    | c -> c)
;;

let position sexp_of_t t =
  match (sexp_of_t t : Sexp.t) with
  | List _ -> assert false
  | Atom atom ->
    let atom =
      String.map atom ~f:(function
        | '_' -> '-'
        | c -> c)
    in
    let atom = Option.value (String.chop_prefix atom ~prefix:"P") ~default:atom in
    Sexp.Atom atom
;;
