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

(* Some functions are copied from [Base] version [v0.17] which is released
   under MIT and may be found at [https://github.com/janestreet/base].

   See Base's LICENSE below:

   ----------------------------------------------------------------------------

   The MIT License

   Copyright (c) 2016--2024 Jane Street Group, LLC <opensource-contacts@janestreet.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.

   ----------------------------------------------------------------------------

   When this is the case, we clearly indicate it next to the copied function. *)

module Code_error = Code_error
module Dyn = Dyn0
module Ordering = Ordering

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)

module List = struct
  include Stdlib.ListLabels

  let filter t ~f = filter ~f t
  let filter_map t ~f = filter_map ~f t
  let init len ~f = init ~len ~f
  let iter t ~f = iter ~f t
  let map t ~f = map ~f t
  let mapi t ~f = mapi ~f t
  let partition t ~f = partition ~f t

  let reduce t ~f =
    match t with
    | [] -> None
    | hd :: tl -> Some (Stdlib.ListLabels.fold_left tl ~init:hd ~f)
  ;;

  let sort t ~compare = sort ~cmp:compare t

  (* ---------------------------------------------------------------------------- *)
  (* [groupi] and [group] are copied from [Base] (MIT). See notice at the top of
     the file and project global notice for licensing information. *)
  let groupi l ~break =
    (* We allocate shared position and list references so we can make the inner loop use
       [[@tail_mod_cons]], and still return back information about position and where in the
       list we left off. *)
    let pos = ref 0 in
    let l = ref l in
    (* As a result of using local references, our inner loop does not need arguments. *)
    let[@tail_mod_cons] rec take_group () =
      match !l with
      | ([] | [ _ ]) as group ->
        l := [];
        group
      | x :: (y :: _ as tl) ->
        pos := !pos + 1;
        l := tl;
        if break !pos x y then [ x ] else x :: take_group ()
    in
    (* Our outer loop does not need arguments, either. *)
    let[@tail_mod_cons] rec groups () =
      if is_empty !l
      then []
      else (
        let group = take_group () in
        group :: groups ())
    in
    (groups () [@nontail])
  ;;

  let group l ~break = groupi l ~break:(fun _ x y -> break x y) [@nontail]
  (* ---------------------------------------------------------------------------- *)

  let[@tail_mod_cons] rec range start stop =
    if start >= stop then [] else start :: range (start + 1) stop
  ;;

  let sort_then_dedup t ~compare =
    let sorted = sort t ~compare in
    let[@tail_mod_cons] rec dedup = function
      | [] -> []
      | [ x ] -> [ x ]
      | x :: (y :: _ as rest) ->
        (match compare x y |> Ordering.of_int with
         | Eq -> dedup rest
         | Lt | Gt -> x :: dedup rest)
    in
    dedup sorted
  ;;
end

module Option = struct
  include Stdlib.Option

  let bind t ~f = bind t f
  let map t ~f = map f t
end

let print_endline = Stdlib.print_endline
let print_string = Stdlib.print_string
let force = `use_Lazy_dot_force
let raise_s = `use_Code_error_dot_raise
