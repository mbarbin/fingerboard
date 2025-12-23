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
let phys_equal a b = a == b

module Array = struct
  include Stdlib.ArrayLabels

  let create ~len a = make len a
  let exists t ~f = exists ~f t
  let iter t ~f = iter ~f t
  let fold t ~init ~f = fold_left t ~init ~f
  let map t ~f = map ~f t

  let rev t =
    let len = Array.length t in
    let pred_len = pred len in
    init len ~f:(fun i -> t.(pred_len - i))
  ;;
end

module Bool = struct
  include Stdlib.Bool

  let compare a b = Ordering.of_int (Bool.compare a b)
end

module Float = struct
  include Stdlib.Float

  let compare a b = Ordering.of_int (Float.compare a b)

  let iround_nearest_exn f =
    let i = Int.of_float f in
    if i >= 0
    then (
      match compare (f -. Int.to_float i) 0.5 with
      | Gt -> i + 1
      | Eq | Lt -> i)
    else (
      match compare (Int.to_float i -. f) 0.5 with
      | Gt -> i - 1
      | Eq | Lt -> i)
  ;;
end

module Int = struct
  include Stdlib.Int

  let compare a b = Ordering.of_int (Int.compare a b)
  let incr = Stdlib.incr
end

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

  let sort t ~compare = sort ~cmp:(fun a b -> compare a b |> Ordering.to_int) t

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
        (match compare x y with
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

module String = struct
  include Stdlib.StringLabels

  let chop_prefix t ~prefix =
    if String.starts_with t ~prefix
    then (
      let prefix_len = String.length prefix in
      Some (StringLabels.sub t ~pos:prefix_len ~len:(String.length t - prefix_len)))
    else None
  ;;

  let compare a b = String.compare a b |> Ordering.of_int
end

let print_endline = Stdlib.print_endline
let print_string = Stdlib.print_string
let force = `use_Lazy_dot_force
let raise_s = `use_Code_error_dot_raise
