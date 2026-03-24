(*_********************************************************************************)
(*_  Fingerboard-stdlib - Extending OCaml's Stdlib for Fingerboard                *)
(*_  SPDX-FileCopyrightText: 2025-2026 Mathieu Barbin <mathieu.barbin@gmail.com>  *)
(*_  SPDX-License-Identifier: MIT OR AGPL-3.0-or-later                            *)
(*_********************************************************************************)

(*_ Notice: This file was vendored from [Dune], which we documented in the NOTICE
  at the root of the repo.

  The original license header was kept with the file, see below.

  List of changes:

  - Add [inline_record] constructor.
*)

(*_ The MIT License

  Copyright (c) 2016 Jane Street Group, LLC <opensource@janestreet.com>

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
  SOFTWARE. *)

(** Dynamic values *)

(** Representation of OCaml values such that they can be processed without
    knowing their type. *)
type t = Dyn.t =
  | Opaque
  | Unit
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Bool of bool
  | String of string
  | Bytes of bytes
  | Char of char
  | Float of float
  | Option of t option
  | List of t list
  | Array of t array
  | Tuple of t list
  | Record of (string * t) list
  | Variant of string * t list
  | Map of (t * t) list
  | Set of t list

val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val pp : t -> _ Pp.t
val to_string : t -> string

(** {1 Constructors} *)

type 'a builder = 'a -> t

val unit : unit builder
val char : char builder
val string : string builder
val int : int builder
val int32 : int32 builder
val int64 : int64 builder
val nativeint : nativeint builder
val float : float builder
val bool : bool builder
val pair : 'a builder -> 'b builder -> ('a * 'b) builder
val triple : 'a builder -> 'b builder -> 'c builder -> ('a * 'b * 'c) builder
val list : 'a builder -> 'a list builder
val array : 'a builder -> 'a array builder
val option : 'a builder -> 'a option builder
val opaque : _ builder
val record : (string * t) list -> t
val variant : string -> t list -> t
val result : 'a builder -> 'error builder -> ('a, 'error) result builder
val inline_record : string -> (string * Dyn.t) list -> Dyn.t
