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

type t =
  { numerator : int
  ; denominator : int
  }

let to_dyn { numerator; denominator } =
  Dyn.record [ "numerator", numerator |> Dyn.int; "denominator", denominator |> Dyn.int ]
;;

let create_exn ~numerator ~denominator =
  assert (numerator > 0);
  assert (denominator > 0);
  { numerator; denominator }
;;

let equal { numerator = n1; denominator = d1 } { numerator = n2; denominator = d2 } =
  Int.equal (n1 * d2) (d1 * n2)
;;

let to_string { numerator = a; denominator = b } =
  if b = 1 then Printf.sprintf "%d" a else Printf.sprintf "%d / %d" a b
;;

let inverse { numerator = a; denominator = b } = { numerator = b; denominator = a }

let multiply { numerator = n1; denominator = d1 } { numerator = n2; denominator = d2 } =
  { numerator = n1 * n2; denominator = d1 * d2 }
;;

let divide t1 t2 = multiply t1 (inverse t2)

let primes_to_100 =
  [ 2
  ; 3
  ; 5
  ; 7
  ; 11
  ; 13
  ; 17
  ; 19
  ; 23
  ; 29
  ; 31
  ; 37
  ; 41
  ; 43
  ; 47
  ; 53
  ; 59
  ; 61
  ; 67
  ; 71
  ; 73
  ; 79
  ; 83
  ; 89
  ; 97
  ]
  |> Set.of_list (module Int)
;;

module Reduced = struct
  module One = struct
    type t =
      { prime : int
      ; exponent : int
      }
    [@@deriving equal]

    let to_dyn { prime; exponent } =
      Dyn.record [ "prime", prime |> Dyn.int; "exponent", exponent |> Dyn.int ]
    ;;

    let to_string { prime; exponent } =
      if exponent = 1
      then Printf.sprintf "%d" prime
      else Printf.sprintf "%d^%d" prime exponent
    ;;

    let inverse { prime; exponent = e } = { prime; exponent = -1 * e }
  end

  type t = One.t list [@@deriving equal]

  let to_dyn t = Dyn.list One.to_dyn t
  let one = []

  let to_string t =
    if List.is_empty t
    then "1"
    else (
      let numerator, denominator =
        List.partition t ~f:(fun (o : One.t) -> o.exponent > 0)
      in
      let denominator = List.map denominator ~f:One.inverse in
      let ones = function
        | [] -> "1"
        | [ hd ] -> One.to_string hd
        | _ :: _ as list ->
          "(" ^ String.concat ~sep:" * " (List.map list ~f:One.to_string) ^ ")"
      in
      if List.is_empty denominator
      then ones numerator
      else Printf.sprintf "%s / %s" (ones numerator) (ones denominator))
  ;;

  let create_exn ~prime ~exponent =
    if prime < 100 && not (Set.mem primes_to_100 prime)
    then Code_error.raise "Not a prime number." [ "number", prime |> Dyn.int ];
    if exponent = 0
    then
      Code_error.raise
        "Non-null exponent expected."
        [ "prime", prime |> Dyn.int; "exponent", exponent |> Dyn.int ];
    [ { One.prime; exponent } ]
  ;;

  let power n i =
    assert (i > 0);
    let rec aux acc i = if i = 0 then acc else aux (acc * n) (Int.pred i) in
    aux 1 i
  ;;

  let to_natural_ratio t =
    let rec aux numerator denominator = function
      | [] -> { numerator; denominator }
      | { One.prime; exponent } :: tl ->
        if exponent = 0
        then aux numerator denominator tl
        else if exponent > 0
        then aux (numerator * power prime exponent) denominator tl
        else (* exponent < 0 *)
          aux numerator (denominator * power prime (-exponent)) tl
    in
    aux 1 1 t
  ;;

  let inverse t =
    List.map t ~f:(fun { One.prime; exponent } -> { One.prime; exponent = -1 * exponent })
  ;;

  let multiply t1 t2 =
    let rec aux (t1 : t) (t2 : t) =
      match t1, t2 with
      | [], t | t, [] -> t
      | hd1 :: tl1, hd2 :: tl2 ->
        if hd1.prime < hd2.prime
        then hd1 :: aux tl1 t2
        else if hd1.prime > hd2.prime
        then hd2 :: aux t1 tl2
        else (
          assert (hd1.prime = hd2.prime);
          let exponent = hd1.exponent + hd2.exponent in
          if exponent = 0
          then aux tl1 tl2
          else { One.prime = hd1.prime; exponent } :: aux tl1 tl2)
    in
    aux t1 t2
  ;;

  let divide t1 t2 = multiply t1 (inverse t2)
  let compound ts = List.reduce ts ~f:multiply |> Option.value ~default:one

  let of_small_natural_ratio_exn ~numerator ~denominator =
    match
      let ( let* ) x f = Option.bind x ~f in
      let rec aux acc i ~exponent =
        if i = 1
        then Option.some acc
        else
          let* prime =
            match Set.find primes_to_100 ~f:(fun prime -> i % prime = 0) with
            | Some _ as some -> some
            | None -> if i < 10_000 then Some i else None
          in
          aux (multiply acc (create_exn ~prime ~exponent)) (i / prime) ~exponent
      in
      let* acc = aux one numerator ~exponent:1 in
      aux acc denominator ~exponent:(-1)
    with
    | Some t -> t
    | None ->
      Code_error.raise
        "Unsupported ratio."
        [ "numerator", numerator |> Dyn.int; "denominator", denominator |> Dyn.int ]
  ;;
end
