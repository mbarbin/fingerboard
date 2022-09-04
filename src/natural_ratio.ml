open! Core

type t =
  { numerator : int
  ; denominator : int
  }
[@@deriving sexp_of]

let create_exn ~numerator ~denominator =
  assert (numerator > 0);
  assert (denominator > 0);
  { numerator; denominator }
;;

let equal { numerator = n1; denominator = d1 } { numerator = n2; denominator = d2 } =
  Int.equal (n1 * d2) (d1 * n2)
;;

let to_string { numerator = a; denominator = b } =
  if b = 1 then sprintf "%d" a else sprintf "%d / %d" a b
;;

let inverse { numerator = a; denominator = b } = { numerator = b; denominator = a }

let multiply { numerator = n1; denominator = d1 } { numerator = n2; denominator = d2 } =
  { numerator = n1 * n2; denominator = d1 * d2 }
;;

let divide t1 t2 = multiply t1 (inverse t2)

module Reduced = struct
  module One = struct
    type t =
      { prime : int
      ; exponent : int
      }
    [@@deriving equal, sexp_of]

    let to_string { prime; exponent } =
      if exponent = 1 then sprintf "%d" prime else sprintf "%d ^ %d" prime exponent
    ;;

    let inverse { prime; exponent = e } = { prime; exponent = -1 * e }
  end

  type t = One.t list [@@deriving equal, sexp_of]

  let one = []

  let to_string t =
    if List.is_empty t
    then "1"
    else (
      let numerator, denominator =
        List.partition_tf t ~f:(fun (o : One.t) -> o.exponent > 0)
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
      else sprintf "%s / %s" (ones numerator) (ones denominator))
  ;;

  let create_exn ~prime ~exponent =
    assert (exponent <> 0);
    [ { One.prime; exponent } ]
  ;;

  let power n i =
    assert (i > 0);
    let rec aux acc i = if i = 0 then acc else aux (acc * n) (pred i) in
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
end