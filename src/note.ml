open! Core

module Letter_name = struct
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
    | G
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  let to_string t =
    match [%sexp_of: t] t with
    | Atom s -> s
    | List _ as sexp ->
      raise_s [%sexp "Unexpected sexp shape", { t : t; sexp : Sexp.t }, [%here]]
  ;;

  let next = function
    | A -> B
    | B -> C
    | C -> D
    | D -> E
    | E -> F
    | F -> G
    | G -> A
  ;;

  let semitons_step ~from =
    match from with
    | A -> 2
    | B -> 1
    | C -> 2
    | D -> 2
    | E -> 1
    | F -> 2
    | G -> 2
  ;;
end

module Symbol = struct
  type t =
    | Triple_flat
    | Double_flat
    | Flat
    | Natural
    | Sharp
    | Double_sharp
    | Triple_sharp
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  let to_string = function
    | Triple_flat -> "bbb"
    | Double_flat -> "bb"
    | Flat -> "b"
    | Natural -> ""
    | Sharp -> "#"
    | Double_sharp -> "##"
    | Triple_sharp -> "###"
  ;;

  let prefix_notation = function
    | Triple_flat -> "bbb"
    | Double_flat -> "bb"
    | Flat -> "b"
    | Natural -> ""
    | Sharp -> "#"
    | Double_sharp -> "x"
    | Triple_sharp -> "#x"
  ;;

  let semitons_shift = function
    | Triple_flat -> -3
    | Double_flat -> -2
    | Flat -> -1
    | Natural -> 0
    | Sharp -> 1
    | Double_sharp -> 2
    | Triple_sharp -> 3
  ;;
end

type t =
  { letter_name : Letter_name.t
  ; symbol : Symbol.t
  }
[@@deriving compare, enumerate, equal, hash, sexp_of]

let to_string { letter_name; symbol } =
  Letter_name.to_string letter_name ^ Symbol.to_string symbol
;;
