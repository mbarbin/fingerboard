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
end

module Symbol = struct
  type t =
    | Natural
    | Flat
    | Sharp
    | Double_sharp
    | Double_flat
    | Triple_sharp
    | Triple_flat
  [@@deriving compare, enumerate, equal, hash, sexp_of]

  let to_string = function
    | Natural -> ""
    | Flat -> "b"
    | Sharp -> "#"
    | Double_sharp -> "##"
    | Double_flat -> "bb"
    | Triple_sharp -> "###"
    | Triple_flat -> "bbb"
  ;;

  let prefix_notation = function
    | Natural -> ""
    | Flat -> "b"
    | Sharp -> "#"
    | Double_sharp -> "x"
    | Double_flat -> "bb"
    | Triple_sharp -> "#x"
    | Triple_flat -> "bbb"
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
