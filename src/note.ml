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

  let succ = function
    | A -> B
    | B -> C
    | C -> D
    | D -> E
    | E -> F
    | F -> G
    | G -> A
  ;;

  let pred = function
    | A -> G
    | B -> A
    | C -> B
    | D -> C
    | E -> D
    | F -> E
    | G -> F
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

  let succ_octave_designation t ~octave_designation =
    octave_designation + if equal t B then 1 else 0
  ;;

  let pred_octave_designation t ~octave_designation =
    octave_designation - if equal t C then 1 else 0
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

  let succ = function
    | Triple_flat -> Some Double_flat
    | Double_flat -> Some Flat
    | Flat -> Some Natural
    | Natural -> Some Sharp
    | Sharp -> Some Double_sharp
    | Double_sharp -> Some Triple_sharp
    | Triple_sharp -> None
  ;;

  let pred = function
    | Triple_flat -> None
    | Double_flat -> Some Triple_flat
    | Flat -> Some Double_flat
    | Natural -> Some Flat
    | Sharp -> Some Natural
    | Double_sharp -> Some Sharp
    | Triple_sharp -> Some Double_sharp
  ;;
end

type t =
  { letter_name : Letter_name.t
  ; symbol : Symbol.t
  ; octave_designation : Octave_designation.t
  }
[@@deriving compare, equal, hash, sexp_of]

let to_string { letter_name; symbol; octave_designation } =
  Letter_name.to_string letter_name
  ^ Symbol.to_string symbol
  ^ Int.to_string octave_designation
;;
