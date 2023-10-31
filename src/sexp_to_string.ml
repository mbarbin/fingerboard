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
