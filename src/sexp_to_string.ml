open! Core

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
