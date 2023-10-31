type t = float

let to_string_nearest t =
  let nearest = t |> Float.iround_exn ~dir:`Nearest in
  Int.to_string nearest
;;
