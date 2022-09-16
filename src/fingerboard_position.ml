open! Core

type t =
  { name : string
  ; basis_acoustic_interval_to_the_open_string : Acoustic_interval.t
  ; at_octave : int
  }
[@@deriving compare, equal, hash, sexp_of]

let name t = t.name
let to_string t = sprintf "%s-%d" t.name t.at_octave

let acoustic_interval_to_the_open_string t =
  t.basis_acoustic_interval_to_the_open_string
  :: List.init t.at_octave ~f:(const Acoustic_interval.octave)
  |> List.reduce ~f:Acoustic_interval.add
  |> Option.value ~default:Acoustic_interval.unison
;;

let at_octave t ~octave = { t with at_octave = octave }

let create_exn ~name ~acoustic_interval_to_the_open_string =
  if Acoustic_interval.compare
       acoustic_interval_to_the_open_string
       Acoustic_interval.octave
     >= 0
  then
    raise_s
      [%sexp
        "Interval out of bounds"
        , [%here]
        , { name : string
          ; acoustic_interval_to_the_open_string : Acoustic_interval.t
          ; in_cents =
              (Acoustic_interval.to_cents acoustic_interval_to_the_open_string : Float.t)
          }];
  { name
  ; basis_acoustic_interval_to_the_open_string = acoustic_interval_to_the_open_string
  ; at_octave = 0
  }
;;

let open_string =
  create_exn ~name:"0" ~acoustic_interval_to_the_open_string:Acoustic_interval.unison
;;
