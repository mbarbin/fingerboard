open! Core

type t =
  { name : string
  ; basis_vibrating_string_portion : Vibrating_string_portion.t
  ; at_octave : int
  }
[@@deriving compare, equal, hash, sexp_of]

let to_string t = sprintf "%s-%d" t.name t.at_octave

let acoustic_interval_to_the_open_string t =
  Vibrating_string_portion.acoustic_interval_to_the_open_string
    t.basis_vibrating_string_portion
  :: List.init t.at_octave ~f:(const Acoustic_interval.octave)
  |> List.reduce ~f:Acoustic_interval.add
  |> Option.value ~default:Acoustic_interval.unison
;;

let at_octave t ~octave = { t with at_octave = octave }

let create_exn ~name ~acoustic_interval_to_the_open_string =
  let in_cents = Acoustic_interval.to_cents acoustic_interval_to_the_open_string in
  if Float.compare in_cents Acoustic_interval.(to_cents octave) >= 0
  then
    raise_s
      [%sexp
        "Interval out of bounds"
        , [%here]
        , { name : string
          ; acoustic_interval_to_the_open_string : Acoustic_interval.t
          ; in_cents : Float.t
          }];
  let basis_vibrating_string_portion =
    Vibrating_string_portion.of_acoustic_interval_to_the_open_string
      acoustic_interval_to_the_open_string
  in
  { name; basis_vibrating_string_portion; at_octave = 0 }
;;

let open_string =
  create_exn ~name:"0" ~acoustic_interval_to_the_open_string:Acoustic_interval.unison
;;
