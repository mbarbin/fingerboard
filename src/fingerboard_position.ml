open! Core

type t =
  { name : string
  ; at_octave : int
  ; basis_acoustic_interval_to_the_open_string : Acoustic_interval.t
  }
[@@deriving compare, equal, hash, sexp_of]

let name t = t.name
let to_string t = if t.at_octave = 0 then t.name else sprintf "%s-%d" t.name t.at_octave

let acoustic_interval_to_the_open_string t =
  t.basis_acoustic_interval_to_the_open_string
  :: List.init t.at_octave ~f:(const Acoustic_interval.octave)
  |> List.reduce ~f:Acoustic_interval.add
  |> Option.value ~default:Acoustic_interval.unison
;;

let ascii_table_columns =
  Ascii_table.Column.
    [ create_attr "Name" (fun t -> [], to_string t)
    ; create_attr ~align:Right "Interval to the open string" (fun t ->
        let acoustic_interval = acoustic_interval_to_the_open_string t in
        let cents = Acoustic_interval.to_cents acoustic_interval in
        ( []
        , sprintf "%s (%0.2f cents)" (Acoustic_interval.to_string acoustic_interval) cents
        ))
    ]
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
  ; at_octave = 0
  ; basis_acoustic_interval_to_the_open_string = acoustic_interval_to_the_open_string
  }
;;

let open_string =
  create_exn ~name:"0" ~acoustic_interval_to_the_open_string:Acoustic_interval.unison
;;
