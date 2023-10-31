type t =
  { interval : Interval.t
  ; acoustic_interval : Acoustic_interval.t
  }
[@@deriving sexp_of]

let allowed_deviation_from_equal_tempered_12_in_cents =
  (* This is just a place holder for now, to be refined as needed. *)
  30.
;;

let check_deviation_exn ~interval ~acoustic_interval =
  let tempered_12_equivalent_in_cents =
    Acoustic_interval.equal_tempered_12 interval |> Acoustic_interval.to_cents
  in
  let in_cents = Acoustic_interval.to_cents acoustic_interval in
  let deviation = Float.abs (tempered_12_equivalent_in_cents -. in_cents) in
  if Float.compare deviation allowed_deviation_from_equal_tempered_12_in_cents > 0
  then
    raise_s
      [%sexp
        "Deviation is out of allowed bounds"
        , [%here]
        , { interval : Interval.t
          ; acoustic_interval : Acoustic_interval.t
          ; tempered_12_equivalent_in_cents : Float.t
          ; in_cents : Float.t
          ; deviation : Float.t
          ; allowed_deviation_from_equal_tempered_12_in_cents : Float.t
          }]
;;

let create_exn ~interval ~acoustic_interval =
  check_deviation_exn ~interval ~acoustic_interval;
  { interval; acoustic_interval }
;;
