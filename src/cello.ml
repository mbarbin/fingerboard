open! Core

module Fingerboard_position_name = struct
  module Edo12 = struct
    type t =
      [ `m2e
      | `M2e
      | `m3e
      | `M3e
      | `P4e
      | `A4e
      | `P5e
      | `m6e
      | `M6e
      | `m7e
      | `M7e
      ]
    [@@deriving compare, equal, hash, sexp_of]

    let sexp_of_t : t -> Sexp.t = function
      | `P4e -> Atom "4e"
      | `P5e -> Atom "5e"
      | (`m2e | `M2e | `m3e | `M3e | `A4e | `m6e | `M6e | `m7e | `M7e) as t -> sexp_of_t t
    ;;

    let acoustic_interval_to_the_open_string : t -> Acoustic_interval.t = function
      | `m2e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:1
      | `M2e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:2
      | `m3e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:3
      | `M3e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:4
      | `P4e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:5
      | `A4e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:6
      | `P5e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:7
      | `m6e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:8
      | `M6e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:9
      | `m7e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:10
      | `M7e ->
        Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions:11
    ;;
  end

  type t =
    [ `open_string
    | Edo12.t
    ]
  [@@deriving compare, equal, hash, sexp_of]

  let sexp_of_t : t -> Sexp.t = function
    | `open_string -> Atom "0"
    | #Edo12.t as t -> [%sexp (t : Edo12.t)]
  ;;

  let acoustic_interval_to_the_open_string : t -> Acoustic_interval.t = function
    | `open_string -> Acoustic_interval.unison
    | #Edo12.t as t -> Edo12.acoustic_interval_to_the_open_string t
  ;;
end

let fingerboard_position name =
  Fingerboard_position.create_exn
    ~name:(Sexp.to_string [%sexp (name : Fingerboard_position_name.t)])
    ~acoustic_interval_to_the_open_string:
      (Fingerboard_position_name.acoustic_interval_to_the_open_string name)
;;
