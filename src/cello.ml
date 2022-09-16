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
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    let sexp_of_t : t -> Sexp.t = function
      | `P4e -> Atom "4e"
      | `P5e -> Atom "5e"
      | (`m2e | `M2e | `m3e | `M3e | `A4e | `m6e | `M6e | `m7e | `M7e) as t -> sexp_of_t t
    ;;

    let acoustic_interval_to_the_open_string (t : t) =
      let number_of_divisions =
        match (t : t) with
        | `m2e -> 1
        | `M2e -> 2
        | `m3e -> 3
        | `M3e -> 4
        | `P4e -> 5
        | `A4e -> 6
        | `P5e -> 7
        | `m6e -> 8
        | `M6e -> 9
        | `m7e -> 10
        | `M7e -> 11
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:12 ~number_of_divisions
    ;;
  end

  module Edo53 = struct
    type t =
      [ `A1z_e53
      | `m2z_e53
      | `M2z_e53
      | `M2p_e53
      | `m3p_e53
      | `m3z_e53
      | `M3z_e53
      | `M3p_e53
      | `P4p_e53
      | `P4z_e53
      | `A4z_e53
      | `d5z_e53
      | `P5z_e53
      | `P5p_e53
      | `m6p_e53
      | `m6z_e53
      | `m7p_e53
      | `m7z_e53
      | `M7z_e53
      | `M7p_e53
      | `P8z_e53
      ]
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    let sexp_of_t : t -> Sexp.t = function
      | `P4p_e53 -> Atom "4p-e53"
      | `P4z_e53 -> Atom "4z-e53"
      | `P5z_e53 -> Atom "5z-e53"
      | `P5p_e53 -> Atom "5p-e53"
      | `P8z_e53 -> Atom "8z-e53"
      | ( `A1z_e53
        | `m2z_e53
        | `M2z_e53
        | `M2p_e53
        | `m3p_e53
        | `m3z_e53
        | `M3z_e53
        | `M3p_e53
        | `A4z_e53
        | `d5z_e53
        | `m6p_e53
        | `m6z_e53
        | `m7p_e53
        | `m7z_e53
        | `M7z_e53
        | `M7p_e53 ) as t ->
        (match sexp_of_t t with
         | List _ -> assert false
         | Atom atom ->
           Atom
             (String.map atom ~f:(function
               | '_' -> '-'
               | c -> c)))
    ;;

    let acoustic_interval_to_the_open_string (t : t) =
      let number_of_divisions =
        match (t : t) with
        | `A1z_e53 -> 4
        | `m2z_e53 -> 5
        | `M2z_e53 -> 8
        | `M2p_e53 -> 9
        | `m3p_e53 -> 13
        | `m3z_e53 -> 14
        | `M3z_e53 -> 17
        | `M3p_e53 -> 18
        | `P4p_e53 -> 22
        | `P4z_e53 -> 23
        | `A4z_e53 -> 26
        | `d5z_e53 -> 27
        | `P5z_e53 -> 30
        | `P5p_e53 -> 31
        | `m6p_e53 -> 35
        | `m6z_e53 -> 36
        | `m7p_e53 -> 44
        | `m7z_e53 -> 45
        | `M7z_e53 -> 48
        | `M7p_e53 -> 49
        | `P8z_e53 -> 52
      in
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions
    ;;
  end

  module Pythagorean = struct
    type t =
      [ `m2p
      | `A1p
      | `d3p
      | `M2p
      | `m3p
      | `A2p
      | `d4p
      | `M3p
      | `P4p
      | `A3p
      | `d5p
      | `A4p
      | `d6p
      | `P5p
      | `m6p
      | `A5p
      | `d7p
      | `M6p
      | `m7p
      | `A6p
      | `d8p
      | `M7p
      ]
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    let sexp_of_t : t -> Sexp.t = function
      | `P4p -> Atom "4p"
      | `P5p -> Atom "5p"
      | ( `m2p
        | `A1p
        | `d3p
        | `M2p
        | `m3p
        | `A2p
        | `d4p
        | `M3p
        | `A3p
        | `d5p
        | `A4p
        | `d6p
        | `m6p
        | `A5p
        | `d7p
        | `M6p
        | `m7p
        | `A6p
        | `d8p
        | `M7p ) as t -> sexp_of_t t
    ;;

    let acoustic_interval_to_the_open_string (t : t) =
      let interval =
        let ( ~. ) quality number =
          { Interval.number; quality; additional_octaves = 0 }
        in
        match (t : t) with
        | `m2p -> ~.Minor Second
        | `A1p -> ~.Augmented Unison
        | `d3p -> ~.Diminished Third
        | `M2p -> ~.Major Second
        | `m3p -> ~.Minor Third
        | `A2p -> ~.Augmented Second
        | `d4p -> ~.Diminished Fourth
        | `M3p -> ~.Major Third
        | `P4p -> ~.Perfect Fourth
        | `A3p -> ~.Augmented Third
        | `d5p -> ~.Diminished Fifth
        | `A4p -> ~.Augmented Fourth
        | `d6p -> ~.Diminished Sixth
        | `P5p -> ~.Perfect Fifth
        | `m6p -> ~.Minor Sixth
        | `A5p -> ~.Augmented Fifth
        | `d7p -> ~.Diminished Seventh
        | `M6p -> ~.Major Sixth
        | `m7p -> ~.Minor Seventh
        | `A6p -> ~.Augmented Sixth
        | `d8p -> ~.Diminished Octave
        | `M7p -> ~.Major Seventh
      in
      Acoustic_interval.pythagorean interval
    ;;
  end

  module Just = struct
    type t =
      [ `m2z
      | `M2z
      | `m3z
      | `M3z
      | `P4z
      | `d5z
      | `P5z
      | `m6z
      | `d8z
      | `P8z
      ]
    [@@deriving compare, equal, enumerate, hash, sexp_of]

    let sexp_of_t : t -> Sexp.t = function
      | `P4z -> Atom "4z"
      | `P5z -> Atom "5z"
      | `P8z -> Atom "8z"
      | (`m2z | `M2z | `m3z | `M3z | `d5z | `m6z | `d8z) as t -> sexp_of_t t
    ;;

    let acoustic_interval_to_the_open_string (t : t) =
      match (t : t) with
      | `m2z -> Acoustic_interval.just_diatonic_semiton
      | `M2z -> Acoustic_interval.just_minor_ton
      | `m3z -> Acoustic_interval.just_minor_third
      | `M3z -> Acoustic_interval.just_major_third
      | `P4z ->
        Acoustic_interval.(
          remove
            (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
            just_minor_ton)
        |> Option.value_exn ~here:[%here]
      | `d5z ->
        Acoustic_interval.(
          add
            (pythagorean { number = Fourth; quality = Perfect; additional_octaves = 0 })
            just_diatonic_semiton)
      | `P5z ->
        Acoustic_interval.(
          add
            (pythagorean { number = Fourth; quality = Perfect; additional_octaves = 0 })
            just_minor_ton)
      | `m6z ->
        Acoustic_interval.(
          add
            (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
            just_diatonic_semiton)
      | `d8z ->
        Acoustic_interval.(
          add
            (pythagorean { number = Seventh; quality = Minor; additional_octaves = 0 })
            just_diatonic_semiton)
      | `P8z ->
        Acoustic_interval.(
          add
            (pythagorean { number = Seventh; quality = Minor; additional_octaves = 0 })
            just_minor_ton)
    ;;
  end

  type t =
    [ `open_string
    | Edo12.t
    | Edo53.t
    | Pythagorean.t
    | Just.t
    ]
  [@@deriving compare, equal, hash, sexp_of]

  let sexp_of_t : t -> Sexp.t = function
    | `open_string -> Atom "0"
    | #Edo12.t as t -> [%sexp (t : Edo12.t)]
    | #Edo53.t as t -> [%sexp (t : Edo53.t)]
    | #Pythagorean.t as t -> [%sexp (t : Pythagorean.t)]
    | #Just.t as t -> [%sexp (t : Just.t)]
  ;;

  let acoustic_interval_to_the_open_string : t -> Acoustic_interval.t = function
    | `open_string -> Acoustic_interval.unison
    | #Edo12.t as t -> t |> Edo12.acoustic_interval_to_the_open_string
    | #Edo53.t as t -> t |> Edo53.acoustic_interval_to_the_open_string
    | #Pythagorean.t as t -> t |> Pythagorean.acoustic_interval_to_the_open_string
    | #Just.t as t -> t |> Just.acoustic_interval_to_the_open_string
  ;;

  let to_string t = Sexp.to_string [%sexp (t : t)]
end

let fingerboard_position name =
  Fingerboard_position.create_exn
    ~name:(Fingerboard_position_name.to_string name)
    ~acoustic_interval_to_the_open_string:
      (Fingerboard_position_name.acoustic_interval_to_the_open_string name)
;;

let fingerboard_highest_note =
  { Note.letter_name = E; symbol = Natural; octave_designation = 6 }
;;

let find_fingerboard_position_exn system fingerboard_position_name =
  System.find_fingerboard_position_exn
    system
    ~name:(Fingerboard_position_name.to_string fingerboard_position_name)
;;
