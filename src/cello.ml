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

  type t =
    [ `open_string
    | Edo12.t
    | Edo53.t
    ]
  [@@deriving compare, equal, hash, sexp_of]

  let sexp_of_t : t -> Sexp.t = function
    | `open_string -> Atom "0"
    | #Edo12.t as t -> [%sexp (t : Edo12.t)]
    | #Edo53.t as t -> [%sexp (t : Edo53.t)]
  ;;

  let acoustic_interval_to_the_open_string : t -> Acoustic_interval.t = function
    | `open_string -> Acoustic_interval.unison
    | #Edo12.t as t -> Edo12.acoustic_interval_to_the_open_string t
    | #Edo53.t as t -> Edo53.acoustic_interval_to_the_open_string t
  ;;

  let to_string t = Sexp.to_string [%sexp (t : t)]
end

let fingerboard_position name =
  Fingerboard_position.create_exn
    ~name:(Fingerboard_position_name.to_string name)
    ~acoustic_interval_to_the_open_string:
      (Fingerboard_position_name.acoustic_interval_to_the_open_string name)
;;
