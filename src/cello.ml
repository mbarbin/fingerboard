(**********************************************************************************)
(*  Fingerboard - a microtonal geography of the cello fingerboard                 *)
(*  Copyright (C) 2022-2024 Mathieu Barbin <mathieu.barbin@gmail.com>             *)
(*                                                                                *)
(*  This file is part of Fingerboard.                                             *)
(*                                                                                *)
(*  Fingerboard is free software: you can redistribute it and/or modify it under  *)
(*  the terms of the GNU Affero General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or any later version.   *)
(*                                                                                *)
(*  Fingerboard is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or         *)
(*  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License   *)
(*  for more details.                                                             *)
(*                                                                                *)
(*  You should have received a copy of the GNU Affero General Public License      *)
(*  along with Fingerboard. If not, see <https://www.gnu.org/licenses/>.          *)
(**********************************************************************************)

let position_name constructor_name =
  let name =
    String.map constructor_name ~f:(function
      | '_' -> '-'
      | c -> c)
  in
  Option.value (String.chop_prefix name ~prefix:"P") ~default:name
;;

module Fingerboard_position_name = struct
  module type S = sig
    type t [@@deriving compare, equal, enumerate]

    val to_dyn : t -> Dyn.t
    val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
  end

  module Make (M : sig
      type t [@@deriving compare, equal, enumerate]

      val constructor_name : t -> string
      val acoustic_interval_to_the_open_string : t -> Acoustic_interval.t
    end) : S with type t = M.t = struct
    include M

    let to_dyn t = Dyn.Variant (constructor_name t, [])
  end

  module Edo12 = struct
    module T = struct
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
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `m2e -> "m2e"
        | `M2e -> "M2e"
        | `m3e -> "m3e"
        | `M3e -> "M3e"
        | `P4e -> "P4e"
        | `A4e -> "A4e"
        | `P5e -> "P5e"
        | `m6e -> "m6e"
        | `M6e -> "M6e"
        | `m7e -> "m7e"
        | `M7e -> "M7e"
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

    include Make (T)
  end

  module Edo19 = struct
    module T = struct
      type t =
        [ `A1_e19
        | `m2_e19
        | `M2_e19
        | `A2_e19
        | `m3_e19
        | `M3_e19
        | `A3_e19
        | `P4_e19
        | `A4_e19
        | `d5_e19
        | `P5_e19
        | `A5_e19
        | `m6_e19
        | `M6_e19
        | `d7_e19
        | `m7_e19
        | `M7_e19
        | `d8_e19
        ]
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `A1_e19 -> "A1_e19"
        | `m2_e19 -> "m2_e19"
        | `M2_e19 -> "M2_e19"
        | `A2_e19 -> "A2_e19"
        | `m3_e19 -> "m3_e19"
        | `M3_e19 -> "M3_e19"
        | `A3_e19 -> "A3_e19"
        | `P4_e19 -> "P4_e19"
        | `A4_e19 -> "A4_e19"
        | `d5_e19 -> "d5_e19"
        | `P5_e19 -> "P5_e19"
        | `A5_e19 -> "A5_e19"
        | `m6_e19 -> "m6_e19"
        | `M6_e19 -> "M6_e19"
        | `d7_e19 -> "d7_e19"
        | `m7_e19 -> "m7_e19"
        | `M7_e19 -> "M7_e19"
        | `d8_e19 -> "d8_e19"
      ;;

      let acoustic_interval_to_the_open_string (t : t) =
        let number_of_divisions =
          match (t : t) with
          | `A1_e19 -> 1
          | `m2_e19 -> 2
          | `M2_e19 -> 3
          | `A2_e19 -> 4
          | `m3_e19 -> 5
          | `M3_e19 -> 6
          | `A3_e19 -> 7
          | `P4_e19 -> 8
          | `A4_e19 -> 9
          | `d5_e19 -> 10
          | `P5_e19 -> 11
          | `A5_e19 -> 12
          | `m6_e19 -> 13
          | `M6_e19 -> 14
          | `d7_e19 -> 15
          | `m7_e19 -> 16
          | `M7_e19 -> 17
          | `d8_e19 -> 18
        in
        Acoustic_interval.equal_division_of_the_octave ~divisor:19 ~number_of_divisions
      ;;
    end

    include Make (T)
  end

  module Edo31 = struct
    module T = struct
      type t =
        [ `A1_e31
        | `m2_e31
        | `M2_e31
        | `d3_e31
        | `A2_e31
        | `m3_e31
        | `M3_e31
        | `d4_e31
        | `A3_e31
        | `P4_e31
        | `A4_e31
        | `d5_e31
        | `P5_e31
        | `A5_e31
        | `m6_e31
        | `M6_e31
        | `d7_e31
        | `m7_e31
        | `M7_e31
        | `d8_e31
        ]
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `A1_e31 -> "A1_e31"
        | `m2_e31 -> "m2_e31"
        | `M2_e31 -> "M2_e31"
        | `d3_e31 -> "d3_e31"
        | `A2_e31 -> "A2_e31"
        | `m3_e31 -> "m3_e31"
        | `M3_e31 -> "M3_e31"
        | `d4_e31 -> "d4_e31"
        | `A3_e31 -> "A3_e31"
        | `P4_e31 -> "P4_e31"
        | `A4_e31 -> "A4_e31"
        | `d5_e31 -> "d5_e31"
        | `P5_e31 -> "P5_e31"
        | `A5_e31 -> "A5_e31"
        | `m6_e31 -> "m6_e31"
        | `M6_e31 -> "M6_e31"
        | `d7_e31 -> "d7_e31"
        | `m7_e31 -> "m7_e31"
        | `M7_e31 -> "M7_e31"
        | `d8_e31 -> "d8_e31"
      ;;

      let acoustic_interval_to_the_open_string (t : t) =
        let number_of_divisions =
          match (t : t) with
          | `A1_e31 -> 2
          | `m2_e31 -> 3
          | `M2_e31 -> 5
          | `d3_e31 -> 6
          | `A2_e31 -> 7
          | `m3_e31 -> 8
          | `M3_e31 -> 10
          | `d4_e31 -> 11
          | `A3_e31 -> 12
          | `P4_e31 -> 13
          | `A4_e31 -> 15
          | `d5_e31 -> 16
          | `P5_e31 -> 18
          | `A5_e31 -> 20
          | `m6_e31 -> 21
          | `M6_e31 -> 23
          | `d7_e31 -> 24
          | `m7_e31 -> 26
          | `M7_e31 -> 28
          | `d8_e31 -> 29
        in
        Acoustic_interval.equal_division_of_the_octave ~divisor:31 ~number_of_divisions
      ;;
    end

    include Make (T)
  end

  module Edo53 = struct
    module T = struct
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
        | `M6z_e53
        | `M6p_e53
        | `m7p_e53
        | `m7z_e53
        | `M7z_e53
        | `M7p_e53
        | `P8z_e53
        ]
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `A1z_e53 -> "A1z_e53"
        | `m2z_e53 -> "m2z_e53"
        | `M2z_e53 -> "M2z_e53"
        | `M2p_e53 -> "M2p_e53"
        | `m3p_e53 -> "m3p_e53"
        | `m3z_e53 -> "m3z_e53"
        | `M3z_e53 -> "M3z_e53"
        | `M3p_e53 -> "M3p_e53"
        | `P4p_e53 -> "P4p_e53"
        | `P4z_e53 -> "P4z_e53"
        | `A4z_e53 -> "A4z_e53"
        | `d5z_e53 -> "d5z_e53"
        | `P5z_e53 -> "P5z_e53"
        | `P5p_e53 -> "P5p_e53"
        | `m6p_e53 -> "m6p_e53"
        | `m6z_e53 -> "m6z_e53"
        | `M6z_e53 -> "M6z_e53"
        | `M6p_e53 -> "M6p_e53"
        | `m7p_e53 -> "m7p_e53"
        | `m7z_e53 -> "m7z_e53"
        | `M7z_e53 -> "M7z_e53"
        | `M7p_e53 -> "M7p_e53"
        | `P8z_e53 -> "P8z_e53"
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
          | `M6z_e53 -> 39
          | `M6p_e53 -> 40
          | `m7p_e53 -> 44
          | `m7z_e53 -> 45
          | `M7z_e53 -> 48
          | `M7p_e53 -> 49
          | `P8z_e53 -> 52
        in
        Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions
      ;;
    end

    include Make (T)
  end

  module Edo55 = struct
    module T = struct
      type t =
        [ `A1_e55
        | `m2_e55
        | `M2_e55
        | `d3_e55
        | `A2_e55
        | `m3_e55
        | `M3_e55
        | `d4_e55
        | `A3_e55
        | `P4_e55
        | `A4_e55
        | `d5_e55
        | `P5_e55
        | `A5_e55
        | `m6_e55
        | `M6_e55
        | `d7_e55
        | `m7_e55
        | `M7_e55
        | `d8_e55
        ]
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `A1_e55 -> "A1_e55"
        | `m2_e55 -> "m2_e55"
        | `M2_e55 -> "M2_e55"
        | `d3_e55 -> "d3_e55"
        | `A2_e55 -> "A2_e55"
        | `m3_e55 -> "m3_e55"
        | `M3_e55 -> "M3_e55"
        | `d4_e55 -> "d4_e55"
        | `A3_e55 -> "A3_e55"
        | `P4_e55 -> "P4_e55"
        | `A4_e55 -> "A4_e55"
        | `d5_e55 -> "d5_e55"
        | `P5_e55 -> "P5_e55"
        | `A5_e55 -> "A5_e55"
        | `m6_e55 -> "m6_e55"
        | `M6_e55 -> "M6_e55"
        | `d7_e55 -> "d7_e55"
        | `m7_e55 -> "m7_e55"
        | `M7_e55 -> "M7_e55"
        | `d8_e55 -> "d8_e55"
      ;;

      let acoustic_interval_to_the_open_string (t : t) =
        let number_of_divisions =
          match (t : t) with
          | `A1_e55 -> 4
          | `m2_e55 -> 5
          | `M2_e55 -> 9
          | `d3_e55 -> 10
          | `A2_e55 -> 13
          | `m3_e55 -> 14
          | `M3_e55 -> 18
          | `d4_e55 -> 19
          | `A3_e55 -> 22
          | `P4_e55 -> 23
          | `A4_e55 -> 27
          | `d5_e55 -> 28
          | `P5_e55 -> 32
          | `A5_e55 -> 36
          | `m6_e55 -> 37
          | `M6_e55 -> 41
          | `d7_e55 -> 42
          | `m7_e55 -> 46
          | `M7_e55 -> 50
          | `d8_e55 -> 51
        in
        Acoustic_interval.equal_division_of_the_octave ~divisor:55 ~number_of_divisions
      ;;
    end

    include Make (T)
  end

  module Pythagorean = struct
    module T = struct
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
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `m2p -> "m2p"
        | `A1p -> "A1p"
        | `d3p -> "d3p"
        | `M2p -> "M2p"
        | `m3p -> "m3p"
        | `A2p -> "A2p"
        | `d4p -> "d4p"
        | `M3p -> "M3p"
        | `P4p -> "P4p"
        | `A3p -> "A3p"
        | `d5p -> "d5p"
        | `A4p -> "A4p"
        | `d6p -> "d6p"
        | `P5p -> "P5p"
        | `m6p -> "m6p"
        | `A5p -> "A5p"
        | `d7p -> "d7p"
        | `M6p -> "M6p"
        | `m7p -> "m7p"
        | `A6p -> "A6p"
        | `d8p -> "d8p"
        | `M7p -> "M7p"
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

    include Make (T)
  end

  module Just = struct
    module T = struct
      type t =
        [ `A1z
        | `m2z
        | `M2z
        | `d3z
        | `m3z
        | `M3z
        | `d4z
        | `P4z
        | `A4z
        | `d5z
        | `P5z
        | `A5z
        | `m6z
        | `M6z
        | `d7z
        | `M7z
        | `d8z
        | `P8z
        ]
      [@@deriving compare, equal, enumerate]

      let constructor_name = function
        | `A1z -> "A1z"
        | `m2z -> "m2z"
        | `M2z -> "M2z"
        | `d3z -> "d3z"
        | `m3z -> "m3z"
        | `M3z -> "M3z"
        | `d4z -> "d4z"
        | `P4z -> "P4z"
        | `A4z -> "A4z"
        | `d5z -> "d5z"
        | `P5z -> "P5z"
        | `A5z -> "A5z"
        | `m6z -> "m6z"
        | `M6z -> "M6z"
        | `d7z -> "d7z"
        | `M7z -> "M7z"
        | `d8z -> "d8z"
        | `P8z -> "P8z"
      ;;

      let acoustic_interval_to_the_open_string (t : t) =
        match (t : t) with
        | `A1z ->
          Acoustic_interval.(remove just_major_ton just_diatonic_semiton) |> Option.get
        | `m2z -> Acoustic_interval.just_diatonic_semiton
        | `M2z -> Acoustic_interval.just_minor_ton
        | `d3z ->
          Acoustic_interval.(
            add
              (pythagorean { number = Second; quality = Minor; additional_octaves = 0 })
              just_diatonic_semiton)
        | `m3z -> Acoustic_interval.just_minor_third
        | `M3z -> Acoustic_interval.just_major_third
        | `d4z ->
          Acoustic_interval.(
            add
              (pythagorean { number = Third; quality = Minor; additional_octaves = 0 })
              just_diatonic_semiton)
        | `P4z ->
          Acoustic_interval.(
            remove
              (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
              just_minor_ton)
          |> Option.get
        | `A4z ->
          Acoustic_interval.(
            remove
              (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
              just_diatonic_semiton)
          |> Option.get
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
        | `A5z ->
          Acoustic_interval.(
            remove
              (pythagorean { number = Sixth; quality = Perfect; additional_octaves = 0 })
              just_diatonic_semiton)
          |> Option.get
        | `m6z -> Acoustic_interval.just_minor_sixth
        | `M6z -> Acoustic_interval.just_major_sixth
        | `d7z ->
          Acoustic_interval.(
            add
              (pythagorean { number = Sixth; quality = Minor; additional_octaves = 0 })
              just_diatonic_semiton)
        | `M7z ->
          Acoustic_interval.(
            add
              (pythagorean { number = Fifth; quality = Perfect; additional_octaves = 0 })
              just_major_third)
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

    include Make (T)
  end

  module T = struct
    type t =
      [ `open_string
      | Edo12.t
      | Edo19.t
      | Edo31.t
      | Edo53.t
      | Edo55.t
      | Pythagorean.t
      | Just.t
      ]
    [@@deriving compare, equal, enumerate]

    let constructor_name = function
      | `open_string -> "open_string"
      | #Edo12.t as t -> Edo12.T.constructor_name t
      | #Edo19.t as t -> Edo19.T.constructor_name t
      | #Edo31.t as t -> Edo31.T.constructor_name t
      | #Edo53.t as t -> Edo53.T.constructor_name t
      | #Edo55.t as t -> Edo55.T.constructor_name t
      | #Pythagorean.t as t -> Pythagorean.T.constructor_name t
      | #Just.t as t -> Just.T.constructor_name t
    ;;

    let acoustic_interval_to_the_open_string : t -> Acoustic_interval.t = function
      | `open_string -> Acoustic_interval.unison
      | #Edo12.t as t -> t |> Edo12.acoustic_interval_to_the_open_string
      | #Edo19.t as t -> t |> Edo19.acoustic_interval_to_the_open_string
      | #Edo31.t as t -> t |> Edo31.acoustic_interval_to_the_open_string
      | #Edo53.t as t -> t |> Edo53.acoustic_interval_to_the_open_string
      | #Edo55.t as t -> t |> Edo55.acoustic_interval_to_the_open_string
      | #Pythagorean.t as t -> t |> Pythagorean.acoustic_interval_to_the_open_string
      | #Just.t as t -> t |> Just.acoustic_interval_to_the_open_string
    ;;
  end

  include Make (T)

  let to_string t =
    match t with
    | `open_string -> "0"
    | _ -> position_name (T.constructor_name t)
  ;;
end

let a_string = { Note.letter_name = A; symbol = Natural; octave_designation = 3 }

let a_string_frequency_220 =
  Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave
;;

let fingerboard_position name =
  Fingerboard_position.create_exn
    ~name:(Fingerboard_position_name.to_string name)
    ~acoustic_interval_to_the_open_string:
      (Fingerboard_position_name.acoustic_interval_to_the_open_string name)
;;

let add_fingerboard_position_exn ?on_n_octaves system fingerboard_position_name =
  System.add_fingerboard_position_exn
    ?on_n_octaves
    system
    (fingerboard_position fingerboard_position_name)
;;

let fifth_system ?acoustic_interval () =
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    let acoustic_interval =
      match acoustic_interval with
      | Some i -> i
      | None -> Acoustic_interval.pythagorean fifth
    in
    Array.create
      ~len:3
      (Characterized_interval.create_exn ~interval:fifth ~acoustic_interval)
  in
  let system =
    System.create
      ~high_vibrating_string:a_string
      ~pitch:a_string_frequency_220
      ~intervals_going_down
  in
  add_fingerboard_position_exn system `open_string;
  system
;;

let fingerboard_highest_note =
  { Note.letter_name = E; symbol = Natural; octave_designation = 6 }
;;

let find_fingerboard_position_exn system fingerboard_position_name =
  System.find_fingerboard_position_exn
    system
    ~name:(Fingerboard_position_name.to_string fingerboard_position_name)
;;
