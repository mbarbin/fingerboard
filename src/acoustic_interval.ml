open! Core

(** *)

(* Natural ratio: maybe worth manipulating in its reduced form ? *)

type t =
  | Zero
  | Equal_division_of_the_octave of
      { divisor : int
      ; number_of_divisions : int
      }
  | Natural_ratio of
      { numerator : int
      ; denominator : int
      }
  | Cents of float

let log2 = Caml.Float.log2

let cents = function
  | Zero -> 0.
  | Equal_division_of_the_octave { divisor; number_of_divisions } ->
    1200. *. float_of_int number_of_divisions /. float_of_int divisor
  | Natural_ratio { numerator; denominator } ->
    log2 (float_of_int numerator /. float_of_int denominator) *. 1200.
  | Cents x -> x
;;

let of_cents x = Cents x

let add t1 t2 =
  match t1, t2 with
  | Zero, t | t, Zero -> t
  | ( Equal_division_of_the_octave { divisor = d1; number_of_divisions = p1 }
    , Equal_division_of_the_octave { divisor = d2; number_of_divisions = p2 } )
    when d1 = d2 ->
    Equal_division_of_the_octave { divisor = d1; number_of_divisions = p1 + p2 }
  | ( Natural_ratio { numerator = n1; denominator = d1 }
    , Natural_ratio { numerator = n2; denominator = d2 } ) ->
    Natural_ratio { numerator = n1 * n2; denominator = d1 * d2 }
  | Cents x, Cents y -> Cents (x +. y)
  | _ -> Cents (cents t1 +. cents t2)
;;

module Symbolic = struct
  type t =
    | Equal_tempered_12 of Interval.t
    | Pythagorean of Interval.t
    | Natural_ratio of
        { numerator : int
        ; denominator : int
        }
    | Equal_division_of_the_octave of
        { divisor : int
        ; number_of_divisions : int
        }
    | Just_diatonic_semiton
    | Just_minor_ton
    | Just_major_ton
    | Just_minor_third
    | Just_major_third
    | Just_minor_sixth
    | Just_major_sixth
    | Compound of t list

  let pythagorean_ton = Natural_ratio { numerator = 9; denominator = 8 }
  let pythagorean_diatonic_semiton = Natural_ratio { numerator = 256; denominator = 243 }

  let pythagorean_chromatic_semiton =
    Natural_ratio { numerator = 2187; denominator = 2048 }
  ;;
end

let ( /^ ) a b = Natural_ratio { numerator = a; denominator = b }

let rec of_symbolic (symbolic : Symbolic.t) =
  match symbolic with
  | Equal_tempered_12 interval ->
    Equal_division_of_the_octave
      { divisor = 12; number_of_divisions = Interval.number_of_semitons interval }
  | Pythagorean interval ->
    let accepts_minor_major_quality =
      Interval.Number.accepts_minor_major_quality interval.number
    in
    let chromatic, diatonic =
      let basis_interval =
        { Interval.number = interval.number
        ; quality = (if accepts_minor_major_quality then Major else Perfect)
        ; additional_octaves = 0
        }
      in
      let number_of_semitons = Interval.number_of_semitons basis_interval in
      let diatonic = Interval.Number.to_int basis_interval.number - 1 in
      let chromatic = number_of_semitons - diatonic in
      let chromatic_shift =
        match interval.quality with
        | Perfect -> 0
        | Major -> 0
        | Minor -> -1
        | Augmented -> 1
        | Diminished -> if accepts_minor_major_quality then -2 else -1
        | Doubly_augmented -> 2
        | Doubly_diminished -> if accepts_minor_major_quality then -3 else -2
      in
      let chromatic = chromatic + chromatic_shift in
      chromatic, diatonic
    in
    of_symbolic
      (Compound
         (List.concat
            [ List.init (min chromatic diatonic) ~f:(const Symbolic.pythagorean_ton)
            ; List.init
                (max 0 (chromatic - diatonic))
                ~f:(const Symbolic.pythagorean_chromatic_semiton)
            ; List.init
                (max 0 (diatonic - chromatic))
                ~f:(const Symbolic.pythagorean_diatonic_semiton)
            ]))
  | Natural_ratio { numerator; denominator } -> Natural_ratio { numerator; denominator }
  | Equal_division_of_the_octave { divisor; number_of_divisions } ->
    Equal_division_of_the_octave { divisor; number_of_divisions }
  | Just_diatonic_semiton -> 16 /^ 15
  | Just_minor_ton -> 10 /^ 9
  | Just_major_ton -> 9 /^ 10
  | Just_minor_third -> 6 /^ 5
  | Just_major_third -> 5 /^ 4
  | Just_minor_sixth -> 8 /^ 5
  | Just_major_sixth -> 10 /^ 6
  | Compound ts -> List.fold ~init:Zero ts ~f:(fun acc s -> add acc (of_symbolic s))
;;
