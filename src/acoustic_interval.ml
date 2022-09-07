open! Core

(** *)

type t =
  | Zero
  | Equal_division_of_the_octave of
      { divisor : int
      ; number_of_divisions : int
      }
  | Reduced_natural_ratio of Natural_ratio.Reduced.t
  | Cents of float
[@@deriving sexp_of]

let log2 = Caml.Float.log2

let to_cents = function
  | Zero -> 0.
  | Equal_division_of_the_octave { divisor; number_of_divisions } ->
    1200. *. float_of_int number_of_divisions /. float_of_int divisor
  | Reduced_natural_ratio r ->
    let { Natural_ratio.numerator; denominator } =
      Natural_ratio.Reduced.to_natural_ratio r
    in
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
  | Reduced_natural_ratio r1, Reduced_natural_ratio r2 ->
    Reduced_natural_ratio (Natural_ratio.Reduced.multiply r1 r2)
  | Cents x, Cents y -> Cents (x +. y)
  | _ -> Cents (to_cents t1 +. to_cents t2)
;;

let compound ts = List.reduce ts ~f:add |> Option.value ~default:Zero

module Symbolic = struct
  type t =
    | Equal_tempered_12 of Interval.t
    | Pythagorean of Interval.t
    | Reduced_natural_ratio of Natural_ratio.Reduced.t
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
end

let pythagorean_ton =
  Natural_ratio.Reduced.(
    compound [ create_exn ~prime:3 ~exponent:2; create_exn ~prime:2 ~exponent:(-3) ])
;;

let fourth =
  Natural_ratio.Reduced.(
    compound [ create_exn ~prime:2 ~exponent:2; create_exn ~prime:3 ~exponent:(-1) ])
;;

let pythagorean_diatonic_semiton =
  let p2ton = Natural_ratio.Reduced.multiply pythagorean_ton pythagorean_ton in
  Natural_ratio.Reduced.divide fourth p2ton
;;

let pythagorean_chromatic_semiton =
  let p3ton =
    Natural_ratio.Reduced.multiply pythagorean_ton pythagorean_ton
    |> Natural_ratio.Reduced.multiply pythagorean_ton
  in
  Natural_ratio.Reduced.divide p3ton fourth
;;

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
            [ List.init
                interval.additional_octaves
                ~f:
                  (const
                     (Symbolic.Reduced_natural_ratio
                        (Natural_ratio.Reduced.create_exn ~prime:2 ~exponent:1)))
            ; List.init
                (min chromatic diatonic)
                ~f:(const (Symbolic.Reduced_natural_ratio pythagorean_ton))
            ; List.init
                (max 0 (chromatic - diatonic))
                ~f:(const (Symbolic.Reduced_natural_ratio pythagorean_chromatic_semiton))
            ; List.init
                (max 0 (diatonic - chromatic))
                ~f:(const (Symbolic.Reduced_natural_ratio pythagorean_diatonic_semiton))
            ]))
  | Reduced_natural_ratio nr -> Reduced_natural_ratio nr
  | Equal_division_of_the_octave { divisor; number_of_divisions } ->
    Equal_division_of_the_octave { divisor; number_of_divisions }
  | Just_diatonic_semiton ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound
          [ create_exn ~prime:2 ~exponent:4
          ; create_exn ~prime:3 ~exponent:(-1)
          ; create_exn ~prime:5 ~exponent:(-1)
          ])
  | Just_minor_ton ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound
          [ create_exn ~prime:2 ~exponent:1
          ; create_exn ~prime:3 ~exponent:(-2)
          ; create_exn ~prime:5 ~exponent:1
          ])
  | Just_major_ton ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound [ create_exn ~prime:2 ~exponent:(-3); create_exn ~prime:3 ~exponent:2 ])
  | Just_minor_third ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound
          [ create_exn ~prime:2 ~exponent:1
          ; create_exn ~prime:3 ~exponent:1
          ; create_exn ~prime:5 ~exponent:(-1)
          ])
  | Just_major_third ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound [ create_exn ~prime:2 ~exponent:(-2); create_exn ~prime:5 ~exponent:1 ])
  | Just_minor_sixth ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound [ create_exn ~prime:2 ~exponent:3; create_exn ~prime:5 ~exponent:(-1) ])
  | Just_major_sixth ->
    Reduced_natural_ratio
      Natural_ratio.Reduced.(
        compound [ create_exn ~prime:3 ~exponent:(-1); create_exn ~prime:5 ~exponent:1 ])
  | Compound ts -> List.fold ~init:Zero ts ~f:(fun acc s -> add acc (of_symbolic s))
;;

let unison = Zero
let octave = Reduced_natural_ratio (Natural_ratio.Reduced.create_exn ~prime:2 ~exponent:1)
let equal_tempered_12 interval = of_symbolic (Equal_tempered_12 interval)
let pythagorean interval = of_symbolic (Pythagorean interval)
let reduced_natural_ratio nr = of_symbolic (Reduced_natural_ratio nr)

let small_natural_ratio_exn ~numerator ~denominator =
  Natural_ratio.create_exn ~numerator ~denominator
  |> Natural_ratio.Reduced.of_small_natural_ratio_exn
  |> reduced_natural_ratio
;;

let equal_division_of_the_octave ~divisor ~number_of_divisions =
  of_symbolic (Equal_division_of_the_octave { divisor; number_of_divisions })
;;

let just_diatonic_semiton = of_symbolic Just_diatonic_semiton
let just_minor_ton = of_symbolic Just_minor_ton
let just_major_ton = of_symbolic Just_major_ton
let just_minor_third = of_symbolic Just_minor_third
let just_major_third = of_symbolic Just_major_third
let just_minor_sixth = of_symbolic Just_minor_sixth
let just_major_sixth = of_symbolic Just_major_sixth

let shift_up t frequency =
  let of_cents cents =
    Frequency.to_float frequency *. Caml.Float.exp2 (cents /. 1200.)
    |> Frequency.of_float_exn
  in
  let of_natural_ratio { Natural_ratio.numerator; denominator } =
    Frequency.to_float frequency *. Float.of_int numerator /. Float.of_int denominator
    |> Frequency.of_float_exn
  in
  match t with
  | Zero -> frequency
  | (Equal_division_of_the_octave _ | Cents _) as t -> t |> to_cents |> of_cents
  | Reduced_natural_ratio rn ->
    of_natural_ratio (Natural_ratio.Reduced.to_natural_ratio rn)
;;

let shift_down t frequency =
  let of_cents cents =
    Frequency.to_float frequency /. Caml.Float.exp2 (cents /. 1200.)
    |> Frequency.of_float_exn
  in
  let of_natural_ratio { Natural_ratio.numerator; denominator } =
    Frequency.to_float frequency *. Float.of_int denominator /. Float.of_int numerator
    |> Frequency.of_float_exn
  in
  match t with
  | Zero -> frequency
  | (Equal_division_of_the_octave _ | Cents _) as t -> t |> to_cents |> of_cents
  | Reduced_natural_ratio rn ->
    of_natural_ratio (Natural_ratio.Reduced.to_natural_ratio rn)
;;
