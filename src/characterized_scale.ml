type t = Characterized_interval.t list

let major_just =
  let second quality acoustic_interval =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn ~interval ~acoustic_interval
  in
  let minor_ton = second Major Acoustic_interval.just_minor_ton in
  let major_ton = second Major Acoustic_interval.just_major_ton in
  let semiton = second Minor Acoustic_interval.just_diatonic_semiton in
  [ major_ton; minor_ton; semiton; major_ton; minor_ton; major_ton; semiton ]
;;

let major_pythagorean =
  let second quality =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.pythagorean interval)
  in
  let ton = second Major in
  let semiton = second Minor in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_just_e53 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions)
  in
  let minor_ton = second Major 8 in
  let major_ton = second Major 9 in
  let semiton = second Minor 5 in
  [ major_ton; minor_ton; semiton; major_ton; minor_ton; major_ton; semiton ]
;;

let major_pythagorean_e53 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions)
  in
  let ton = second Major 9 in
  let semiton = second Minor 4 in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_e12 =
  let second quality =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:(Acoustic_interval.equal_tempered_12 interval)
  in
  let ton = second Major in
  let semiton = second Minor in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;

let major_e55 =
  let second quality number_of_divisions =
    let interval = { Interval.number = Second; quality; additional_octaves = 0 } in
    Characterized_interval.create_exn
      ~interval
      ~acoustic_interval:
        (Acoustic_interval.equal_division_of_the_octave ~divisor:55 ~number_of_divisions)
  in
  let ton = second Major 9 in
  let semiton = second Minor 5 in
  [ ton; ton; semiton; ton; ton; ton; semiton ]
;;
