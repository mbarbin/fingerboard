open! Base
open! Stdio
open! Fingerboard

let test_pitch_exn ~system ~(intervals_going_down : Characterized_interval.t array) =
  (* We test that the pitch is the same for the open string and the position on
     the next vibrating string at the position of the interval between the two
     vibrating strings. *)
  for i = 1 to Array.length intervals_going_down do
    let high = Roman_numeral.of_int_exn i in
    let low = Roman_numeral.succ_exn high in
    let open_string =
      System.pitch
        system
        { fingerboard_position = Fingerboard_position.open_string; string_number = high }
    in
    let unison =
      System.pitch
        system
        { fingerboard_position =
            Fingerboard_position.create_exn
              ~name:"unison"
              ~acoustic_interval_to_the_open_string:
                intervals_going_down.(i - 1).acoustic_interval
        ; string_number = low
        }
    in
    if not (Frequency.equal open_string unison)
    then
      raise_s
        [%sexp
          "Unexpected pitch calculation"
          , [%here]
          , { high : Roman_numeral.t
            ; low : Roman_numeral.t
            ; open_string : Frequency.t
            ; unison : Frequency.t
            }]
  done
;;

let%expect_test "4-strings cello" =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create
      ~len:3
      (Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:(Acoustic_interval.pythagorean fifth))
  in
  let system = System.create ~high_vibrating_string:a ~pitch ~intervals_going_down in
  test_pitch_exn ~system ~intervals_going_down;
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
        (pitch 220) (roman_numeral I))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral II))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.777777777777771) (roman_numeral III))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.185185185185176) (roman_numeral IV))))
     (intervals_going_down
      (((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))) |}];
  (* Creating a few positions and check the intervals between them. *)
  let fourth =
    Fingerboard_position.create_exn
      ~name:"4th"
      ~acoustic_interval_to_the_open_string:
        (Acoustic_interval.pythagorean
           { number = Fourth; quality = Perfect; additional_octaves = 0 })
  in
  let just_minor_ton =
    Fingerboard_position.create_exn
      ~name:"2MZ"
      ~acoustic_interval_to_the_open_string:Acoustic_interval.just_minor_ton
  in
  let pythagorean_minor_third =
    Fingerboard_position.create_exn
      ~name:"2mP"
      ~acoustic_interval_to_the_open_string:
        (Acoustic_interval.pythagorean
           { number = Third; quality = Minor; additional_octaves = 0 })
  in
  let i =
    System.acoustic_interval
      system
      ~from:{ fingerboard_position = fourth; string_number = III }
      ~to_:{ fingerboard_position = pythagorean_minor_third; string_number = I }
    |> Option.value_exn ~here:[%here]
  in
  assert (Acoustic_interval.equal i Acoustic_interval.octave);
  print_string (Acoustic_interval.to_string i);
  [%expect {| 2 |}];
  let i =
    System.acoustic_interval
      system
      ~from:{ fingerboard_position = fourth; string_number = III }
      ~to_:{ fingerboard_position = just_minor_ton; string_number = II }
    |> Option.value_exn ~here:[%here]
  in
  assert (Acoustic_interval.equal i Acoustic_interval.just_major_third);
  print_string (Acoustic_interval.to_string i);
  [%expect {| 5 / 2^2 |}];
  let i =
    System.acoustic_interval
      system
      ~from:{ fingerboard_position = fourth; string_number = III }
      ~to_:{ fingerboard_position = pythagorean_minor_third; string_number = II }
    |> Option.value_exn ~here:[%here]
  in
  assert (
    Acoustic_interval.equal
      i
      (Acoustic_interval.pythagorean
         { number = Fourth; quality = Perfect; additional_octaves = 0 }));
  print_string (Acoustic_interval.to_string i);
  [%expect {| 2^2 / 3 |}];
  let i =
    System.acoustic_interval
      system
      ~from:
        { fingerboard_position = Fingerboard_position.open_string; string_number = II }
      ~to_:{ fingerboard_position = just_minor_ton; string_number = I }
    |> Option.value_exn ~here:[%here]
  in
  assert (Acoustic_interval.equal i Acoustic_interval.just_major_sixth);
  print_string (Acoustic_interval.to_string i);
  [%expect {| 5 / 3 |}];
  let i =
    System.acoustic_interval
      system
      ~from:{ fingerboard_position = just_minor_ton; string_number = I }
      ~to_:{ fingerboard_position = pythagorean_minor_third; string_number = I }
    |> Option.value_exn ~here:[%here]
  in
  assert (Acoustic_interval.equal i Acoustic_interval.just_diatonic_semiton);
  print_string (Acoustic_interval.to_string i);
  [%expect {| 2^4 / (3 * 5) |}];
  ()
;;

let%expect_test "piccolo cello" =
  let e = { Note.letter_name = E; symbol = Natural; octave_designation = 4 } in
  let pitch =
    Frequency.a4_440
    |> Acoustic_interval.shift_down
         (Acoustic_interval.pythagorean
            { number = Fourth; quality = Perfect; additional_octaves = 0 })
  in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create
      ~len:4
      (Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:(Acoustic_interval.pythagorean fifth))
  in
  let system = System.create ~high_vibrating_string:e ~pitch ~intervals_going_down in
  test_pitch_exn ~system ~intervals_going_down;
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name E) (symbol Natural) (octave_designation 4)))
        (pitch 330) (roman_numeral I))
       ((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
        (pitch 220) (roman_numeral II))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral III))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.777777777777771) (roman_numeral IV))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.185185185185176) (roman_numeral V))))
     (intervals_going_down
      (((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))) |}];
  ()
;;

let%expect_test "5th Bach's suite for cello" =
  let g = { Note.letter_name = G; symbol = Natural; octave_designation = 3 } in
  let pitch =
    Frequency.a4_440
    |> Acoustic_interval.shift_down
         (Acoustic_interval.pythagorean
            { number = Second; quality = Major; additional_octaves = 1 })
  in
  let intervals_going_down =
    let fourth =
      { Interval.number = Fourth; quality = Perfect; additional_octaves = 0 }
    in
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.concat
      [ [| Characterized_interval.create_exn
             ~interval:fourth
             ~acoustic_interval:(Acoustic_interval.pythagorean fourth)
        |]
      ; Array.create
          ~len:2
          (Characterized_interval.create_exn
             ~interval:fifth
             ~acoustic_interval:(Acoustic_interval.pythagorean fifth))
      ]
  in
  let system = System.create ~high_vibrating_string:g ~pitch ~intervals_going_down in
  test_pitch_exn ~system ~intervals_going_down;
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name G) (symbol Natural) (octave_designation 3)))
        (pitch 195.55555555555554) (roman_numeral I))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral II))
       ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
        (pitch 97.777777777777771) (roman_numeral III))
       ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
        (pitch 65.185185185185176) (roman_numeral IV))))
     (intervals_going_down
      (((interval ((number Fourth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent 2)) ((prime 3) (exponent -1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))) |}];
  ()
;;

let%expect_test "Kodaly sonata for cello solo" =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    [| Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:(Acoustic_interval.pythagorean fifth)
     ; Characterized_interval.create_exn
         ~interval:{ Interval.number = Sixth; quality = Minor; additional_octaves = 0 }
         ~acoustic_interval:Acoustic_interval.just_minor_sixth
     ; Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:(Acoustic_interval.pythagorean fifth)
    |]
  in
  let system = System.create ~high_vibrating_string:a ~pitch ~intervals_going_down in
  test_pitch_exn ~system ~intervals_going_down;
  print_s [%sexp (system : System.t)];
  [%expect
    {|
    ((vibrating_strings
      (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
        (pitch 220) (roman_numeral I))
       ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
        (pitch 146.66666666666666) (roman_numeral II))
       ((open_string ((letter_name F) (symbol Sharp) (octave_designation 2)))
        (pitch 91.666666666666657) (roman_numeral III))
       ((open_string ((letter_name B) (symbol Natural) (octave_designation 1)))
        (pitch 61.111111111111107) (roman_numeral IV))))
     (intervals_going_down
      (((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))
       ((interval ((number Sixth) (quality Minor) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent 3)) ((prime 5) (exponent -1))))))
       ((interval ((number Fifth) (quality Perfect) (additional_octaves 0)))
        (acoustic_interval
         (Reduced_natural_ratio
          (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))) |}];
  ()
;;

let%expect_test "reset-pitch" =
  let a = { Note.letter_name = A; symbol = Natural; octave_designation = 3 } in
  let pitch = Frequency.a4_440 |> Acoustic_interval.shift_down Acoustic_interval.octave in
  let intervals_going_down =
    let fifth = { Interval.number = Fifth; quality = Perfect; additional_octaves = 0 } in
    Array.create
      ~len:3
      (Characterized_interval.create_exn
         ~interval:fifth
         ~acoustic_interval:(Acoustic_interval.pythagorean fifth))
  in
  let system = System.create ~high_vibrating_string:a ~pitch ~intervals_going_down in
  test_pitch_exn ~system ~intervals_going_down;
  let sexp1 = [%sexp (system : System.t)] in
  let change ~f =
    f ();
    let sexp2 = [%sexp (system : System.t)] in
    let diff = Sexp_diff.Algo.diff ~original:sexp1 ~updated:sexp2 () in
    print_string
      (Sexp_diff.Display.display_as_plain_string
         (Sexp_diff.Display.Display_options.create ~num_shown:2 Two_column)
         diff)
  in
  change ~f:(fun () -> System.reset_pitch system (Roman_numeral.of_int_exn 1) ~pitch);
  [%expect {| (no changes) |}];
  change ~f:(fun () ->
    System.reset_pitch
      system
      (Roman_numeral.of_int_exn 1)
      ~pitch:
        (Frequency.of_float_exn 442.
         |> Acoustic_interval.shift_down Acoustic_interval.octave));
  [%expect
    {|
     ((vibrating_strings                                                           ((vibrating_strings
       (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))     (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
         (pitch                                                                        (pitch
    -     220                                                                     +     221
         )                                                                             )
         (roman_numeral I))                                                            (roman_numeral I))
        ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))      ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
         (pitch                                                                        (pitch
    -     146.66666666666666                                                      +     147.33333333333334
         )                                                                             )
         (roman_numeral II))                                                           (roman_numeral II))
        ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))      ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
         (pitch                                                                        (pitch
    -     97.777777777777771                                                      +     98.222222222222229
         )                                                                             )
         (roman_numeral III))                                                          (roman_numeral III))
        ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))      ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
         (pitch                                                                        (pitch
    -     65.185185185185176                                                      +     65.481481481481481
         )                                                                             )
         (roman_numeral IV))))                                                         (roman_numeral IV))))
                                                                    ...11 unchanged lines...
          (Reduced_natural_ratio                                                        (Reduced_natural_ratio
           (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))))                    (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))) |}];
  change ~f:(fun () ->
    System.reset_pitch
      system
      (Roman_numeral.of_int_exn 2)
      ~pitch:(Frequency.of_float_exn 147.));
  [%expect
    {|
     ((vibrating_strings                                                           ((vibrating_strings
       (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))     (((open_string ((letter_name A) (symbol Natural) (octave_designation 3)))
         (pitch                                                                        (pitch
    -     220                                                                     +     220.5
         )                                                                             )
         (roman_numeral I))                                                            (roman_numeral I))
        ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))      ((open_string ((letter_name D) (symbol Natural) (octave_designation 3)))
         (pitch                                                                        (pitch
    -     146.66666666666666                                                      +     147
         )                                                                             )
         (roman_numeral II))                                                           (roman_numeral II))
        ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))      ((open_string ((letter_name G) (symbol Natural) (octave_designation 2)))
         (pitch                                                                        (pitch
    -     97.777777777777771                                                      +     98
         )                                                                             )
         (roman_numeral III))                                                          (roman_numeral III))
        ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))      ((open_string ((letter_name C) (symbol Natural) (octave_designation 2)))
         (pitch                                                                        (pitch
    -     65.185185185185176                                                      +     65.333333333333329
         )                                                                             )
         (roman_numeral IV))))                                                         (roman_numeral IV))))
                                                                    ...11 unchanged lines...
          (Reduced_natural_ratio                                                        (Reduced_natural_ratio
           (((prime 2) (exponent -1)) ((prime 3) (exponent 1)))))))))                    (((prime 2) (exponent -1)) ((prime 3) (exponent 1))))))))) |}];
  ()
;;
