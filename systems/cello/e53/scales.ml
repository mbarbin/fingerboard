open! Core
open! Cemper

let%expect_test "edo53 and octaves" =
  let t = force E53.t in
  let az =
    System.find_fingerboard_position_exn
      t
      ~name:(Cello.Fingerboard_position_name.to_string `P8z_e53)
  in
  let bz =
    System.find_fingerboard_position_exn
      t
      ~name:(Cello.Fingerboard_position_name.to_string `M2z_e53)
    |> Fingerboard_position.at_octave ~octave:1
  in
  let i =
    System.acoustic_interval
      t
      ~from:{ fingerboard_position = az; string_number = I }
      ~to_:{ fingerboard_position = bz; string_number = I }
    |> Option.value_exn ~here:[%here]
  in
  print_s [%sexp (i : Acoustic_interval.t)];
  [%expect {| (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)) |}];
  let cents_i = Acoustic_interval.to_cents i in
  let major_ton =
    Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:9
  in
  print_s [%sexp (major_ton : Acoustic_interval.t)];
  [%expect {| (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)) |}];
  let cents_major_ton = Acoustic_interval.to_cents major_ton in
  print_s [%sexp (cents_major_ton : float)];
  [%expect {| 203.77358490566036 |}];
  print_s [%sexp (Float.equal cents_i cents_major_ton : bool)];
  [%expect {| true |}];
  ()
;;

let fingerboard_high_bound =
  (* The selection of this particular note is somewhat arbitrary,
     anything around that note is realistic here. *)
  { Note.letter_name = E; symbol = Natural; octave_designation = 6 }
;;

let lower_c =
  let t = force E53.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let make_scale t ~characterized_scale ~from =
  let rec aux acc scale (located_note : Located_note.t) =
    if Option.is_some
         (Interval.compute ~from:fingerboard_high_bound ~to_:located_note.note ())
    then acc
    else (
      match scale with
      | [] -> aux acc characterized_scale located_note
      | hd :: tl ->
        (match System.find_next_located_note t located_note hd with
         | None -> acc
         | Some next_located_note -> aux (next_located_note :: acc) tl next_located_note))
  in
  aux [ from ] [] from |> List.rev
;;

let characterized_major_just_scale =
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

let c_just_scale =
  let t = force E53.t in
  make_scale t ~characterized_scale:characterized_major_just_scale ~from:lower_c
;;

let%expect_test "c_just_scale" =
  print_s [%sexp (c_just_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name C) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number IV))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number IV))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 17)))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number III))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8)))))
        (string_number III))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 17)))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number II))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8)))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number II))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30)))))
        (string_number II))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44)))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 8z-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 52)))))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44)))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 8z-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 52)))))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2z-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 8)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5z-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 30)))))
        (string_number I))))) |}];
  ()
;;

let characterized_major_pythagorean_scale =
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

let c_pythagorean_scale =
  let t = force E53.t in
  make_scale t ~characterized_scale:characterized_major_pythagorean_scale ~from:lower_c
;;

let%expect_test "c_pythagorean_scale" =
  print_s [%sexp (c_pythagorean_scale : Located_note.t list)];
  [%expect
    {|
    (((note ((letter_name C) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number IV))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number IV))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 18)))))
        (string_number IV))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number IV))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number III))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number III))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 2)))
      (fingerboard_location
       ((fingerboard_position
         ((name M3p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 18)))))
        (string_number III))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number III))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number II))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number II))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number II))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number II))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 3)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p-e53) (at_octave 0)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44)))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 4)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31)))))
        (string_number I))))
     ((note ((letter_name F) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m6p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 35)))))
        (string_number I))))
     ((note ((letter_name G) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name m7p-e53) (at_octave 1)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 44)))))
        (string_number I))))
     ((note ((letter_name A) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name 0) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string Zero)))
        (string_number I))))
     ((note ((letter_name B) (symbol Natural) (octave_designation 5)))
      (fingerboard_location
       ((fingerboard_position
         ((name M2p-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 9)))))
        (string_number I))))
     ((note ((letter_name C) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name m3p-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 13)))))
        (string_number I))))
     ((note ((letter_name D) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 4p-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 22)))))
        (string_number I))))
     ((note ((letter_name E) (symbol Natural) (octave_designation 6)))
      (fingerboard_location
       ((fingerboard_position
         ((name 5p-e53) (at_octave 2)
          (basis_acoustic_interval_to_the_open_string
           (Equal_division_of_the_octave (divisor 53) (number_of_divisions 31)))))
        (string_number I))))) |}];
  ()
;;
