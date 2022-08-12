open! Core
open! Cemper

let%expect_test "major and minor" =
  let major = force Scale.major in
  let minor = force Scale.minor in
  let tonics =
    let queue = Queue.create () in
    let rec aux name =
      if (not (Note.Letter_name.equal name A)) || Queue.is_empty queue
      then (
        Queue.enqueue queue { Note.letter_name = name; symbol = Natural };
        let succ = Note.Letter_name.succ name in
        Queue.enqueue queue { Note.letter_name = name; symbol = Sharp };
        Queue.enqueue queue { Note.letter_name = succ; symbol = Flat };
        aux succ)
    in
    aux Note.Letter_name.A;
    Queue.to_list queue
  in
  let str arr = Array.map arr ~f:Note.to_string in
  List.iter tonics ~f:(fun tonic ->
    print_s [%sexp (Scale.ascending major ~from:tonic |> str : string array)]);
  [%expect
    {|
    (A B C# D E F# G# A)
    (A# B# C## D# E# F## G## A#)
    (Bb C D Eb F G A Bb)
    (B C# D# E F# G# A# B)
    (B# C## D## E# F## G## A## B#)
    (Cb Db Eb Fb Gb Ab Bb Cb)
    (C D E F G A B C)
    (C# D# E# F# G# A# B# C#)
    (Db Eb F Gb Ab Bb C Db)
    (D E F# G A B C# D)
    (D# E# F## G# A# B# C## D#)
    (Eb F G Ab Bb C D Eb)
    (E F# G# A B C# D# E)
    (E# F## G## A# B# C## D## E#)
    (Fb Gb Ab Bbb Cb Db Eb Fb)
    (F G A Bb C D E F)
    (F# G# A# B C# D# E# F#)
    (Gb Ab Bb Cb Db Eb F Gb)
    (G A B C D E F# G)
    (G# A# B# C# D# E# F## G#)
    (Ab Bb C Db Eb F G Ab) |}];
  List.iter tonics ~f:(fun tonic ->
    print_s [%sexp (Scale.descending major ~from:tonic |> str : string array)]);
  [%expect
    {|
    (A G# F# E D C# B A)
    (A# G## F## E# D# C## B# A#)
    (Bb A G F Eb D C Bb)
    (B A# G# F# E D# C# B)
    (B# A## G## F## E# D## C## B#)
    (Cb Bb Ab Gb Fb Eb Db Cb)
    (C B A G F E D C)
    (C# B# A# G# F# E# D# C#)
    (Db C Bb Ab Gb F Eb Db)
    (D C# B A G F# E D)
    (D# C## B# A# G# F## E# D#)
    (Eb D C Bb Ab G F Eb)
    (E D# C# B A G# F# E)
    (E# D## C## B# A# G## F## E#)
    (Fb Eb Db Cb Bbb Ab Gb Fb)
    (F E D C Bb A G F)
    (F# E# D# C# B A# G# F#)
    (Gb F Eb Db Cb Bb Ab Gb)
    (G F# E D C B A G)
    (G# F## E# D# C# B# A# G#)
    (Ab G F Eb Db C Bb Ab) |}];
  List.iter tonics ~f:(fun tonic ->
    print_s [%sexp (Scale.ascending minor ~from:tonic |> str : string array)]);
  [%expect
    {|
    (A B C D E F G A)
    (A# B# C# D# E# F# G# A#)
    (Bb C Db Eb F Gb Ab Bb)
    (B C# D E F# G A B)
    (B# C## D# E# F## G# A# B#)
    (Cb Db Ebb Fb Gb Abb Bbb Cb)
    (C D Eb F G Ab Bb C)
    (C# D# E F# G# A B C#)
    (Db Eb Fb Gb Ab Bbb Cb Db)
    (D E F G A Bb C D)
    (D# E# F# G# A# B C# D#)
    (Eb F Gb Ab Bb Cb Db Eb)
    (E F# G A B C D E)
    (E# F## G# A# B# C# D# E#)
    (Fb Gb Abb Bbb Cb Dbb Ebb Fb)
    (F G Ab Bb C Db Eb F)
    (F# G# A B C# D E F#)
    (Gb Ab Bbb Cb Db Ebb Fb Gb)
    (G A Bb C D Eb F G)
    (G# A# B C# D# E F# G#)
    (Ab Bb Cb Db Eb Fb Gb Ab) |}];
  List.iter tonics ~f:(fun tonic ->
    print_s [%sexp (Scale.descending minor ~from:tonic |> str : string array)]);
  [%expect
    {|
    (A G F E D C B A)
    (A# G# F# E# D# C# B# A#)
    (Bb Ab Gb F Eb Db C Bb)
    (B A G F# E D C# B)
    (B# A# G# F## E# D# C## B#)
    (Cb Bbb Abb Gb Fb Ebb Db Cb)
    (C Bb Ab G F Eb D C)
    (C# B A G# F# E D# C#)
    (Db Cb Bbb Ab Gb Fb Eb Db)
    (D C Bb A G F E D)
    (D# C# B A# G# F# E# D#)
    (Eb Db Cb Bb Ab Gb F Eb)
    (E D C B A G F# E)
    (E# D# C# B# A# G# F## E#)
    (Fb Ebb Dbb Cb Bbb Abb Gb Fb)
    (F Eb Db C Bb Ab G F)
    (F# E D C# B A G# F#)
    (Gb Fb Ebb Db Cb Bbb Ab Gb)
    (G F Eb D C Bb A G)
    (G# F# E D# C# B A# G#)
    (Ab Gb Fb Eb Db Cb Bb Ab) |}];
  ()
;;
