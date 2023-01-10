open! Core
open! Fingerboard

let adjustment =
  { System.Double_stops.Adjustment.from =
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:40
  ; to_ =
      Acoustic_interval.equal_division_of_the_octave ~divisor:53 ~number_of_divisions:39
  }
;;

let make_scale ~characterized_scale ~from ~adjustment =
  let t = force E53.t in
  System.Double_stops.make_scale
    t
    ?adjustment
    ~characterized_scale
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_pythagorean_scale ~from =
  make_scale
    ~characterized_scale:Characterized_scale.major_pythagorean_e53
    ~from
    ~adjustment:None
;;

let make_major_just_scale ~from =
  make_scale
    ~characterized_scale:Characterized_scale.major_just_e53
    ~from
    ~adjustment:(Some adjustment)
;;

let%expect_test "c_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ C2  │ IV     │ 0         │ 0     │ A2   │ III    │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ D2  │ IV     │ M2p-e53   │ 204   │ B2   │ III    │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ E2  │ IV     │ M3z-e53   │ 385   │ C3   │ III    │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ F2  │ IV     │ 4z-e53    │ 521   │ D3   │ III    │ 5p-e53    │ 702   │ M6 - 39-53edo │ 883   │
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ A2  │ III    │ M2z-e53   │ 181   │ F3   │ II     │ m3p-e53   │ 294   │ m6 - 36-53edo │ 815   │
    │ B2  │ III    │ M3z-e53   │ 385   │ G3   │ II     │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ C3  │ III    │ 4p-e53    │ 498   │ A3   │ II     │ 5z-e53    │ 679   │ M6 - 39-53edo │ 883   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ E3  │ II     │ M2z-e53   │ 181   │ C4   │ I      │ m3p-e53   │ 294   │ m6 - 36-53edo │ 815   │
    │ F3  │ II     │ m3z-e53   │ 317   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5z-e53    │ 679   │ M6 - 39-53edo │ 883   │
    │ A3  │ II     │ 5z-e53    │ 679   │ F4   │ I      │ m6p-e53   │ 792   │ m6 - 36-53edo │ 815   │
    │ B3  │ II     │ M6z-e53   │ 883   │ G4   │ I      │ m7p-e53   │ 996   │ m6 - 36-53edo │ 815   │
    │ C4  │ II     │ m7p-e53   │ 996   │ A4   │ I      │ 8z-e53    │ 1177  │ M6 - 39-53edo │ 883   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M6 - 39-53edo │ 883   │
    │ E4  │ II     │ M2z-e53-1 │ 1381  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m6 - 36-53edo │ 815   │
    │ F4  │ II     │ m3z-e53-1 │ 1517  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 39-53edo │ 883   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5z-e53-1  │ 1879  │ M6 - 39-53edo │ 883   │
    │ A4  │ II     │ 5z-e53-1  │ 1879  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m6 - 36-53edo │ 815   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m6 - 36-53edo │ 815   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ A5   │ I      │ 8z-e53-1  │ 2377  │ M6 - 39-53edo │ 883   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M6 - 39-53edo │ 883   │
    │ E5  │ II     │ M2z-e53-2 │ 2581  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m6 - 36-53edo │ 815   │
    │ F5  │ II     │ m3z-e53-2 │ 2717  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 39-53edo │ 883   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5z-e53-2  │ 3079  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ C2  │ IV     │ 0         │ 0     │ A2   │ III    │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ D2  │ IV     │ M2p-e53   │ 204   │ B2   │ III    │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ E2  │ IV     │ M3p-e53   │ 408   │ C3   │ III    │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ D3   │ III    │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F3   │ II     │ m3p-e53   │ 294   │ m6 - 35-53edo │ 792   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G3   │ II     │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ C3  │ III    │ 4p-e53    │ 498   │ A3   │ II     │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C4   │ I      │ m3p-e53   │ 294   │ m6 - 35-53edo │ 792   │
    │ F3  │ II     │ m3p-e53   │ 294   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F4   │ I      │ m6p-e53   │ 792   │ m6 - 35-53edo │ 792   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G4   │ I      │ m7p-e53   │ 996   │ m6 - 35-53edo │ 792   │
    │ C4  │ II     │ m7p-e53   │ 996   │ A4   │ I      │ 0-1       │ 1200  │ M6 - 40-53edo │ 906   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M6 - 40-53edo │ 906   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m6 - 35-53edo │ 792   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 40-53edo │ 906   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M6 - 40-53edo │ 906   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m6 - 35-53edo │ 792   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m6 - 35-53edo │ 792   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ A5   │ I      │ 0-2       │ 2400  │ M6 - 40-53edo │ 906   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M6 - 40-53edo │ 906   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m6 - 35-53edo │ 792   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 40-53edo │ 906   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_sharp_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_cp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ C#2 │ IV     │ m2z-e53   │ 113   │ A#2  │ III    │ m3z-e53   │ 317   │ M6 - 40-53edo │ 906   │
    │ D#2 │ IV     │ m3z-e53   │ 317   │ B#2  │ III    │ 4z-e53    │ 521   │ M6 - 40-53edo │ 906   │
    │ E#2 │ IV     │ 4z-e53    │ 521   │ C#3  │ III    │ d5z-e53   │ 611   │ m6 - 35-53edo │ 792   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ D#3  │ III    │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ G#2 │ III    │ m2z-e53   │ 113   │ E#3  │ II     │ m3z-e53   │ 317   │ M6 - 40-53edo │ 906   │
    │ A#2 │ III    │ m3z-e53   │ 317   │ F#3  │ II     │ M3p-e53   │ 408   │ m6 - 35-53edo │ 792   │
    │ B#2 │ III    │ 4z-e53    │ 521   │ G#3  │ II     │ d5z-e53   │ 611   │ m6 - 35-53edo │ 792   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A#3  │ II     │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ D#3 │ II     │ m2z-e53   │ 113   │ B#3  │ I      │ m3z-e53   │ 317   │ M6 - 40-53edo │ 906   │
    │ E#3 │ II     │ m3z-e53   │ 317   │ C#4  │ I      │ M3p-e53   │ 408   │ m6 - 35-53edo │ 792   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ E#4  │ I      │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ A#3 │ II     │ m6z-e53   │ 815   │ F#4  │ I      │ M6p-e53   │ 906   │ m6 - 35-53edo │ 792   │
    │ B#3 │ II     │ m7z-e53   │ 1019  │ G#4  │ I      │ M7p-e53   │ 1109  │ m6 - 35-53edo │ 792   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A#4  │ I      │ m2z-e53-1 │ 1313  │ M6 - 40-53edo │ 906   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ B#4  │ I      │ m3z-e53-1 │ 1517  │ M6 - 40-53edo │ 906   │
    │ E#4 │ II     │ m3z-e53-1 │ 1517  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m6 - 35-53edo │ 792   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M6 - 40-53edo │ 906   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ E#5  │ I      │ m6z-e53-1 │ 2015  │ M6 - 40-53edo │ 906   │
    │ A#4 │ II     │ m6z-e53-1 │ 2015  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 35-53edo │ 792   │
    │ B#4 │ II     │ m7z-e53-1 │ 2219  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ m6 - 35-53edo │ 792   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A#5  │ I      │ m2z-e53-2 │ 2513  │ M6 - 40-53edo │ 906   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ B#5  │ I      │ m3z-e53-2 │ 2717  │ M6 - 40-53edo │ 906   │
    │ E#5 │ II     │ m3z-e53-2 │ 2717  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m6 - 35-53edo │ 792   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M6 - 40-53edo │ 906   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ E#6  │ I      │ m6z-e53-2 │ 3215  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_dp_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Db2 │ IV     │ A1z-e53   │ 91    │ Bb2  │ III    │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ Eb2 │ IV     │ m3p-e53   │ 294   │ C3   │ III    │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ Db3  │ III    │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ Gb2 │ IV     │ A4z-e53   │ 589   │ Eb3  │ III    │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Ab2 │ III    │ A1z-e53   │ 91    │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ Gb3  │ II     │ M3z-e53   │ 385   │ m6 - 35-53edo │ 792   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Ab3  │ II     │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ Db3 │ III    │ A4z-e53   │ 589   │ Bb3  │ II     │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Eb3 │ II     │ A1z-e53   │ 91    │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ F3  │ II     │ m3p-e53   │ 294   │ Db4  │ I      │ M3z-e53   │ 385   │ m6 - 35-53edo │ 792   │
    │ Gb3 │ II     │ M3z-e53   │ 385   │ Eb4  │ I      │ A4z-e53   │ 589   │ M6 - 40-53edo │ 906   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Gb4  │ I      │ M6z-e53   │ 883   │ m6 - 35-53edo │ 792   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Ab4  │ I      │ M7z-e53   │ 1087  │ m6 - 35-53edo │ 792   │
    │ Db4 │ II     │ M7z-e53   │ 1087  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 40-53edo │ 906   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 40-53edo │ 906   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Db5  │ I      │ M3z-e53-1 │ 1585  │ m6 - 35-53edo │ 792   │
    │ Gb4 │ II     │ M3z-e53-1 │ 1585  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 40-53edo │ 906   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 40-53edo │ 906   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Gb5  │ I      │ M6z-e53-1 │ 2083  │ m6 - 35-53edo │ 792   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m6 - 35-53edo │ 792   │
    │ Db5 │ II     │ M7z-e53-1 │ 2287  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 40-53edo │ 906   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 40-53edo │ 906   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Db6  │ I      │ M3z-e53-2 │ 2785  │ m6 - 35-53edo │ 792   │
    │ Gb5 │ II     │ M3z-e53-2 │ 2785  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 40-53edo │ 906   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_dz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Db2 │ IV     │ m2z-e53   │ 113   │ Bb2  │ III    │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ Eb2 │ IV     │ m3z-e53   │ 317   │ C3   │ III    │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ Db3  │ III    │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ Gb2 │ IV     │ d5z-e53   │ 611   │ Eb3  │ III    │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Ab2 │ III    │ m2z-e53   │ 113   │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ Gb3  │ II     │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Ab3  │ II     │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ Db3 │ III    │ d5z-e53   │ 611   │ Bb3  │ II     │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Eb3 │ II     │ m2z-e53   │ 113   │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ F3  │ II     │ m3p-e53   │ 294   │ Db4  │ I      │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ Gb3 │ II     │ M3p-e53   │ 408   │ Eb4  │ I      │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Gb4  │ I      │ M6p-e53   │ 906   │ m6 - 36-53edo │ 815   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Ab4  │ I      │ M7p-e53   │ 1109  │ m6 - 36-53edo │ 815   │
    │ Db4 │ II     │ M7p-e53   │ 1109  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 39-53edo │ 883   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 39-53edo │ 883   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Db5  │ I      │ M3p-e53-1 │ 1608  │ m6 - 36-53edo │ 815   │
    │ Gb4 │ II     │ M3p-e53-1 │ 1608  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 39-53edo │ 883   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 39-53edo │ 883   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Gb5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 36-53edo │ 815   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m6 - 36-53edo │ 815   │
    │ Db5 │ II     │ M7p-e53-1 │ 2309  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 39-53edo │ 883   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 39-53edo │ 883   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Db6  │ I      │ M3p-e53-2 │ 2808  │ m6 - 36-53edo │ 815   │
    │ Gb5 │ II     │ M3p-e53-2 │ 2808  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 39-53edo │ 883   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ D2  │ IV     │ M2p-e53   │ 204   │ B2   │ III    │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ E2  │ IV     │ M3p-e53   │ 408   │ C#3  │ III    │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ D3   │ III    │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G3   │ II     │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A3   │ II     │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D4   │ I      │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6p-e53   │ 906   │ M6 - 40-53edo │ 906   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G4   │ I      │ m7p-e53   │ 996   │ m6 - 35-53edo │ 792   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A4   │ I      │ 0-1       │ 1200  │ m6 - 35-53edo │ 792   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M6 - 40-53edo │ 906   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M6 - 40-53edo │ 906   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m6 - 35-53edo │ 792   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M6 - 40-53edo │ 906   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M6 - 40-53edo │ 906   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m6 - 35-53edo │ 792   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A5   │ I      │ 0-2       │ 2400  │ m6 - 35-53edo │ 792   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M6 - 40-53edo │ 906   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M6 - 40-53edo │ 906   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m6 - 35-53edo │ 792   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "d_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_d in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ D2  │ IV     │ M2p-e53   │ 204   │ B2   │ III    │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ E2  │ IV     │ M3p-e53   │ 408   │ C#3  │ III    │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ F#2 │ IV     │ A4z-e53   │ 589   │ D3   │ III    │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ B2  │ III    │ M3z-e53   │ 385   │ G3   │ II     │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ A3   │ II     │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ D4   │ I      │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5z-e53    │ 679   │ M6 - 39-53edo │ 883   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6z-e53   │ 883   │ M6 - 39-53edo │ 883   │
    │ B3  │ II     │ M6z-e53   │ 883   │ G4   │ I      │ m7p-e53   │ 996   │ m6 - 36-53edo │ 815   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ A4   │ I      │ 0-1       │ 1200  │ m6 - 36-53edo │ 815   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M6 - 39-53edo │ 883   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M6 - 39-53edo │ 883   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m6 - 36-53edo │ 815   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5z-e53-1  │ 1879  │ M6 - 39-53edo │ 883   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M6 - 39-53edo │ 883   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m6 - 36-53edo │ 815   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ A5   │ I      │ 0-2       │ 2400  │ m6 - 36-53edo │ 815   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M6 - 39-53edo │ 883   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M6 - 39-53edo │ 883   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m6 - 36-53edo │ 815   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5z-e53-2  │ 3079  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_ez_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3z-e53   │ 317   │ C3   │ III    │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ F2  │ IV     │ 4z-e53    │ 521   │ D3   │ III    │ 5p-e53    │ 702   │ M6 - 39-53edo │ 883   │
    │ G2  │ III    │ 0         │ 0     │ Eb3  │ II     │ m2z-e53   │ 113   │ m6 - 36-53edo │ 815   │
    │ Ab2 │ III    │ m2z-e53   │ 113   │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ Bb2 │ III    │ m3z-e53   │ 317   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Ab3  │ II     │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ D3  │ II     │ 0         │ 0     │ Bb3  │ I      │ m2z-e53   │ 113   │ m6 - 36-53edo │ 815   │
    │ Eb3 │ II     │ m2z-e53   │ 113   │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ F3  │ II     │ m3z-e53   │ 317   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Eb4  │ I      │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Bb3 │ II     │ m6z-e53   │ 815   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 39-53edo │ 883   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Ab4  │ I      │ M7p-e53   │ 1109  │ m6 - 36-53edo │ 815   │
    │ D4  │ II     │ 0-1       │ 1200  │ Bb4  │ I      │ m2z-e53-1 │ 1313  │ m6 - 36-53edo │ 815   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 39-53edo │ 883   │
    │ F4  │ II     │ m3z-e53-1 │ 1517  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 39-53edo │ 883   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m6 - 36-53edo │ 815   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 39-53edo │ 883   │
    │ Bb4 │ II     │ m6z-e53-1 │ 2015  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 39-53edo │ 883   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m6 - 36-53edo │ 815   │
    │ D5  │ II     │ 0-2       │ 2400  │ Bb5  │ I      │ m2z-e53-2 │ 2513  │ m6 - 36-53edo │ 815   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 39-53edo │ 883   │
    │ F5  │ II     │ m3z-e53-2 │ 2717  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 39-53edo │ 883   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m6 - 36-53edo │ 815   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_ep_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Eb2 │ IV     │ m3p-e53   │ 294   │ C3   │ III    │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ D3   │ III    │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ G2  │ III    │ 0         │ 0     │ Eb3  │ II     │ A1z-e53   │ 91    │ m6 - 35-53edo │ 792   │
    │ Ab2 │ III    │ A1z-e53   │ 91    │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Ab3  │ II     │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ D3  │ II     │ 0         │ 0     │ Bb3  │ I      │ A1z-e53   │ 91    │ m6 - 35-53edo │ 792   │
    │ Eb3 │ II     │ A1z-e53   │ 91    │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ F3  │ II     │ m3p-e53   │ 294   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Eb4  │ I      │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 40-53edo │ 906   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Ab4  │ I      │ M7z-e53   │ 1087  │ m6 - 35-53edo │ 792   │
    │ D4  │ II     │ 0-1       │ 1200  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m6 - 35-53edo │ 792   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 40-53edo │ 906   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 40-53edo │ 906   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m6 - 35-53edo │ 792   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 40-53edo │ 906   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 40-53edo │ 906   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m6 - 35-53edo │ 792   │
    │ D5  │ II     │ 0-2       │ 2400  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m6 - 35-53edo │ 792   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 40-53edo │ 906   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 40-53edo │ 906   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m6 - 35-53edo │ 792   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ C#3  │ III    │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ D#3  │ III    │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ G#2 │ III    │ m2z-e53   │ 113   │ E3   │ II     │ M2p-e53   │ 204   │ m6 - 35-53edo │ 792   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A3   │ II     │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ D#3 │ II     │ m2z-e53   │ 113   │ B3   │ I      │ M2p-e53   │ 204   │ m6 - 35-53edo │ 792   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ E4   │ I      │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6p-e53   │ 906   │ M6 - 40-53edo │ 906   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7p-e53   │ 1109  │ M6 - 40-53edo │ 906   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A4   │ I      │ 0-1       │ 1200  │ m6 - 35-53edo │ 792   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m6 - 35-53edo │ 792   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M6 - 40-53edo │ 906   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M6 - 40-53edo │ 906   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m6 - 35-53edo │ 792   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M6 - 40-53edo │ 906   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M6 - 40-53edo │ 906   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A5   │ I      │ 0-2       │ 2400  │ m6 - 35-53edo │ 792   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m6 - 35-53edo │ 792   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M6 - 40-53edo │ 906   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M6 - 40-53edo │ 906   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m6 - 35-53edo │ 792   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "e_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_e in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ C#3  │ III    │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ F#2 │ IV     │ d5z-e53   │ 611   │ D#3  │ III    │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ G#2 │ III    │ A1z-e53   │ 91    │ E3   │ II     │ M2p-e53   │ 204   │ m6 - 36-53edo │ 815   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ A3   │ II     │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ D#3 │ II     │ A1z-e53   │ 91    │ B3   │ I      │ M2p-e53   │ 204   │ m6 - 36-53edo │ 815   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ E4   │ I      │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6z-e53   │ 883   │ M6 - 39-53edo │ 883   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7z-e53   │ 1087  │ M6 - 39-53edo │ 883   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ A4   │ I      │ 0-1       │ 1200  │ m6 - 36-53edo │ 815   │
    │ D#4 │ II     │ A1z-e53-1 │ 1291  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m6 - 36-53edo │ 815   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M6 - 39-53edo │ 883   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 39-53edo │ 883   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m6 - 36-53edo │ 815   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M6 - 39-53edo │ 883   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M6 - 39-53edo │ 883   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ A5   │ I      │ 0-2       │ 2400  │ m6 - 36-53edo │ 815   │
    │ D#5 │ II     │ A1z-e53-2 │ 2491  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m6 - 36-53edo │ 815   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M6 - 39-53edo │ 883   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 39-53edo │ 883   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m6 - 36-53edo │ 815   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "fp_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_fp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4p-e53    │ 498   │ D3   │ III    │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F3   │ II     │ m3p-e53   │ 294   │ m6 - 35-53edo │ 792   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ C3  │ III    │ 4p-e53    │ 498   │ A3   │ II     │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ D3  │ II     │ 0         │ 0     │ Bb3  │ I      │ A1z-e53   │ 91    │ m6 - 35-53edo │ 792   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C4   │ I      │ m3p-e53   │ 294   │ m6 - 35-53edo │ 792   │
    │ F3  │ II     │ m3p-e53   │ 294   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F4   │ I      │ m6p-e53   │ 792   │ m6 - 35-53edo │ 792   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 40-53edo │ 906   │
    │ C4  │ II     │ m7p-e53   │ 996   │ A4   │ I      │ 0-1       │ 1200  │ M6 - 40-53edo │ 906   │
    │ D4  │ II     │ 0-1       │ 1200  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m6 - 35-53edo │ 792   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m6 - 35-53edo │ 792   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 40-53edo │ 906   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M6 - 40-53edo │ 906   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m6 - 35-53edo │ 792   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 40-53edo │ 906   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ A5   │ I      │ 0-2       │ 2400  │ M6 - 40-53edo │ 906   │
    │ D5  │ II     │ 0-2       │ 2400  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m6 - 35-53edo │ 792   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m6 - 35-53edo │ 792   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 40-53edo │ 906   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "fp_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────────┬───────┬──────┬────────┬─────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos     │ Cents │ High │ String │ Pos     │ Cents │ Interval      │ Cents │
    ├─────┼────────┼─────────┼───────┼──────┼────────┼─────────┼───────┼───────────────┼───────┤
    │ F2  │ IV     │ 4p-e53  │ 498   │ D3   │ III    │ 5z-e53  │ 679   │ M6 - 39-53edo │ 883   │
    │ G2  │ III    │ 0       │ 0     │ E3   │ II     │ M2z-e53 │ 181   │ M6 - 39-53edo │ 883   │
    │ A2  │ III    │ M2z-e53 │ 181   │ F3   │ II     │ m3p-e53 │ 294   │ m6 - 36-53edo │ 815   │
    │ Bb2 │ III    │ m3z-e53 │ 317   │ G3   │ II     │ 4p-e53  │ 498   │ M6 - 39-53edo │ 883   │
    │ C3  │ III    │ 4p-e53  │ 498   │ A3   │ II     │ 5z-e53  │ 679   │ M6 - 39-53edo │ 883   │
    │ D3  │ III    │ 5z-e53  │ 679   │ Bb3  │ II     │ m6p-e53 │ 792   │ m6 - 36-53edo │ 815   │
    │ E3  │ II     │ M2z-e53 │ 181   │ C4   │ I      │ m3p-e53 │ 294   │ m6 - 36-53edo │ 815   │
    └─────┴────────┴─────────┴───────┴──────┴────────┴─────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "fz_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_fz in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬─────┬───────┬──────┬────────┬─────┬───────┬──────────┬───────┐
    │ Low │ String │ Pos │ Cents │ High │ String │ Pos │ Cents │ Interval │ Cents │
    ├┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┼┬┬┬┬┬┼┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┬┬┬┼┬┬┬┬┬┬┬┤
    └┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┘ |}]
;;

let%expect_test "f_sharp_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_fp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ d5z-e53   │ 611   │ D#3  │ III    │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ G#2 │ III    │ m2z-e53   │ 113   │ E#3  │ II     │ m3z-e53   │ 317   │ M6 - 40-53edo │ 906   │
    │ A#2 │ III    │ m3z-e53   │ 317   │ F#3  │ II     │ M3p-e53   │ 408   │ m6 - 35-53edo │ 792   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A#3  │ II     │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ D#3 │ II     │ m2z-e53   │ 113   │ B3   │ I      │ M2p-e53   │ 204   │ m6 - 35-53edo │ 792   │
    │ E#3 │ II     │ m3z-e53   │ 317   │ C#4  │ I      │ M3p-e53   │ 408   │ m6 - 35-53edo │ 792   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ E#4  │ I      │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ A#3 │ II     │ m6z-e53   │ 815   │ F#4  │ I      │ M6p-e53   │ 906   │ m6 - 35-53edo │ 792   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7p-e53   │ 1109  │ M6 - 40-53edo │ 906   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A#4  │ I      │ m2z-e53-1 │ 1313  │ M6 - 40-53edo │ 906   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m6 - 35-53edo │ 792   │
    │ E#4 │ II     │ m3z-e53-1 │ 1517  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m6 - 35-53edo │ 792   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M6 - 40-53edo │ 906   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ E#5  │ I      │ m6z-e53-1 │ 2015  │ M6 - 40-53edo │ 906   │
    │ A#4 │ II     │ m6z-e53-1 │ 2015  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 35-53edo │ 792   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M6 - 40-53edo │ 906   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A#5  │ I      │ m2z-e53-2 │ 2513  │ M6 - 40-53edo │ 906   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m6 - 35-53edo │ 792   │
    │ E#5 │ II     │ m3z-e53-2 │ 2717  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m6 - 35-53edo │ 792   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M6 - 40-53edo │ 906   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ E#6  │ I      │ m6z-e53-2 │ 3215  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "f_sharp_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_fp_sharp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ F#2 │ IV     │ d5z-e53   │ 611   │ D#3  │ III    │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ G#2 │ III    │ m2z-e53   │ 113   │ E#3  │ II     │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ A#2 │ III    │ m3p-e53   │ 294   │ F#3  │ II     │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A#3  │ II     │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ D#3 │ II     │ A1z-e53   │ 91    │ B3   │ I      │ M2p-e53   │ 204   │ m6 - 36-53edo │ 815   │
    │ E#3 │ II     │ m3p-e53   │ 294   │ C#4  │ I      │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ E#4  │ I      │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ A#3 │ II     │ m6p-e53   │ 792   │ F#4  │ I      │ M6p-e53   │ 906   │ m6 - 36-53edo │ 815   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7z-e53   │ 1087  │ M6 - 39-53edo │ 883   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A#4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 39-53edo │ 883   │
    │ D#4 │ II     │ A1z-e53-1 │ 1291  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m6 - 36-53edo │ 815   │
    │ E#4 │ II     │ m3p-e53-1 │ 1494  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ m6 - 36-53edo │ 815   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 39-53edo │ 883   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ E#5  │ I      │ m6p-e53-1 │ 1992  │ M6 - 39-53edo │ 883   │
    │ A#4 │ II     │ m6p-e53-1 │ 1992  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 36-53edo │ 815   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M6 - 39-53edo │ 883   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A#5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 39-53edo │ 883   │
    │ D#5 │ II     │ A1z-e53-2 │ 2491  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m6 - 36-53edo │ 815   │
    │ E#5 │ II     │ m3p-e53-2 │ 2694  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ m6 - 36-53edo │ 815   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 39-53edo │ 883   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ E#6  │ I      │ m6p-e53-2 │ 3192  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_gp_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ A4z-e53   │ 589   │ Eb3  │ III    │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Ab2 │ III    │ A1z-e53   │ 91    │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ Gb3  │ II     │ M3z-e53   │ 385   │ m6 - 35-53edo │ 792   │
    │ Cb3 │ III    │ M3z-e53   │ 385   │ Ab3  │ II     │ A4z-e53   │ 589   │ M6 - 40-53edo │ 906   │
    │ Db3 │ III    │ A4z-e53   │ 589   │ Bb3  │ II     │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Eb3 │ II     │ A1z-e53   │ 91    │ Cb4  │ I      │ M2z-e53   │ 181   │ m6 - 35-53edo │ 792   │
    │ F3  │ II     │ m3p-e53   │ 294   │ Db4  │ I      │ M3z-e53   │ 385   │ m6 - 35-53edo │ 792   │
    │ Gb3 │ II     │ M3z-e53   │ 385   │ Eb4  │ I      │ A4z-e53   │ 589   │ M6 - 40-53edo │ 906   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Gb4  │ I      │ M6z-e53   │ 883   │ m6 - 35-53edo │ 792   │
    │ Cb4 │ II     │ M6z-e53   │ 883   │ Ab4  │ I      │ M7z-e53   │ 1087  │ M6 - 40-53edo │ 906   │
    │ Db4 │ II     │ M7z-e53   │ 1087  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 40-53edo │ 906   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ Cb5  │ I      │ M2z-e53-1 │ 1381  │ m6 - 35-53edo │ 792   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Db5  │ I      │ M3z-e53-1 │ 1585  │ m6 - 35-53edo │ 792   │
    │ Gb4 │ II     │ M3z-e53-1 │ 1585  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 40-53edo │ 906   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 40-53edo │ 906   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Gb5  │ I      │ M6z-e53-1 │ 2083  │ m6 - 35-53edo │ 792   │
    │ Cb5 │ II     │ M6z-e53-1 │ 2083  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ M6 - 40-53edo │ 906   │
    │ Db5 │ II     │ M7z-e53-1 │ 2287  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 40-53edo │ 906   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ Cb6  │ I      │ M2z-e53-2 │ 2581  │ m6 - 35-53edo │ 792   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Db6  │ I      │ M3z-e53-2 │ 2785  │ m6 - 35-53edo │ 792   │
    │ Gb5 │ II     │ M3z-e53-2 │ 2785  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 40-53edo │ 906   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_gz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Gb2 │ IV     │ d5z-e53   │ 611   │ Eb3  │ III    │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Ab2 │ III    │ m2z-e53   │ 113   │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ Gb3  │ II     │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ Cb3 │ III    │ M3p-e53   │ 408   │ Ab3  │ II     │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ Db3 │ III    │ d5z-e53   │ 611   │ Bb3  │ II     │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Eb3 │ II     │ A1z-e53   │ 91    │ Cb4  │ I      │ M2p-e53   │ 204   │ m6 - 36-53edo │ 815   │
    │ F3  │ II     │ m3p-e53   │ 294   │ Db4  │ I      │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ Gb3 │ II     │ M3p-e53   │ 408   │ Eb4  │ I      │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ Gb4  │ I      │ M6p-e53   │ 906   │ m6 - 36-53edo │ 815   │
    │ Cb4 │ II     │ M6p-e53   │ 906   │ Ab4  │ I      │ M7z-e53   │ 1087  │ M6 - 39-53edo │ 883   │
    │ Db4 │ II     │ M7p-e53   │ 1109  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 39-53edo │ 883   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ Cb5  │ I      │ M2p-e53-1 │ 1404  │ m6 - 36-53edo │ 815   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Db5  │ I      │ M3p-e53-1 │ 1608  │ m6 - 36-53edo │ 815   │
    │ Gb4 │ II     │ M3p-e53-1 │ 1608  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 39-53edo │ 883   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 39-53edo │ 883   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ Gb5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 36-53edo │ 815   │
    │ Cb5 │ II     │ M6p-e53-1 │ 2106  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ M6 - 39-53edo │ 883   │
    │ Db5 │ II     │ M7p-e53-1 │ 2309  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 39-53edo │ 883   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ Cb6  │ I      │ M2p-e53-2 │ 2604  │ m6 - 36-53edo │ 815   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Db6  │ I      │ M3p-e53-2 │ 2808  │ m6 - 36-53edo │ 815   │
    │ Gb5 │ II     │ M3p-e53-2 │ 2808  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 39-53edo │ 883   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_g in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G3   │ II     │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ C3  │ III    │ 4p-e53    │ 498   │ A3   │ II     │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C4   │ I      │ m3p-e53   │ 294   │ m6 - 35-53edo │ 792   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D4   │ I      │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6p-e53   │ 906   │ M6 - 40-53edo │ 906   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G4   │ I      │ m7p-e53   │ 996   │ m6 - 35-53edo │ 792   │
    │ C4  │ II     │ m7p-e53   │ 996   │ A4   │ I      │ 0-1       │ 1200  │ M6 - 40-53edo │ 906   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M6 - 40-53edo │ 906   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m6 - 35-53edo │ 792   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m6 - 35-53edo │ 792   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M6 - 40-53edo │ 906   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M6 - 40-53edo │ 906   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m6 - 35-53edo │ 792   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ A5   │ I      │ 0-2       │ 2400  │ M6 - 40-53edo │ 906   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M6 - 40-53edo │ 906   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m6 - 35-53edo │ 792   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m6 - 35-53edo │ 792   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "g_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_g in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ G2  │ III    │ 0         │ 0     │ E3   │ II     │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ B2  │ III    │ M3z-e53   │ 385   │ G3   │ II     │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ C3  │ III    │ 4z-e53    │ 521   │ A3   │ II     │ 5p-e53    │ 702   │ M6 - 39-53edo │ 883   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ E3  │ II     │ M2z-e53   │ 181   │ C4   │ I      │ m3p-e53   │ 294   │ m6 - 36-53edo │ 815   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ D4   │ I      │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ G3  │ II     │ 4p-e53    │ 498   │ E4   │ I      │ 5z-e53    │ 679   │ M6 - 39-53edo │ 883   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6z-e53   │ 883   │ M6 - 39-53edo │ 883   │
    │ B3  │ II     │ M6z-e53   │ 883   │ G4   │ I      │ m7p-e53   │ 996   │ m6 - 36-53edo │ 815   │
    │ C4  │ II     │ m7z-e53   │ 1019  │ A4   │ I      │ 0-1       │ 1200  │ M6 - 39-53edo │ 883   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M6 - 39-53edo │ 883   │
    │ E4  │ II     │ M2z-e53-1 │ 1381  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m6 - 36-53edo │ 815   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m6 - 36-53edo │ 815   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ E5   │ I      │ 5z-e53-1  │ 1879  │ M6 - 39-53edo │ 883   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M6 - 39-53edo │ 883   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m6 - 36-53edo │ 815   │
    │ C5  │ II     │ m7z-e53-1 │ 2219  │ A5   │ I      │ 0-2       │ 2400  │ M6 - 39-53edo │ 883   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M6 - 39-53edo │ 883   │
    │ E5  │ II     │ M2z-e53-2 │ 2581  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m6 - 36-53edo │ 815   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m6 - 36-53edo │ 815   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ E6   │ I      │ 5z-e53-2  │ 3079  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_ap_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Ab2 │ III    │ A1z-e53   │ 91    │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ Bb2 │ III    │ m3p-e53   │ 294   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Ab3  │ II     │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ Db3 │ III    │ A4z-e53   │ 589   │ Bb3  │ II     │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Eb3 │ II     │ A1z-e53   │ 91    │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ F3  │ II     │ m3p-e53   │ 294   │ Db4  │ I      │ M3z-e53   │ 385   │ m6 - 35-53edo │ 792   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Eb4  │ I      │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ Ab3 │ II     │ A4z-e53   │ 589   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 40-53edo │ 906   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 40-53edo │ 906   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Ab4  │ I      │ M7z-e53   │ 1087  │ m6 - 35-53edo │ 792   │
    │ Db4 │ II     │ M7z-e53   │ 1087  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 40-53edo │ 906   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 40-53edo │ 906   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Db5  │ I      │ M3z-e53-1 │ 1585  │ m6 - 35-53edo │ 792   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m6 - 35-53edo │ 792   │
    │ Ab4 │ II     │ A4z-e53-1 │ 1789  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 40-53edo │ 906   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 40-53edo │ 906   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Ab5  │ I      │ M7z-e53-1 │ 2287  │ m6 - 35-53edo │ 792   │
    │ Db5 │ II     │ M7z-e53-1 │ 2287  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 40-53edo │ 906   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 40-53edo │ 906   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Db6  │ I      │ M3z-e53-2 │ 2785  │ m6 - 35-53edo │ 792   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m6 - 35-53edo │ 792   │
    │ Ab5 │ II     │ A4z-e53-2 │ 2989  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 40-53edo │ 906   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_az_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Ab2 │ III    │ m2z-e53   │ 113   │ F3   │ II     │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ Bb2 │ III    │ m3z-e53   │ 317   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ C3  │ III    │ 4p-e53    │ 498   │ Ab3  │ II     │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ Db3 │ III    │ d5z-e53   │ 611   │ Bb3  │ II     │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Eb3 │ II     │ m2z-e53   │ 113   │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ F3  │ II     │ m3p-e53   │ 294   │ Db4  │ I      │ M3p-e53   │ 408   │ m6 - 36-53edo │ 815   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Eb4  │ I      │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ Ab3 │ II     │ d5z-e53   │ 611   │ F4   │ I      │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ Bb3 │ II     │ m6z-e53   │ 815   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 39-53edo │ 883   │
    │ C4  │ II     │ m7p-e53   │ 996   │ Ab4  │ I      │ M7p-e53   │ 1109  │ m6 - 36-53edo │ 815   │
    │ Db4 │ II     │ M7p-e53   │ 1109  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 39-53edo │ 883   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 39-53edo │ 883   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ Db5  │ I      │ M3p-e53-1 │ 1608  │ m6 - 36-53edo │ 815   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m6 - 36-53edo │ 815   │
    │ Ab4 │ II     │ d5z-e53-1 │ 1811  │ F5   │ I      │ m6p-e53-1 │ 1992  │ M6 - 39-53edo │ 883   │
    │ Bb4 │ II     │ m6z-e53-1 │ 2015  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 39-53edo │ 883   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ Ab5  │ I      │ M7p-e53-1 │ 2309  │ m6 - 36-53edo │ 815   │
    │ Db5 │ II     │ M7p-e53-1 │ 2309  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 39-53edo │ 883   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 39-53edo │ 883   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ Db6  │ I      │ M3p-e53-2 │ 2808  │ m6 - 36-53edo │ 815   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m6 - 36-53edo │ 815   │
    │ Ab5 │ II     │ d5z-e53-2 │ 3011  │ F6   │ I      │ m6p-e53-2 │ 3192  │ M6 - 39-53edo │ 883   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A3   │ II     │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2p-e53   │ 204   │ M6 - 40-53edo │ 906   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D4   │ I      │ 4p-e53    │ 498   │ m6 - 35-53edo │ 792   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ E4   │ I      │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6p-e53   │ 906   │ M6 - 40-53edo │ 906   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7p-e53   │ 1109  │ M6 - 40-53edo │ 906   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A4   │ I      │ 0-1       │ 1200  │ m6 - 35-53edo │ 792   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M6 - 40-53edo │ 906   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M6 - 40-53edo │ 906   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m6 - 35-53edo │ 792   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m6 - 35-53edo │ 792   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ M6 - 40-53edo │ 906   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M6 - 40-53edo │ 906   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A5   │ I      │ 0-2       │ 2400  │ m6 - 35-53edo │ 792   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M6 - 40-53edo │ 906   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M6 - 40-53edo │ 906   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m6 - 35-53edo │ 792   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m6 - 35-53edo │ 792   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "a_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_a in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ A2  │ III    │ M2p-e53   │ 204   │ F#3  │ II     │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ C#3 │ III    │ A4z-e53   │ 589   │ A3   │ II     │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ D3  │ II     │ 0         │ 0     │ B3   │ I      │ M2z-e53   │ 181   │ M6 - 39-53edo │ 883   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ F#3 │ II     │ M3z-e53   │ 385   │ D4   │ I      │ 4p-e53    │ 498   │ m6 - 36-53edo │ 815   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ E4   │ I      │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F#4  │ I      │ M6z-e53   │ 883   │ M6 - 39-53edo │ 883   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7z-e53   │ 1087  │ M6 - 39-53edo │ 883   │
    │ C#4 │ II     │ M7z-e53   │ 1087  │ A4   │ I      │ 0-1       │ 1200  │ m6 - 36-53edo │ 815   │
    │ D4  │ II     │ 0-1       │ 1200  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M6 - 39-53edo │ 883   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M6 - 39-53edo │ 883   │
    │ F#4 │ II     │ M3z-e53-1 │ 1585  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m6 - 36-53edo │ 815   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m6 - 36-53edo │ 815   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F#5  │ I      │ M6z-e53-1 │ 2083  │ M6 - 39-53edo │ 883   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M6 - 39-53edo │ 883   │
    │ C#5 │ II     │ M7z-e53-1 │ 2287  │ A5   │ I      │ 0-2       │ 2400  │ m6 - 36-53edo │ 815   │
    │ D5  │ II     │ 0-2       │ 2400  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M6 - 39-53edo │ 883   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M6 - 39-53edo │ 883   │
    │ F#5 │ II     │ M3z-e53-2 │ 2785  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m6 - 36-53edo │ 815   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m6 - 36-53edo │ 815   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_bp_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3p-e53   │ 294   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ C3  │ III    │ 4p-e53    │ 498   │ A3   │ II     │ 5p-e53    │ 702   │ M6 - 40-53edo │ 906   │
    │ D3  │ II     │ 0         │ 0     │ Bb3  │ I      │ A1z-e53   │ 91    │ m6 - 35-53edo │ 792   │
    │ Eb3 │ II     │ A1z-e53   │ 91    │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 40-53edo │ 906   │
    │ F3  │ II     │ m3p-e53   │ 294   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 40-53edo │ 906   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Eb4  │ I      │ A4z-e53   │ 589   │ m6 - 35-53edo │ 792   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F4   │ I      │ m6p-e53   │ 792   │ m6 - 35-53edo │ 792   │
    │ Bb3 │ II     │ m6p-e53   │ 792   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 40-53edo │ 906   │
    │ C4  │ II     │ m7p-e53   │ 996   │ A4   │ I      │ 0-1       │ 1200  │ M6 - 40-53edo │ 906   │
    │ D4  │ II     │ 0-1       │ 1200  │ Bb4  │ I      │ A1z-e53-1 │ 1291  │ m6 - 35-53edo │ 792   │
    │ Eb4 │ II     │ A1z-e53-1 │ 1291  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 40-53edo │ 906   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 40-53edo │ 906   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Eb5  │ I      │ A4z-e53-1 │ 1789  │ m6 - 35-53edo │ 792   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m6 - 35-53edo │ 792   │
    │ Bb4 │ II     │ m6p-e53-1 │ 1992  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 40-53edo │ 906   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ A5   │ I      │ 0-2       │ 2400  │ M6 - 40-53edo │ 906   │
    │ D5  │ II     │ 0-2       │ 2400  │ Bb5  │ I      │ A1z-e53-2 │ 2491  │ m6 - 35-53edo │ 792   │
    │ Eb5 │ II     │ A1z-e53-2 │ 2491  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 40-53edo │ 906   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 40-53edo │ 906   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Eb6  │ I      │ A4z-e53-2 │ 2989  │ m6 - 35-53edo │ 792   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ F6   │ I      │ m6p-e53-2 │ 3192  │ m6 - 35-53edo │ 792   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_flat_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_bz_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ Bb2 │ III    │ m3z-e53   │ 317   │ G3   │ II     │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ C3  │ III    │ 4z-e53    │ 521   │ A3   │ II     │ 5p-e53    │ 702   │ M6 - 39-53edo │ 883   │
    │ D3  │ II     │ 0         │ 0     │ Bb3  │ I      │ m2z-e53   │ 113   │ m6 - 36-53edo │ 815   │
    │ Eb3 │ II     │ m2z-e53   │ 113   │ C4   │ I      │ m3p-e53   │ 294   │ M6 - 39-53edo │ 883   │
    │ F3  │ II     │ m3z-e53   │ 317   │ D4   │ I      │ 4p-e53    │ 498   │ M6 - 39-53edo │ 883   │
    │ G3  │ II     │ 4p-e53    │ 498   │ Eb4  │ I      │ d5z-e53   │ 611   │ m6 - 36-53edo │ 815   │
    │ A3  │ II     │ 5p-e53    │ 702   │ F4   │ I      │ m6z-e53   │ 815   │ m6 - 36-53edo │ 815   │
    │ Bb3 │ II     │ m6z-e53   │ 815   │ G4   │ I      │ m7p-e53   │ 996   │ M6 - 39-53edo │ 883   │
    │ C4  │ II     │ m7z-e53   │ 1019  │ A4   │ I      │ 0-1       │ 1200  │ M6 - 39-53edo │ 883   │
    │ D4  │ II     │ 0-1       │ 1200  │ Bb4  │ I      │ m2z-e53-1 │ 1313  │ m6 - 36-53edo │ 815   │
    │ Eb4 │ II     │ m2z-e53-1 │ 1313  │ C5   │ I      │ m3p-e53-1 │ 1494  │ M6 - 39-53edo │ 883   │
    │ F4  │ II     │ m3z-e53-1 │ 1517  │ D5   │ I      │ 4p-e53-1  │ 1698  │ M6 - 39-53edo │ 883   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ Eb5  │ I      │ d5z-e53-1 │ 1811  │ m6 - 36-53edo │ 815   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ F5   │ I      │ m6z-e53-1 │ 2015  │ m6 - 36-53edo │ 815   │
    │ Bb4 │ II     │ m6z-e53-1 │ 2015  │ G5   │ I      │ m7p-e53-1 │ 2196  │ M6 - 39-53edo │ 883   │
    │ C5  │ II     │ m7z-e53-1 │ 2219  │ A5   │ I      │ 0-2       │ 2400  │ M6 - 39-53edo │ 883   │
    │ D5  │ II     │ 0-2       │ 2400  │ Bb5  │ I      │ m2z-e53-2 │ 2513  │ m6 - 36-53edo │ 815   │
    │ Eb5 │ II     │ m2z-e53-2 │ 2513  │ C6   │ I      │ m3p-e53-2 │ 2694  │ M6 - 39-53edo │ 883   │
    │ F5  │ II     │ m3z-e53-2 │ 2717  │ D6   │ I      │ 4p-e53-2  │ 2898  │ M6 - 39-53edo │ 883   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ Eb6  │ I      │ d5z-e53-2 │ 3011  │ m6 - 36-53edo │ 815   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ F6   │ I      │ m6z-e53-2 │ 3215  │ m6 - 36-53edo │ 815   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_bp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A#3  │ II     │ m6z-e53   │ 815   │ M6 - 40-53edo │ 906   │
    │ D#3 │ II     │ m2z-e53   │ 113   │ B3   │ I      │ M2p-e53   │ 204   │ m6 - 35-53edo │ 792   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3p-e53   │ 408   │ M6 - 40-53edo │ 906   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ d5z-e53   │ 611   │ M6 - 40-53edo │ 906   │
    │ G#3 │ II     │ d5z-e53   │ 611   │ E4   │ I      │ 5p-e53    │ 702   │ m6 - 35-53edo │ 792   │
    │ A#3 │ II     │ m6z-e53   │ 815   │ F#4  │ I      │ M6p-e53   │ 906   │ m6 - 35-53edo │ 792   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7p-e53   │ 1109  │ M6 - 40-53edo │ 906   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A#4  │ I      │ m2z-e53-1 │ 1313  │ M6 - 40-53edo │ 906   │
    │ D#4 │ II     │ m2z-e53-1 │ 1313  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m6 - 35-53edo │ 792   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3p-e53-1 │ 1608  │ M6 - 40-53edo │ 906   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ d5z-e53-1 │ 1811  │ M6 - 40-53edo │ 906   │
    │ G#4 │ II     │ d5z-e53-1 │ 1811  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m6 - 35-53edo │ 792   │
    │ A#4 │ II     │ m6z-e53-1 │ 2015  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 35-53edo │ 792   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7p-e53-1 │ 2309  │ M6 - 40-53edo │ 906   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A#5  │ I      │ m2z-e53-2 │ 2513  │ M6 - 40-53edo │ 906   │
    │ D#5 │ II     │ m2z-e53-2 │ 2513  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m6 - 35-53edo │ 792   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3p-e53-2 │ 2808  │ M6 - 40-53edo │ 906   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ d5z-e53-2 │ 3011  │ M6 - 40-53edo │ 906   │
    │ G#5 │ II     │ d5z-e53-2 │ 3011  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m6 - 35-53edo │ 792   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "b_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale ~from:Scales.lower_bp in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ B2  │ III    │ M3p-e53   │ 408   │ G#3  │ II     │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ C#3 │ III    │ d5z-e53   │ 611   │ A#3  │ II     │ m6p-e53   │ 792   │ M6 - 39-53edo │ 883   │
    │ D#3 │ II     │ A1z-e53   │ 91    │ B3   │ I      │ M2p-e53   │ 204   │ m6 - 36-53edo │ 815   │
    │ E3  │ II     │ M2p-e53   │ 204   │ C#4  │ I      │ M3z-e53   │ 385   │ M6 - 39-53edo │ 883   │
    │ F#3 │ II     │ M3p-e53   │ 408   │ D#4  │ I      │ A4z-e53   │ 589   │ M6 - 39-53edo │ 883   │
    │ G#3 │ II     │ A4z-e53   │ 589   │ E4   │ I      │ 5p-e53    │ 702   │ m6 - 36-53edo │ 815   │
    │ A#3 │ II     │ m6p-e53   │ 792   │ F#4  │ I      │ M6p-e53   │ 906   │ m6 - 36-53edo │ 815   │
    │ B3  │ II     │ M6p-e53   │ 906   │ G#4  │ I      │ M7z-e53   │ 1087  │ M6 - 39-53edo │ 883   │
    │ C#4 │ II     │ M7p-e53   │ 1109  │ A#4  │ I      │ A1z-e53-1 │ 1291  │ M6 - 39-53edo │ 883   │
    │ D#4 │ II     │ A1z-e53-1 │ 1291  │ B4   │ I      │ M2p-e53-1 │ 1404  │ m6 - 36-53edo │ 815   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ C#5  │ I      │ M3z-e53-1 │ 1585  │ M6 - 39-53edo │ 883   │
    │ F#4 │ II     │ M3p-e53-1 │ 1608  │ D#5  │ I      │ A4z-e53-1 │ 1789  │ M6 - 39-53edo │ 883   │
    │ G#4 │ II     │ A4z-e53-1 │ 1789  │ E5   │ I      │ 5p-e53-1  │ 1902  │ m6 - 36-53edo │ 815   │
    │ A#4 │ II     │ m6p-e53-1 │ 1992  │ F#5  │ I      │ M6p-e53-1 │ 2106  │ m6 - 36-53edo │ 815   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ G#5  │ I      │ M7z-e53-1 │ 2287  │ M6 - 39-53edo │ 883   │
    │ C#5 │ II     │ M7p-e53-1 │ 2309  │ A#5  │ I      │ A1z-e53-2 │ 2491  │ M6 - 39-53edo │ 883   │
    │ D#5 │ II     │ A1z-e53-2 │ 2491  │ B5   │ I      │ M2p-e53-2 │ 2604  │ m6 - 36-53edo │ 815   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ C#6  │ I      │ M3z-e53-2 │ 2785  │ M6 - 39-53edo │ 883   │
    │ F#5 │ II     │ M3p-e53-2 │ 2808  │ D#6  │ I      │ A4z-e53-2 │ 2989  │ M6 - 39-53edo │ 883   │
    │ G#5 │ II     │ A4z-e53-2 │ 2989  │ E6   │ I      │ 5p-e53-2  │ 3102  │ m6 - 36-53edo │ 815   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;
