open! Core
open! Fingerboard

let make_scale ~characterized_scale ~from =
  let t = force Pythagorean.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_pythagorean ~from
;;

let%expect_test "c_major" =
  let t = force Pythagorean.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval       │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼────────────────┼───────┤
    │ C2  │ IV     │ 0     │ 0     │ A2   │ III    │ M2p   │ 204   │ M6 - 3^3 / 2^4 │ 906   │
    │ D2  │ IV     │ M2p   │ 204   │ B2   │ III    │ M3p   │ 408   │ M6 - 3^3 / 2^4 │ 906   │
    │ E2  │ IV     │ M3p   │ 408   │ C3   │ III    │ 4p    │ 498   │ m6 - 2^7 / 3^4 │ 792   │
    │ F2  │ IV     │ 4p    │ 498   │ D3   │ II     │ 0     │ 0     │ M6 - 3^3 / 2^4 │ 906   │
    │ G2  │ III    │ 0     │ 0     │ E3   │ II     │ M2p   │ 204   │ M6 - 3^3 / 2^4 │ 906   │
    │ A2  │ III    │ M2p   │ 204   │ F3   │ II     │ m3p   │ 294   │ m6 - 2^7 / 3^4 │ 792   │
    │ B2  │ III    │ M3p   │ 408   │ G3   │ II     │ 4p    │ 498   │ m6 - 2^7 / 3^4 │ 792   │
    │ C3  │ III    │ 4p    │ 498   │ A3   │ I      │ 0     │ 0     │ M6 - 3^3 / 2^4 │ 906   │
    │ D3  │ II     │ 0     │ 0     │ B3   │ I      │ M2p   │ 204   │ M6 - 3^3 / 2^4 │ 906   │
    │ E3  │ II     │ M2p   │ 204   │ C4   │ I      │ m3p   │ 294   │ m6 - 2^7 / 3^4 │ 792   │
    │ F3  │ II     │ m3p   │ 294   │ D4   │ I      │ 4p    │ 498   │ M6 - 3^3 / 2^4 │ 906   │
    │ G3  │ II     │ 4p    │ 498   │ E4   │ I      │ 5p    │ 702   │ M6 - 3^3 / 2^4 │ 906   │
    │ A3  │ II     │ 5p    │ 702   │ F4   │ I      │ m6p   │ 792   │ m6 - 2^7 / 3^4 │ 792   │
    │ B3  │ II     │ M6p   │ 906   │ G4   │ I      │ m7p   │ 996   │ m6 - 2^7 / 3^4 │ 792   │
    │ C4  │ II     │ m7p   │ 996   │ A4   │ I      │ 0-1   │ 1200  │ M6 - 3^3 / 2^4 │ 906   │
    │ D4  │ II     │ 0-1   │ 1200  │ B4   │ I      │ M2p-1 │ 1404  │ M6 - 3^3 / 2^4 │ 906   │
    │ E4  │ II     │ M2p-1 │ 1404  │ C5   │ I      │ m3p-1 │ 1494  │ m6 - 2^7 / 3^4 │ 792   │
    │ F4  │ II     │ m3p-1 │ 1494  │ D5   │ I      │ 4p-1  │ 1698  │ M6 - 3^3 / 2^4 │ 906   │
    │ G4  │ II     │ 4p-1  │ 1698  │ E5   │ I      │ 5p-1  │ 1902  │ M6 - 3^3 / 2^4 │ 906   │
    │ A4  │ II     │ 5p-1  │ 1902  │ F5   │ I      │ m6p-1 │ 1992  │ m6 - 2^7 / 3^4 │ 792   │
    │ B4  │ II     │ M6p-1 │ 2106  │ G5   │ I      │ m7p-1 │ 2196  │ m6 - 2^7 / 3^4 │ 792   │
    │ C5  │ II     │ m7p-1 │ 2196  │ A5   │ I      │ 0-2   │ 2400  │ M6 - 3^3 / 2^4 │ 906   │
    │ D5  │ II     │ 0-2   │ 2400  │ B5   │ I      │ M2p-2 │ 2604  │ M6 - 3^3 / 2^4 │ 906   │
    │ E5  │ II     │ M2p-2 │ 2604  │ C6   │ I      │ m3p-2 │ 2694  │ m6 - 2^7 / 3^4 │ 792   │
    │ F5  │ II     │ m3p-2 │ 2694  │ D6   │ I      │ 4p-2  │ 2898  │ M6 - 3^3 / 2^4 │ 906   │
    │ G5  │ II     │ 4p-2  │ 2898  │ E6   │ I      │ 5p-2  │ 3102  │ M6 - 3^3 / 2^4 │ 906   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴────────────────┴───────┘ |}]
;;
