open! Core
open! Fingerboard

let make_scale ~characterized_scale ~from =
  let t = force Just.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:Third
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_pythagorean_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_pythagorean ~from
;;

let make_major_just_scale ~from =
  make_scale ~characterized_scale:Characterized_scale.major_just ~from
;;

let%expect_test "c_major_just" =
  let t = force Just.t in
  let scale = make_major_just_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval         │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────────┼───────┤
    │ E2  │ IV     │ M3z   │ 386   │ G2   │ III    │ 0     │ 0     │ m3 - (2 * 3) / 5 │ 316   │
    │ F2  │ IV     │ 4p    │ 498   │ A2   │ III    │ M2z   │ 182   │ M3 - 5 / 2^2     │ 386   │
    │ G2  │ IV     │ 5p    │ 702   │ B2   │ III    │ M3z   │ 386   │ M3 - 5 / 2^2     │ 386   │
    │ A2  │ IV     │ M6z   │ 884   │ C3   │ III    │ 4p    │ 498   │ m3 - (2 * 3) / 5 │ 316   │
    │ B2  │ III    │ M3z   │ 386   │ D3   │ II     │ 0     │ 0     │ m3 - (2 * 3) / 5 │ 316   │
    │ C3  │ III    │ 4p    │ 498   │ E3   │ II     │ M2z   │ 182   │ M3 - 5 / 2^2     │ 386   │
    │ D3  │ III    │ 5p    │ 702   │ F3   │ II     │ m3p   │ 294   │ m3 - 2^5 / 3^3   │ 294   │
    │ E3  │ III    │ M6z   │ 884   │ G3   │ II     │ 4p    │ 498   │ m3 - (2 * 3) / 5 │ 316   │
    │ F3  │ III    │ m7p   │ 996   │ A3   │ II     │ 5z    │ 680   │ M3 - 5 / 2^2     │ 386   │
    │ G3  │ II     │ 4p    │ 498   │ B3   │ I      │ M2z   │ 182   │ M3 - 5 / 2^2     │ 386   │
    │ A3  │ II     │ 5z    │ 680   │ C4   │ I      │ m3p   │ 294   │ m3 - (2 * 3) / 5 │ 316   │
    │ B3  │ II     │ M6z   │ 884   │ D4   │ I      │ 4p    │ 498   │ m3 - (2 * 3) / 5 │ 316   │
    │ C4  │ II     │ m7p   │ 996   │ E4   │ I      │ 5z    │ 680   │ M3 - 5 / 2^2     │ 386   │
    │ D4  │ II     │ 0-1   │ 1200  │ F4   │ I      │ m6p   │ 792   │ m3 - 2^5 / 3^3   │ 294   │
    │ E4  │ II     │ M2z-1 │ 1382  │ G4   │ I      │ m7p   │ 996   │ m3 - (2 * 3) / 5 │ 316   │
    │ F4  │ II     │ m3p-1 │ 1494  │ A4   │ I      │ 8z    │ 1178  │ M3 - 5 / 2^2     │ 386   │
    │ G4  │ II     │ 4p-1  │ 1698  │ B4   │ I      │ M2z-1 │ 1382  │ M3 - 5 / 2^2     │ 386   │
    │ A4  │ II     │ 5z-1  │ 1880  │ C5   │ I      │ m3p-1 │ 1494  │ m3 - (2 * 3) / 5 │ 316   │
    │ B4  │ II     │ M6z-1 │ 2084  │ D5   │ I      │ 4p-1  │ 1698  │ m3 - (2 * 3) / 5 │ 316   │
    │ C5  │ II     │ m7p-1 │ 2196  │ E5   │ I      │ 5z-1  │ 1880  │ M3 - 5 / 2^2     │ 386   │
    │ D5  │ II     │ 0-2   │ 2400  │ F5   │ I      │ m6p-1 │ 1992  │ m3 - 2^5 / 3^3   │ 294   │
    │ E5  │ II     │ M2z-2 │ 2582  │ G5   │ I      │ m7p-1 │ 2196  │ m3 - (2 * 3) / 5 │ 316   │
    │ F5  │ II     │ m3p-2 │ 2694  │ A5   │ I      │ 8z-1  │ 2378  │ M3 - 5 / 2^2     │ 386   │
    │ G5  │ II     │ 4p-2  │ 2898  │ B5   │ I      │ M2z-2 │ 2582  │ M3 - 5 / 2^2     │ 386   │
    │ A5  │ II     │ 5z-2  │ 3080  │ C6   │ I      │ m3p-2 │ 2694  │ m3 - (2 * 3) / 5 │ 316   │
    │ B5  │ II     │ M6z-2 │ 3284  │ D6   │ I      │ 4p-2  │ 2898  │ m3 - (2 * 3) / 5 │ 316   │
    │ C6  │ II     │ m7p-2 │ 3396  │ E6   │ I      │ 5z-2  │ 3080  │ M3 - 5 / 2^2     │ 386   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────────┴───────┘ |}]
;;

let%expect_test "c_major_pythagorean" =
  let t = force Just.t in
  let scale = make_major_pythagorean_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬────────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval       │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼────────────────┼───────┤
    │ E2  │ IV     │ M3p   │ 408   │ G2   │ III    │ 0     │ 0     │ m3 - 2^5 / 3^3 │ 294   │
    │ F2  │ IV     │ 4p    │ 498   │ A2   │ III    │ M2p   │ 204   │ M3 - 3^4 / 2^6 │ 408   │
    │ G2  │ IV     │ 5p    │ 702   │ B2   │ III    │ M3p   │ 408   │ M3 - 3^4 / 2^6 │ 408   │
    │ A2  │ IV     │ M6p   │ 906   │ C3   │ III    │ 4p    │ 498   │ m3 - 2^5 / 3^3 │ 294   │
    │ B2  │ III    │ M3p   │ 408   │ D3   │ II     │ 0     │ 0     │ m3 - 2^5 / 3^3 │ 294   │
    │ C3  │ III    │ 4p    │ 498   │ E3   │ II     │ M2p   │ 204   │ M3 - 3^4 / 2^6 │ 408   │
    │ D3  │ III    │ 5p    │ 702   │ F3   │ II     │ m3p   │ 294   │ m3 - 2^5 / 3^3 │ 294   │
    │ E3  │ III    │ M6p   │ 906   │ G3   │ II     │ 4p    │ 498   │ m3 - 2^5 / 3^3 │ 294   │
    │ F3  │ II     │ m3p   │ 294   │ A3   │ I      │ 0     │ 0     │ M3 - 3^4 / 2^6 │ 408   │
    │ G3  │ II     │ 4p    │ 498   │ B3   │ I      │ M2p   │ 204   │ M3 - 3^4 / 2^6 │ 408   │
    │ A3  │ II     │ 5p    │ 702   │ C4   │ I      │ m3p   │ 294   │ m3 - 2^5 / 3^3 │ 294   │
    │ B3  │ II     │ M6p   │ 906   │ D4   │ I      │ 4p    │ 498   │ m3 - 2^5 / 3^3 │ 294   │
    │ C4  │ II     │ m7p   │ 996   │ E4   │ I      │ 5p    │ 702   │ M3 - 3^4 / 2^6 │ 408   │
    │ D4  │ II     │ 0-1   │ 1200  │ F4   │ I      │ m6p   │ 792   │ m3 - 2^5 / 3^3 │ 294   │
    │ E4  │ II     │ M2p-1 │ 1404  │ G4   │ I      │ m7p   │ 996   │ m3 - 2^5 / 3^3 │ 294   │
    │ F4  │ II     │ m3p-1 │ 1494  │ A4   │ I      │ 0-1   │ 1200  │ M3 - 3^4 / 2^6 │ 408   │
    │ G4  │ II     │ 4p-1  │ 1698  │ B4   │ I      │ M2p-1 │ 1404  │ M3 - 3^4 / 2^6 │ 408   │
    │ A4  │ II     │ 5p-1  │ 1902  │ C5   │ I      │ m3p-1 │ 1494  │ m3 - 2^5 / 3^3 │ 294   │
    │ B4  │ II     │ M6p-1 │ 2106  │ D5   │ I      │ 4p-1  │ 1698  │ m3 - 2^5 / 3^3 │ 294   │
    │ C5  │ II     │ m7p-1 │ 2196  │ E5   │ I      │ 5p-1  │ 1902  │ M3 - 3^4 / 2^6 │ 408   │
    │ D5  │ II     │ 0-2   │ 2400  │ F5   │ I      │ m6p-1 │ 1992  │ m3 - 2^5 / 3^3 │ 294   │
    │ E5  │ II     │ M2p-2 │ 2604  │ G5   │ I      │ m7p-1 │ 2196  │ m3 - 2^5 / 3^3 │ 294   │
    │ F5  │ II     │ m3p-2 │ 2694  │ A5   │ I      │ 0-2   │ 2400  │ M3 - 3^4 / 2^6 │ 408   │
    │ G5  │ II     │ 4p-2  │ 2898  │ B5   │ I      │ M2p-2 │ 2604  │ M3 - 3^4 / 2^6 │ 408   │
    │ A5  │ II     │ 5p-2  │ 3102  │ C6   │ I      │ m3p-2 │ 2694  │ m3 - 2^5 / 3^3 │ 294   │
    │ B5  │ II     │ M6p-2 │ 3306  │ D6   │ I      │ 4p-2  │ 2898  │ m3 - 2^5 / 3^3 │ 294   │
    │ C6  │ II     │ m7p-2 │ 3396  │ E6   │ I      │ 5p-2  │ 3102  │ M3 - 3^4 / 2^6 │ 408   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴────────────────┴───────┘ |}]
;;
