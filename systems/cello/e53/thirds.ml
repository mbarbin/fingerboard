open! Core
open! Fingerboard

let make_scale_in_third ~characterized_scale ~from =
  let t = force E53.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale
    ~interval_number:3
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let make_major_just_scale_in_third ~from =
  make_scale_in_third ~characterized_scale:Characterized_scale.major_just_e53 ~from
;;

let make_major_pythagorean_scale_in_third ~from =
  make_scale_in_third ~characterized_scale:Characterized_scale.major_pythagorean_e53 ~from
;;

let%expect_test "c_major_just" =
  let t = force E53.t in
  let scale = make_major_just_scale_in_third ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3z-e53   │ 385   │ G2   │ III    │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ A2   │ III    │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3z-e53   │ 385   │ M3 - 17-53edo │ 385   │
    │ A2  │ IV     │ M6z-e53   │ 883   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ B2  │ III    │ M3z-e53   │ 385   │ D3   │ II     │ 0         │ 0     │ m3 - 14-53edo │ 317   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ E3  │ III    │ M6z-e53   │ 883   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ F3  │ III    │ m7p-e53   │ 996   │ A3   │ II     │ 5z-e53    │ 679   │ M3 - 17-53edo │ 385   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2z-e53   │ 181   │ M3 - 17-53edo │ 385   │
    │ A3  │ II     │ 5z-e53    │ 679   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 14-53edo │ 317   │
    │ B3  │ II     │ M6z-e53   │ 883   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 14-53edo │ 317   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5z-e53    │ 679   │ M3 - 17-53edo │ 385   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6p-e53   │ 792   │ m3 - 13-53edo │ 294   │
    │ E4  │ II     │ M2z-e53-1 │ 1381  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 14-53edo │ 317   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ A4   │ I      │ 8z-e53    │ 1177  │ M3 - 17-53edo │ 385   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2z-e53-1 │ 1381  │ M3 - 17-53edo │ 385   │
    │ A4  │ II     │ 5z-e53-1  │ 1879  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 14-53edo │ 317   │
    │ B4  │ II     │ M6z-e53-1 │ 2083  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 14-53edo │ 317   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5z-e53-1  │ 1879  │ M3 - 17-53edo │ 385   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m3 - 13-53edo │ 294   │
    │ E5  │ II     │ M2z-e53-2 │ 2581  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 14-53edo │ 317   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ A5   │ I      │ 8z-e53-1  │ 2377  │ M3 - 17-53edo │ 385   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2z-e53-2 │ 2581  │ M3 - 17-53edo │ 385   │
    │ A5  │ II     │ 5z-e53-2  │ 3079  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 14-53edo │ 317   │
    │ B5  │ II     │ M6z-e53-2 │ 3283  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 14-53edo │ 317   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5z-e53-2  │ 3079  │ M3 - 17-53edo │ 385   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;

let%expect_test "c_major_pythagorean" =
  let t = force E53.t in
  let scale = make_major_pythagorean_scale_in_third ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────────┬───────┬──────┬────────┬───────────┬───────┬───────────────┬───────┐
    │ Low │ String │ Pos       │ Cents │ High │ String │ Pos       │ Cents │ Interval      │ Cents │
    ├─────┼────────┼───────────┼───────┼──────┼────────┼───────────┼───────┼───────────────┼───────┤
    │ E2  │ IV     │ M3p-e53   │ 408   │ G2   │ III    │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ F2  │ IV     │ 4p-e53    │ 498   │ A2   │ III    │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ G2  │ IV     │ 5p-e53    │ 702   │ B2   │ III    │ M3p-e53   │ 408   │ M3 - 18-53edo │ 408   │
    │ A2  │ IV     │ M6p-e53   │ 906   │ C3   │ III    │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ B2  │ III    │ M3p-e53   │ 408   │ D3   │ II     │ 0         │ 0     │ m3 - 13-53edo │ 294   │
    │ C3  │ III    │ 4p-e53    │ 498   │ E3   │ II     │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ D3  │ III    │ 5p-e53    │ 702   │ F3   │ II     │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ E3  │ III    │ M6p-e53   │ 906   │ G3   │ II     │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ F3  │ II     │ m3p-e53   │ 294   │ A3   │ I      │ 0         │ 0     │ M3 - 18-53edo │ 408   │
    │ G3  │ II     │ 4p-e53    │ 498   │ B3   │ I      │ M2p-e53   │ 204   │ M3 - 18-53edo │ 408   │
    │ A3  │ II     │ 5p-e53    │ 702   │ C4   │ I      │ m3p-e53   │ 294   │ m3 - 13-53edo │ 294   │
    │ B3  │ II     │ M6p-e53   │ 906   │ D4   │ I      │ 4p-e53    │ 498   │ m3 - 13-53edo │ 294   │
    │ C4  │ II     │ m7p-e53   │ 996   │ E4   │ I      │ 5p-e53    │ 702   │ M3 - 18-53edo │ 408   │
    │ D4  │ II     │ 0-1       │ 1200  │ F4   │ I      │ m6p-e53   │ 792   │ m3 - 13-53edo │ 294   │
    │ E4  │ II     │ M2p-e53-1 │ 1404  │ G4   │ I      │ m7p-e53   │ 996   │ m3 - 13-53edo │ 294   │
    │ F4  │ II     │ m3p-e53-1 │ 1494  │ A4   │ I      │ 0-1       │ 1200  │ M3 - 18-53edo │ 408   │
    │ G4  │ II     │ 4p-e53-1  │ 1698  │ B4   │ I      │ M2p-e53-1 │ 1404  │ M3 - 18-53edo │ 408   │
    │ A4  │ II     │ 5p-e53-1  │ 1902  │ C5   │ I      │ m3p-e53-1 │ 1494  │ m3 - 13-53edo │ 294   │
    │ B4  │ II     │ M6p-e53-1 │ 2106  │ D5   │ I      │ 4p-e53-1  │ 1698  │ m3 - 13-53edo │ 294   │
    │ C5  │ II     │ m7p-e53-1 │ 2196  │ E5   │ I      │ 5p-e53-1  │ 1902  │ M3 - 18-53edo │ 408   │
    │ D5  │ II     │ 0-2       │ 2400  │ F5   │ I      │ m6p-e53-1 │ 1992  │ m3 - 13-53edo │ 294   │
    │ E5  │ II     │ M2p-e53-2 │ 2604  │ G5   │ I      │ m7p-e53-1 │ 2196  │ m3 - 13-53edo │ 294   │
    │ F5  │ II     │ m3p-e53-2 │ 2694  │ A5   │ I      │ 0-2       │ 2400  │ M3 - 18-53edo │ 408   │
    │ G5  │ II     │ 4p-e53-2  │ 2898  │ B5   │ I      │ M2p-e53-2 │ 2604  │ M3 - 18-53edo │ 408   │
    │ A5  │ II     │ 5p-e53-2  │ 3102  │ C6   │ I      │ m3p-e53-2 │ 2694  │ m3 - 13-53edo │ 294   │
    │ B5  │ II     │ M6p-e53-2 │ 3306  │ D6   │ I      │ 4p-e53-2  │ 2898  │ m3 - 13-53edo │ 294   │
    │ C6  │ II     │ m7p-e53-2 │ 3396  │ E6   │ I      │ 5p-e53-2  │ 3102  │ M3 - 18-53edo │ 408   │
    └─────┴────────┴───────────┴───────┴──────┴────────┴───────────┴───────┴───────────────┴───────┘ |}]
;;
