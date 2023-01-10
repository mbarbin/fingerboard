open! Core
open! Fingerboard
open! Core
open! Fingerboard

let make_major_scale ~from =
  let t = force E12.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale:Characterized_scale.major_e12
    ~interval_number:Sixth
    ~from
    ~to_:Cello.fingerboard_highest_note
;;

let%expect_test "c_major" =
  let t = force E12.t in
  let scale = make_major_scale ~from:Scales.lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ C2  │ IV     │ 0     │ 0     │ A2   │ III    │ M2e   │ 200   │ M6 - 9-12edo │ 900   │
    │ D2  │ IV     │ M2e   │ 200   │ B2   │ III    │ M3e   │ 400   │ M6 - 9-12edo │ 900   │
    │ E2  │ IV     │ M3e   │ 400   │ C3   │ III    │ 4e    │ 500   │ m6 - 8-12edo │ 800   │
    │ F2  │ IV     │ 4e    │ 500   │ D3   │ III    │ 5e    │ 700   │ M6 - 9-12edo │ 900   │
    │ G2  │ III    │ 0     │ 0     │ E3   │ II     │ M2e   │ 200   │ M6 - 9-12edo │ 900   │
    │ A2  │ III    │ M2e   │ 200   │ F3   │ II     │ m3e   │ 300   │ m6 - 8-12edo │ 800   │
    │ B2  │ III    │ M3e   │ 400   │ G3   │ II     │ 4e    │ 500   │ m6 - 8-12edo │ 800   │
    │ C3  │ III    │ 4e    │ 500   │ A3   │ II     │ 5e    │ 700   │ M6 - 9-12edo │ 900   │
    │ D3  │ II     │ 0     │ 0     │ B3   │ I      │ M2e   │ 200   │ M6 - 9-12edo │ 900   │
    │ E3  │ II     │ M2e   │ 200   │ C4   │ I      │ m3e   │ 300   │ m6 - 8-12edo │ 800   │
    │ F3  │ II     │ m3e   │ 300   │ D4   │ I      │ 4e    │ 500   │ M6 - 9-12edo │ 900   │
    │ G3  │ II     │ 4e    │ 500   │ E4   │ I      │ 5e    │ 700   │ M6 - 9-12edo │ 900   │
    │ A3  │ II     │ 5e    │ 700   │ F4   │ I      │ m6e   │ 800   │ m6 - 8-12edo │ 800   │
    │ B3  │ II     │ M6e   │ 900   │ G4   │ I      │ m7e   │ 1000  │ m6 - 8-12edo │ 800   │
    │ C4  │ II     │ m7e   │ 1000  │ A4   │ I      │ 0-1   │ 1200  │ M6 - 9-12edo │ 900   │
    │ D4  │ II     │ 0-1   │ 1200  │ B4   │ I      │ M2e-1 │ 1400  │ M6 - 9-12edo │ 900   │
    │ E4  │ II     │ M2e-1 │ 1400  │ C5   │ I      │ m3e-1 │ 1500  │ m6 - 8-12edo │ 800   │
    │ F4  │ II     │ m3e-1 │ 1500  │ D5   │ I      │ 4e-1  │ 1700  │ M6 - 9-12edo │ 900   │
    │ G4  │ II     │ 4e-1  │ 1700  │ E5   │ I      │ 5e-1  │ 1900  │ M6 - 9-12edo │ 900   │
    │ A4  │ II     │ 5e-1  │ 1900  │ F5   │ I      │ m6e-1 │ 2000  │ m6 - 8-12edo │ 800   │
    │ B4  │ II     │ M6e-1 │ 2100  │ G5   │ I      │ m7e-1 │ 2200  │ m6 - 8-12edo │ 800   │
    │ C5  │ II     │ m7e-1 │ 2200  │ A5   │ I      │ 0-2   │ 2400  │ M6 - 9-12edo │ 900   │
    │ D5  │ II     │ 0-2   │ 2400  │ B5   │ I      │ M2e-2 │ 2600  │ M6 - 9-12edo │ 900   │
    │ E5  │ II     │ M2e-2 │ 2600  │ C6   │ I      │ m3e-2 │ 2700  │ m6 - 8-12edo │ 800   │
    │ F5  │ II     │ m3e-2 │ 2700  │ D6   │ I      │ 4e-2  │ 2900  │ M6 - 9-12edo │ 900   │
    │ G5  │ II     │ 4e-2  │ 2900  │ E6   │ I      │ 5e-2  │ 3100  │ M6 - 9-12edo │ 900   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;

let%expect_test "e_flat_major" =
  let t = force E12.t in
  let scale = make_major_scale ~from:Scales.lower_e_flat in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ Eb2 │ IV     │ m3e   │ 300   │ C3   │ III    │ 4e    │ 500   │ M6 - 9-12edo │ 900   │
    │ F2  │ IV     │ 4e    │ 500   │ D3   │ III    │ 5e    │ 700   │ M6 - 9-12edo │ 900   │
    │ G2  │ III    │ 0     │ 0     │ Eb3  │ II     │ m2e   │ 100   │ m6 - 8-12edo │ 800   │
    │ Ab2 │ III    │ m2e   │ 100   │ F3   │ II     │ m3e   │ 300   │ M6 - 9-12edo │ 900   │
    │ Bb2 │ III    │ m3e   │ 300   │ G3   │ II     │ 4e    │ 500   │ M6 - 9-12edo │ 900   │
    │ C3  │ III    │ 4e    │ 500   │ Ab3  │ II     │ A4e   │ 600   │ m6 - 8-12edo │ 800   │
    │ D3  │ II     │ 0     │ 0     │ Bb3  │ I      │ m2e   │ 100   │ m6 - 8-12edo │ 800   │
    │ Eb3 │ II     │ m2e   │ 100   │ C4   │ I      │ m3e   │ 300   │ M6 - 9-12edo │ 900   │
    │ F3  │ II     │ m3e   │ 300   │ D4   │ I      │ 4e    │ 500   │ M6 - 9-12edo │ 900   │
    │ G3  │ II     │ 4e    │ 500   │ Eb4  │ I      │ A4e   │ 600   │ m6 - 8-12edo │ 800   │
    │ Ab3 │ II     │ A4e   │ 600   │ F4   │ I      │ m6e   │ 800   │ M6 - 9-12edo │ 900   │
    │ Bb3 │ II     │ m6e   │ 800   │ G4   │ I      │ m7e   │ 1000  │ M6 - 9-12edo │ 900   │
    │ C4  │ II     │ m7e   │ 1000  │ Ab4  │ I      │ M7e   │ 1100  │ m6 - 8-12edo │ 800   │
    │ D4  │ II     │ 0-1   │ 1200  │ Bb4  │ I      │ m2e-1 │ 1300  │ m6 - 8-12edo │ 800   │
    │ Eb4 │ II     │ m2e-1 │ 1300  │ C5   │ I      │ m3e-1 │ 1500  │ M6 - 9-12edo │ 900   │
    │ F4  │ II     │ m3e-1 │ 1500  │ D5   │ I      │ 4e-1  │ 1700  │ M6 - 9-12edo │ 900   │
    │ G4  │ II     │ 4e-1  │ 1700  │ Eb5  │ I      │ A4e-1 │ 1800  │ m6 - 8-12edo │ 800   │
    │ Ab4 │ II     │ A4e-1 │ 1800  │ F5   │ I      │ m6e-1 │ 2000  │ M6 - 9-12edo │ 900   │
    │ Bb4 │ II     │ m6e-1 │ 2000  │ G5   │ I      │ m7e-1 │ 2200  │ M6 - 9-12edo │ 900   │
    │ C5  │ II     │ m7e-1 │ 2200  │ Ab5  │ I      │ M7e-1 │ 2300  │ m6 - 8-12edo │ 800   │
    │ D5  │ II     │ 0-2   │ 2400  │ Bb5  │ I      │ m2e-2 │ 2500  │ m6 - 8-12edo │ 800   │
    │ Eb5 │ II     │ m2e-2 │ 2500  │ C6   │ I      │ m3e-2 │ 2700  │ M6 - 9-12edo │ 900   │
    │ F5  │ II     │ m3e-2 │ 2700  │ D6   │ I      │ 4e-2  │ 2900  │ M6 - 9-12edo │ 900   │
    │ G5  │ II     │ 4e-2  │ 2900  │ Eb6  │ I      │ A4e-2 │ 3000  │ m6 - 8-12edo │ 800   │
    │ Ab5 │ II     │ A4e-2 │ 3000  │ F6   │ I      │ m6e-2 │ 3200  │ M6 - 9-12edo │ 900   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;
