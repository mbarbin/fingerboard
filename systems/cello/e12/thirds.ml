open! Core
open! Fingerboard

let make_major_scale ~from =
  let t = force E12.t in
  System.Double_stops.make_scale
    t
    ~characterized_scale:Characterized_scale.major_e12
    ~interval_number:Third
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
    │ E2  │ IV     │ M3e   │ 400   │ G2   │ III    │ 0     │ 0     │ m3 - 3-12edo │ 300   │
    │ F2  │ IV     │ 4e    │ 500   │ A2   │ III    │ M2e   │ 200   │ M3 - 4-12edo │ 400   │
    │ G2  │ IV     │ 5e    │ 700   │ B2   │ III    │ M3e   │ 400   │ M3 - 4-12edo │ 400   │
    │ A2  │ IV     │ M6e   │ 900   │ C3   │ III    │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ B2  │ III    │ M3e   │ 400   │ D3   │ II     │ 0     │ 0     │ m3 - 3-12edo │ 300   │
    │ C3  │ III    │ 4e    │ 500   │ E3   │ II     │ M2e   │ 200   │ M3 - 4-12edo │ 400   │
    │ D3  │ III    │ 5e    │ 700   │ F3   │ II     │ m3e   │ 300   │ m3 - 3-12edo │ 300   │
    │ E3  │ III    │ M6e   │ 900   │ G3   │ II     │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ F3  │ II     │ m3e   │ 300   │ A3   │ I      │ 0     │ 0     │ M3 - 4-12edo │ 400   │
    │ G3  │ II     │ 4e    │ 500   │ B3   │ I      │ M2e   │ 200   │ M3 - 4-12edo │ 400   │
    │ A3  │ II     │ 5e    │ 700   │ C4   │ I      │ m3e   │ 300   │ m3 - 3-12edo │ 300   │
    │ B3  │ II     │ M6e   │ 900   │ D4   │ I      │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ C4  │ II     │ m7e   │ 1000  │ E4   │ I      │ 5e    │ 700   │ M3 - 4-12edo │ 400   │
    │ D4  │ II     │ 0-1   │ 1200  │ F4   │ I      │ m6e   │ 800   │ m3 - 3-12edo │ 300   │
    │ E4  │ II     │ M2e-1 │ 1400  │ G4   │ I      │ m7e   │ 1000  │ m3 - 3-12edo │ 300   │
    │ F4  │ II     │ m3e-1 │ 1500  │ A4   │ I      │ 0-1   │ 1200  │ M3 - 4-12edo │ 400   │
    │ G4  │ II     │ 4e-1  │ 1700  │ B4   │ I      │ M2e-1 │ 1400  │ M3 - 4-12edo │ 400   │
    │ A4  │ II     │ 5e-1  │ 1900  │ C5   │ I      │ m3e-1 │ 1500  │ m3 - 3-12edo │ 300   │
    │ B4  │ II     │ M6e-1 │ 2100  │ D5   │ I      │ 4e-1  │ 1700  │ m3 - 3-12edo │ 300   │
    │ C5  │ II     │ m7e-1 │ 2200  │ E5   │ I      │ 5e-1  │ 1900  │ M3 - 4-12edo │ 400   │
    │ D5  │ II     │ 0-2   │ 2400  │ F5   │ I      │ m6e-1 │ 2000  │ m3 - 3-12edo │ 300   │
    │ E5  │ II     │ M2e-2 │ 2600  │ G5   │ I      │ m7e-1 │ 2200  │ m3 - 3-12edo │ 300   │
    │ F5  │ II     │ m3e-2 │ 2700  │ A5   │ I      │ 0-2   │ 2400  │ M3 - 4-12edo │ 400   │
    │ G5  │ II     │ 4e-2  │ 2900  │ B5   │ I      │ M2e-2 │ 2600  │ M3 - 4-12edo │ 400   │
    │ A5  │ II     │ 5e-2  │ 3100  │ C6   │ I      │ m3e-2 │ 2700  │ m3 - 3-12edo │ 300   │
    │ B5  │ II     │ M6e-2 │ 3300  │ D6   │ I      │ 4e-2  │ 2900  │ m3 - 3-12edo │ 300   │
    │ C6  │ II     │ m7e-2 │ 3400  │ E6   │ I      │ 5e-2  │ 3100  │ M3 - 4-12edo │ 400   │
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
    │ Eb2 │ IV     │ m3e   │ 300   │ G2   │ III    │ 0     │ 0     │ M3 - 4-12edo │ 400   │
    │ F2  │ IV     │ 4e    │ 500   │ Ab2  │ III    │ m2e   │ 100   │ m3 - 3-12edo │ 300   │
    │ G2  │ IV     │ 5e    │ 700   │ Bb2  │ III    │ m3e   │ 300   │ m3 - 3-12edo │ 300   │
    │ Ab2 │ IV     │ m6e   │ 800   │ C3   │ III    │ 4e    │ 500   │ M3 - 4-12edo │ 400   │
    │ Bb2 │ III    │ m3e   │ 300   │ D3   │ II     │ 0     │ 0     │ M3 - 4-12edo │ 400   │
    │ C3  │ III    │ 4e    │ 500   │ Eb3  │ II     │ m2e   │ 100   │ m3 - 3-12edo │ 300   │
    │ D3  │ III    │ 5e    │ 700   │ F3   │ II     │ m3e   │ 300   │ m3 - 3-12edo │ 300   │
    │ Eb3 │ III    │ m6e   │ 800   │ G3   │ II     │ 4e    │ 500   │ M3 - 4-12edo │ 400   │
    │ F3  │ III    │ m7e   │ 1000  │ Ab3  │ II     │ A4e   │ 600   │ m3 - 3-12edo │ 300   │
    │ G3  │ II     │ 4e    │ 500   │ Bb3  │ I      │ m2e   │ 100   │ m3 - 3-12edo │ 300   │
    │ Ab3 │ II     │ A4e   │ 600   │ C4   │ I      │ m3e   │ 300   │ M3 - 4-12edo │ 400   │
    │ Bb3 │ II     │ m6e   │ 800   │ D4   │ I      │ 4e    │ 500   │ M3 - 4-12edo │ 400   │
    │ C4  │ II     │ m7e   │ 1000  │ Eb4  │ I      │ A4e   │ 600   │ m3 - 3-12edo │ 300   │
    │ D4  │ II     │ 0-1   │ 1200  │ F4   │ I      │ m6e   │ 800   │ m3 - 3-12edo │ 300   │
    │ Eb4 │ II     │ m2e-1 │ 1300  │ G4   │ I      │ m7e   │ 1000  │ M3 - 4-12edo │ 400   │
    │ F4  │ II     │ m3e-1 │ 1500  │ Ab4  │ I      │ M7e   │ 1100  │ m3 - 3-12edo │ 300   │
    │ G4  │ II     │ 4e-1  │ 1700  │ Bb4  │ I      │ m2e-1 │ 1300  │ m3 - 3-12edo │ 300   │
    │ Ab4 │ II     │ A4e-1 │ 1800  │ C5   │ I      │ m3e-1 │ 1500  │ M3 - 4-12edo │ 400   │
    │ Bb4 │ II     │ m6e-1 │ 2000  │ D5   │ I      │ 4e-1  │ 1700  │ M3 - 4-12edo │ 400   │
    │ C5  │ II     │ m7e-1 │ 2200  │ Eb5  │ I      │ A4e-1 │ 1800  │ m3 - 3-12edo │ 300   │
    │ D5  │ II     │ 0-2   │ 2400  │ F5   │ I      │ m6e-1 │ 2000  │ m3 - 3-12edo │ 300   │
    │ Eb5 │ II     │ m2e-2 │ 2500  │ G5   │ I      │ m7e-1 │ 2200  │ M3 - 4-12edo │ 400   │
    │ F5  │ II     │ m3e-2 │ 2700  │ Ab5  │ I      │ M7e-1 │ 2300  │ m3 - 3-12edo │ 300   │
    │ G5  │ II     │ 4e-2  │ 2900  │ Bb5  │ I      │ m2e-2 │ 2500  │ m3 - 3-12edo │ 300   │
    │ Ab5 │ II     │ A4e-2 │ 3000  │ C6   │ I      │ m3e-2 │ 2700  │ M3 - 4-12edo │ 400   │
    │ Bb5 │ II     │ m6e-2 │ 3200  │ D6   │ I      │ 4e-2  │ 2900  │ M3 - 4-12edo │ 400   │
    │ C6  │ II     │ m7e-2 │ 3400  │ Eb6  │ I      │ A4e-2 │ 3000  │ m3 - 3-12edo │ 300   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;
