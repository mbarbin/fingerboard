open! Core
open! Fingerboard

let make_scale t ~characterized_scale ~from =
  System.make_scale t ~characterized_scale ~from ~to_:Cello.fingerboard_highest_note
;;

let make_major_scale_in_third ~from =
  let t = force E12.t in
  let scale = make_scale t ~characterized_scale:Characterized_scale.major_e12 ~from in
  let double_stops =
    let rec aux acc = function
      | a :: (_ :: c :: _ as tl) ->
        aux (Double_stop.{ low_note = a; high_note = c } :: acc) tl
      | _ -> List.rev acc
    in
    aux [] scale
  in
  double_stops
;;

let lower_c =
  let t = force E12.t in
  System.open_string t IV |> Option.value_exn ~here:[%here]
;;

let%expect_test "c_major" =
  let t = force E12.t in
  let scale = make_major_scale_in_third ~from:lower_c in
  print_endline (System.Double_stops.to_ascii_table t scale);
  [%expect
    {|
    ┌─────┬────────┬───────┬───────┬──────┬────────┬───────┬───────┬──────────────┬───────┐
    │ Low │ String │ Pos   │ Cents │ High │ String │ Pos   │ Cents │ Interval     │ Cents │
    ├─────┼────────┼───────┼───────┼──────┼────────┼───────┼───────┼──────────────┼───────┤
    │ C2  │ IV     │ 0     │ 0     │ E2   │ IV     │ M3e   │ 400   │ M3 - 4-12edo │ 400   │
    │ D2  │ IV     │ M2e   │ 200   │ F2   │ IV     │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ E2  │ IV     │ M3e   │ 400   │ G2   │ III    │ 0     │ 0     │ m3 - 3-12edo │ 300   │
    │ F2  │ IV     │ 4e    │ 500   │ A2   │ III    │ M2e   │ 200   │ M3 - 4-12edo │ 400   │
    │ G2  │ III    │ 0     │ 0     │ B2   │ III    │ M3e   │ 400   │ M3 - 4-12edo │ 400   │
    │ A2  │ III    │ M2e   │ 200   │ C3   │ III    │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ B2  │ III    │ M3e   │ 400   │ D3   │ II     │ 0     │ 0     │ m3 - 3-12edo │ 300   │
    │ C3  │ III    │ 4e    │ 500   │ E3   │ II     │ M2e   │ 200   │ M3 - 4-12edo │ 400   │
    │ D3  │ II     │ 0     │ 0     │ F3   │ II     │ m3e   │ 300   │ m3 - 3-12edo │ 300   │
    │ E3  │ II     │ M2e   │ 200   │ G3   │ II     │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ F3  │ II     │ m3e   │ 300   │ A3   │ I      │ 0     │ 0     │ M3 - 4-12edo │ 400   │
    │ G3  │ II     │ 4e    │ 500   │ B3   │ I      │ M2e   │ 200   │ M3 - 4-12edo │ 400   │
    │ A3  │ I      │ 0     │ 0     │ C4   │ I      │ m3e   │ 300   │ m3 - 3-12edo │ 300   │
    │ B3  │ I      │ M2e   │ 200   │ D4   │ I      │ 4e    │ 500   │ m3 - 3-12edo │ 300   │
    │ C4  │ I      │ m3e   │ 300   │ E4   │ I      │ 5e    │ 700   │ M3 - 4-12edo │ 400   │
    │ D4  │ I      │ 4e    │ 500   │ F4   │ I      │ m6e   │ 800   │ m3 - 3-12edo │ 300   │
    │ E4  │ I      │ 5e    │ 700   │ G4   │ I      │ m7e   │ 1000  │ m3 - 3-12edo │ 300   │
    │ F4  │ I      │ m6e   │ 800   │ A4   │ I      │ 0-1   │ 1200  │ M3 - 4-12edo │ 400   │
    │ G4  │ I      │ m7e   │ 1000  │ B4   │ I      │ M2e-1 │ 1400  │ M3 - 4-12edo │ 400   │
    │ A4  │ I      │ 0-1   │ 1200  │ C5   │ I      │ m3e-1 │ 1500  │ m3 - 3-12edo │ 300   │
    │ B4  │ I      │ M2e-1 │ 1400  │ D5   │ I      │ 4e-1  │ 1700  │ m3 - 3-12edo │ 300   │
    │ C5  │ I      │ m3e-1 │ 1500  │ E5   │ I      │ 5e-1  │ 1900  │ M3 - 4-12edo │ 400   │
    │ D5  │ I      │ 4e-1  │ 1700  │ F5   │ I      │ m6e-1 │ 2000  │ m3 - 3-12edo │ 300   │
    │ E5  │ I      │ 5e-1  │ 1900  │ G5   │ I      │ m7e-1 │ 2200  │ m3 - 3-12edo │ 300   │
    │ F5  │ I      │ m6e-1 │ 2000  │ A5   │ I      │ 0-2   │ 2400  │ M3 - 4-12edo │ 400   │
    │ G5  │ I      │ m7e-1 │ 2200  │ B5   │ I      │ M2e-2 │ 2600  │ M3 - 4-12edo │ 400   │
    │ A5  │ I      │ 0-2   │ 2400  │ C6   │ I      │ m3e-2 │ 2700  │ m3 - 3-12edo │ 300   │
    │ B5  │ I      │ M2e-2 │ 2600  │ D6   │ I      │ 4e-2  │ 2900  │ m3 - 3-12edo │ 300   │
    │ C6  │ I      │ m3e-2 │ 2700  │ E6   │ I      │ 5e-2  │ 3100  │ M3 - 4-12edo │ 400   │
    └─────┴────────┴───────┴───────┴──────┴────────┴───────┴───────┴──────────────┴───────┘ |}]
;;
