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
    ┌─────┬──────┐
    │ Low │ High │
    ├─────┼──────┤
    │ C2  │ E2   │
    │ D2  │ F2   │
    │ E2  │ G2   │
    │ F2  │ A2   │
    │ G2  │ B2   │
    │ A2  │ C3   │
    │ B2  │ D3   │
    │ C3  │ E3   │
    │ D3  │ F3   │
    │ E3  │ G3   │
    │ F3  │ A3   │
    │ G3  │ B3   │
    │ A3  │ C4   │
    │ B3  │ D4   │
    │ C4  │ E4   │
    │ D4  │ F4   │
    │ E4  │ G4   │
    │ F4  │ A4   │
    │ G4  │ B4   │
    │ A4  │ C5   │
    │ B4  │ D5   │
    │ C5  │ E5   │
    │ D5  │ F5   │
    │ E5  │ G5   │
    │ F5  │ A5   │
    │ G5  │ B5   │
    │ A5  │ C6   │
    │ B5  │ D6   │
    │ C6  │ E6   │
    └─────┴──────┘ |}]
;;
