open! Base

type t = { consecutive_intervals : Interval.t array }

let create consecutive_intervals = { consecutive_intervals }

let fold_map_opt t ~init ~f =
  let queue = Queue.create () in
  let rec aux acc i =
    Queue.enqueue queue acc;
    if i < Array.length t
    then (
      match f t.(i) acc with
      | None -> ()
      | Some acc -> aux acc (Int.succ i))
  in
  aux init 0;
  Queue.to_array queue
;;

let ascending t ~from =
  fold_map_opt t.consecutive_intervals ~init:from ~f:Interval.shift_up
;;

let descending t ~from =
  fold_map_opt (t.consecutive_intervals |> Array.rev) ~init:from ~f:Interval.shift_down
;;

let major =
  lazy
    (let ton = Interval.{ number = Second; quality = Major; additional_octaves = 0 } in
     let semiton =
       Interval.{ number = Second; quality = Minor; additional_octaves = 0 }
     in
     create [| ton; ton; semiton; ton; ton; ton; semiton |])
;;

let minor =
  lazy
    (let ton = Interval.{ number = Second; quality = Major; additional_octaves = 0 } in
     let semiton =
       Interval.{ number = Second; quality = Minor; additional_octaves = 0 }
     in
     create [| ton; semiton; ton; ton; semiton; ton; ton |])
;;
