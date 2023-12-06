open Core

type input = (int * int) list

let input_of_lines lines =
  let parse_ints line =
    String.strip line |> String.split ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  in
  let times =
    String.chop_prefix_exn ~prefix:"Time: " (List.nth_exn lines 0) |> parse_ints
  in
  let distances =
    String.chop_prefix_exn ~prefix:"Distance: " (List.nth_exn lines 1)
    |> parse_ints
  in
  List.zip_exn times distances

let ways_to_win time distance =
  List.init (time + 1) ~f:(fun t -> (time - t) * t)
  |> List.count ~f:(fun d -> d > distance)

let part_1 games =
  List.map games ~f:(fun (time, distance) -> ways_to_win time distance)
  |> List.fold ~init:1 ~f:( * )

let part_2 games =
  let int_length x = Float.to_int (Float.log10 (Float.of_int x)) + 1 in
  let time, distance =
    List.fold_left games ~init:(0, 0)
      ~f:(fun (concat_time, concat_dist) (time, distance) ->
        ( (concat_time * Int.pow 10 (int_length time)) + time,
          (concat_dist * Int.pow 10 (int_length distance)) + distance ))
  in
  ways_to_win time distance
