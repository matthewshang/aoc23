open Core

type input = int list list

let input_of_lines lines =
  List.map lines ~f:(fun line ->
      String.split line ~on:' ' |> List.map ~f:Int.of_string)

let differences history =
  List.map2_exn (List.tl_exn history) (List.drop_last_exn history) ~f:Int.( - )

let part_1 ls =
  let rec value history =
    if List.for_all history ~f:(fun x -> x = 0) then 0
    else List.last_exn history + value (differences history)
  in
  List.map ls ~f:value |> List.fold ~init:0 ~f:( + )

let part_2 ls =
  let rec value history =
    if List.for_all history ~f:(fun x -> x = 0) then 0
    else List.hd_exn history - value (differences history)
  in
  List.map ls ~f:value |> List.fold ~init:0 ~f:( + )
