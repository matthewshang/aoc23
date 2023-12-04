open Core

type color = Red | Green | Blue
type game = { id : int; subsets : (int * color) list list }
type input = game list

let parse_subset subset =
  String.split subset ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:(fun s ->
         let num, color = String.lsplit2_exn s ~on:' ' in
         let num = Int.of_string num in
         let color =
           match color with
           | "red" -> Red
           | "green" -> Green
           | "blue" -> Blue
           | _ -> failwith "invalid color"
         in
         (num, color))

let parse_line line =
  let id, subsets = String.lsplit2_exn line ~on:':' in
  let id = String.lsplit2_exn id ~on:' ' |> snd |> Int.of_string in
  let subsets = String.split subsets ~on:';' |> List.map ~f:parse_subset in
  { id; subsets }

let input_of_lines lines = List.map lines ~f:parse_line

let part_1 games =
  let limit = (12, 13, 14) in
  List.filter games ~f:(fun { id = _; subsets } ->
      List.for_all subsets ~f:(fun subset ->
          let r, g, b =
            List.fold subset ~init:(0, 0, 0) ~f:(fun (r, g, b) (num, color) ->
                match color with
                | Red -> (max r num, g, b)
                | Green -> (r, max g num, b)
                | Blue -> (r, g, max b num))
          in
          r <= fst3 limit && g <= snd3 limit && b <= trd3 limit))
  |> List.map ~f:(fun { id; subsets = _ } -> id)
  |> List.fold ~init:0 ~f:( + )

let part_2 games =
  List.map games ~f:(fun { id = _; subsets } ->
      let r, g, b =
        List.fold (List.concat subsets) ~init:(0, 0, 0)
          ~f:(fun (r, g, b) (num, color) ->
            match color with
            | Red -> (max r num, g, b)
            | Green -> (r, max g num, b)
            | Blue -> (r, g, max b num))
      in
      r * g * b)
  |> List.fold ~init:0 ~f:( + )
