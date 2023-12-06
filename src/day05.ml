open Core

type map = (int * int * int) list
type input = int list * map list

let input_of_lines lines =
  let seeds =
    List.map
      (List.hd_exn lines
      |> String.chop_prefix_exn ~prefix:"seeds: "
      |> String.split ~on:' ')
      ~f:Int.of_string
  in
  let rec parse_map lines =
    match lines with
    | [] -> []
    | line :: rest -> (
        if String.is_empty line then []
        else
          match String.split line ~on:' ' with
          | [ dest; source; length ] ->
              (Int.of_string dest, Int.of_string source, Int.of_string length)
              :: parse_map rest
          | _ -> failwith "Invalid input")
  in
  let rec parse_maps lines =
    match lines with
    | [] -> []
    | line :: rest ->
        if String.is_substring line ~substring:"map" then
          parse_map rest :: parse_maps rest
        else parse_maps rest
  in
  (seeds, parse_maps lines)

let convert seed map =
  match
    List.find_map map ~f:(fun (dest, source, length) ->
        if source <= seed && seed <= source + length - 1 then
          Some (seed + dest - source)
        else None)
  with
  | None -> seed
  | Some x -> x

let part_1 (seeds, maps) =
  let locations =
    List.map seeds ~f:(fun seed -> List.fold maps ~init:seed ~f:convert)
  in
  List.fold ~init:(List.hd_exn locations) ~f:Int.min locations

let part_2 (seeds, maps) =
  let rec map_pairs list ~f =
    match list with
    | [] -> []
    | [ _ ] -> []
    | x :: y :: rest -> f x y :: map_pairs rest ~f
  in
  let pairs = map_pairs seeds ~f:(fun x y -> (x, y)) in
  let in_any x =
    List.find pairs ~f:(fun (start, length) ->
        start <= x && x <= start + length - 1)
    |> Option.is_some
  in
  let reversed_maps =
    List.map maps
      ~f:(List.map ~f:(fun (dest, source, length) -> (source, dest, length)))
    |> List.rev
  in
  let rec find image =
    let seed = List.fold reversed_maps ~init:image ~f:convert in
    if in_any seed then image else find (image + 1)
  in
  find 0
