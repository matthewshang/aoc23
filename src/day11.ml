open Core

type input = {
  galaxies : (int * int) list;
  empty_rows : int list;
  empty_cols : int list;
}

let input_of_lines lines =
  let galaxies, empty_rows =
    List.foldi lines ~init:([], []) ~f:(fun row (galaxies, empty_rows) line ->
        let galaxies =
          String.foldi line ~init:galaxies ~f:(fun col galaxies c ->
              match c with '#' -> (row, col) :: galaxies | _ -> galaxies)
        in
        let empty_rows =
          if String.contains line '#' then empty_rows else row :: empty_rows
        in
        (galaxies, empty_rows))
  in
  let cols = String.length (List.hd_exn lines) in
  let empty_cols =
    List.filter (List.init cols ~f:Fn.id) ~f:(fun col ->
        List.for_all lines ~f:(fun line -> Char.( = ) line.[col] '.'))
  in
  { galaxies; empty_rows; empty_cols }

let calc { galaxies; empty_rows; empty_cols } ~scale =
  let dist (ra, ca) (rb, cb) =
    let ra, rb = if ra > rb then (rb, ra) else (ra, rb) in
    let ca, cb = if ca > cb then (cb, ca) else (ca, cb) in
    let expand_rows = List.count empty_rows ~f:(fun r -> ra < r && r < rb) in
    let expand_cols = List.count empty_cols ~f:(fun c -> ca < c && c < cb) in
    rb - ra + cb - ca + ((scale - 1) * expand_rows) + ((scale - 1) * expand_cols)
  in
  let rec loop galaxies =
    match galaxies with
    | [] -> 0
    | head :: rest ->
        List.fold rest ~init:0 ~f:(fun acc other -> acc + dist head other)
        + loop rest
  in
  loop galaxies

let part_1 grid = calc grid ~scale:2
let part_2 grid = calc grid ~scale:1000000
