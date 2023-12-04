open Core

type card = { id : int; winning : int list; have : int list }
type input = card list

let parse_line line =
  let id, numbers = String.lsplit2_exn line ~on:':' in
  let id =
    String.chop_prefix_exn id ~prefix:"Card" |> String.strip |> Int.of_string
  in
  let winning, have = String.lsplit2_exn numbers ~on:'|' in
  let parse_numbers s =
    String.split ~on:' ' s |> List.map ~f:String.strip
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  in
  let winning = parse_numbers winning in
  let have = parse_numbers have in
  { id; winning; have }

let input_of_lines lines = List.map lines ~f:parse_line

let num_matching { id = _; winning; have } =
  List.count have ~f:(fun x -> List.mem winning x ~equal:( = ))

let part_1 cards =
  List.map cards ~f:(fun card ->
      let count = num_matching card in
      if count = 0 then 0 else Int.pow 2 (count - 1))
  |> List.fold ~init:0 ~f:( + )

let part_2 cards =
  let counts = List.map cards ~f:num_matching in
  List.fold_right counts ~init:(0, []) ~f:(fun count (total, acc) ->
      let added = 1 + List.fold (List.take acc count) ~init:0 ~f:( + ) in
      let total = total + added in
      let acc = added :: acc in
      (total, acc))
  |> fst
