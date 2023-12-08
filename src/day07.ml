open Core

type input = (string * int) list

let input_of_lines lines =
  let parse line =
    let hand, bid = String.lsplit2_exn line ~on:' ' in
    (hand, Int.of_string bid)
  in
  List.map lines ~f:parse

let strength card =
  let order = "23456789TJQKA" in
  String.index_exn order card

let rank hand =
  String.fold hand ~init:Char.Map.empty
    ~f:(Map.update ~f:(function None -> 1 | Some x -> x + 1))
  |> Map.data
  |> List.sort ~compare:Int.compare
  |> List.rev

let rec compare_lists a b =
  match (a, b) with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | x :: xs, y :: ys ->
      let cmp = Int.compare x y in
      if cmp = 0 then compare_lists xs ys else cmp

let compare_ranks a b = compare_lists (rank a) (rank b)

let compare_hands a b =
  let cmp = compare_ranks a b in
  if cmp <> 0 then cmp
  else
    List.map
      (List.zip_exn (String.to_list a) (String.to_list b))
      ~f:(fun (x, y) -> strength x - strength y)
    |> List.find ~f:(fun x -> x <> 0)
    |> Option.value ~default:0

let part_1 hands =
  let ordered =
    List.sort hands ~compare:(fun (hand1, _) (hand2, _) ->
        compare_hands hand1 hand2)
  in
  List.foldi ordered ~init:0 ~f:(fun idx acc (_, bid) ->
      acc + ((idx + 1) * bid))

let compare_hands_j a b =
  let rank_j hand =
    let r = rank (String.filter hand ~f:(fun c -> Char.( <> ) c 'J')) in
    Option.value (List.hd r) ~default:0
    + String.count hand ~f:(fun c -> Char.( = ) c 'J')
    :: Option.value (List.tl r) ~default:[]
  in
  let cmp = compare_lists (rank_j a) (rank_j b) in
  if cmp <> 0 then cmp
  else
    let strength_j card =
      let order = "J23456789TQKA" in
      String.index_exn order card
    in
    List.map
      (List.zip_exn (String.to_list a) (String.to_list b))
      ~f:(fun (x, y) -> strength_j x - strength_j y)
    |> List.find ~f:(fun x -> x <> 0)
    |> Option.value ~default:0

let part_2 hands =
  let ordered =
    List.sort hands ~compare:(fun (hand1, _) (hand2, _) ->
        compare_hands_j hand1 hand2)
  in
  List.foldi ordered ~init:0 ~f:(fun idx acc (_, bid) ->
      acc + ((idx + 1) * bid))
