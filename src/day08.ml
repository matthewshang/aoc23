open Core

type direction = Left | Right [@@deriving sexp]
type input = direction list * (string * string) String.Map.t

let input_of_lines lines =
  let instructions, nodes = List.split_n lines 2 in
  let instructions =
    List.map
      (String.to_list (List.hd_exn instructions))
      ~f:(function
        | 'L' -> Left | 'R' -> Right | _ -> failwith "invalid direction")
  in
  let nodes =
    List.map nodes ~f:(fun node ->
        let name, children = String.lsplit2_exn node ~on:'=' in
        let name = String.strip name in
        let left, right = String.lsplit2_exn children ~on:',' in
        let left = String.chop_prefix_exn (String.strip left) ~prefix:"(" in
        let right = String.chop_suffix_exn (String.strip right) ~suffix:")" in
        (name, (left, right)))
  in
  (instructions, String.Map.of_alist_exn nodes)

let part_1 (instructions, nodes) =
  let rec walk instruction = function
    | "ZZZ" -> 0
    | name -> (
        let left, right = Map.find_exn nodes name in
        let next, move =
          match instruction with
          | [] -> (List.tl_exn instructions, List.hd_exn instructions)
          | move :: rest -> (rest, move)
        in
        match move with
        | Left -> 1 + walk next left
        | Right -> 1 + walk next right)
  in
  walk [] "AAA"

let part_2 (instructions, nodes) =
  let advance = function
    | [] -> (List.tl_exn instructions, List.hd_exn instructions)
    | move :: rest -> (rest, move)
  in
  let rec walk move cur =
    if String.is_suffix cur ~suffix:"Z" then 0
    else
      let rest, move = advance move in
      let next =
        let left, right = Map.find_exn nodes cur in
        match move with Left -> left | Right -> right
      in
      1 + walk rest next
  in
  let cursors =
    List.filter (Map.keys nodes) ~f:(fun name ->
        String.is_suffix name ~suffix:"A")
  in
  let rec gcd x y =
    match (x, y) with
    | 0, n | n, 0 -> n
    | n, m -> if n > m then gcd (n - m) m else gcd n (m - n)
  in
  let lcm x y = x * y / gcd x y in
  let lengths = List.map cursors ~f:(walk []) in
  List.fold lengths ~init:1 ~f:lcm
