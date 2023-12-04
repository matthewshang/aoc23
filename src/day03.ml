open Core

type input = string Array.t

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

let input_of_lines lines = Array.of_list lines

let neighbors =
  [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) ]

let build_map grid =
  let n = Array.length grid in
  let m = String.length grid.(0) in
  let map_neighbors i j ~f =
    List.filter_map neighbors ~f:(fun (di, dj) ->
        let i' = i + di in
        let j' = j + dj in
        if i' >= 0 && i' < n && j' >= 0 && j' < m then Some (f i' j') else None)
  in
  let is_leftmost i j = j = 0 || not (Char.is_digit grid.(i).[j - 1]) in
  let is_symbol c = match c with '0' .. '9' | '.' -> false | _ -> true in
  let get_number_end i j =
    let row = grid.(i) in
    match String.lfindi row ~pos:j ~f:(fun _ c -> not (Char.is_digit c)) with
    | Some end_pos -> end_pos
    | None -> String.length row
  in
  let cells =
    Array.to_list grid
    |> List.mapi ~f:(fun i row ->
           String.to_list row |> List.mapi ~f:(fun j cell -> (i, j, cell)))
    |> List.concat
  in
  List.filter_map cells ~f:(fun (i, j, cell) ->
      match cell with
      | '0' .. '9' ->
          let row = grid.(i) in
          if is_leftmost i j then
            let end_pos = get_number_end i j in
            let symbols =
              List.concat_map
                (List.init (end_pos - j) ~f:Fn.id)
                ~f:(fun k ->
                  map_neighbors i (j + k) ~f:(fun i' j' ->
                      if is_symbol grid.(i').[j'] then Some (i', j') else None)
                  |> List.filter_opt)
            in
            if List.is_empty symbols then None
            else
              let number =
                String.sub row ~pos:j ~len:(end_pos - j) |> Int.of_string
              in
              Some (number, List.dedup_and_sort symbols ~compare:IntPair.compare)
          else None
      | _ -> None)

let part_1 grid =
  let symbols_of_number = build_map grid in
  List.map symbols_of_number ~f:fst |> List.fold ~init:0 ~f:( + )

let part_2 grid =
  let symbols_of_number = build_map grid in
  let numbers_of_symbol =
    List.fold symbols_of_number
      ~init:(Map.empty (module IntPair))
      ~f:(fun acc (number, symbols) ->
        List.filter symbols ~f:(fun (i, j) -> Char.( = ) grid.(i).[j] '*')
        |> List.fold ~init:acc ~f:(fun acc symbol ->
               Map.update acc symbol ~f:(function
                 | None -> [ number ]
                 | Some numbers -> number :: numbers)))
  in
  Map.filter numbers_of_symbol ~f:(fun numbers -> List.length numbers >= 2)
  |> Map.data
  |> List.map ~f:(List.fold ~init:1 ~f:( * ))
  |> List.fold ~init:0 ~f:( + )
