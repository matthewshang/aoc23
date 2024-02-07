open Core

type input = string array
type direction = Right | Left | Up | Down [@@deriving sexp]

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module PairSet = Set.Make (IntPair)
module PairMap = Map.Make (IntPair)

let input_of_lines lines = Array.of_list lines

let part_1 grid =
  let n = Array.length grid in
  let m = String.length (Array.last grid) in
  let start_row, start_col =
    Array.find_mapi_exn grid ~f:(fun r row ->
        String.find_mapi row ~f:(fun c value ->
            match value with 'S' -> Some (r, c) | _ -> None))
  in
  let rec go r c dir =
    if r < 0 || r = n || c < 0 || c = m then None
    else
      let res =
        match (grid.(r).[c], dir) with
        | '|', Down -> go (r + 1) c Down
        | '|', Up -> go (r - 1) c Up
        | '-', Left -> go r (c - 1) Left
        | '-', Right -> go r (c + 1) Right
        | 'L', Down -> go r (c + 1) Right
        | 'L', Left -> go (r - 1) c Up
        | 'J', Down -> go r (c - 1) Left
        | 'J', Right -> go (r - 1) c Up
        | '7', Up -> go r (c - 1) Left
        | '7', Right -> go (r + 1) c Down
        | 'F', Up -> go r (c + 1) Right
        | 'F', Left -> go (r + 1) c Down
        | 'S', _ -> Some 0
        | _ -> None
      in
      Option.map res ~f:(fun x -> x + 1)
  in
  let paths =
    [
      go (start_row + 1) start_col Down;
      go (start_row - 1) start_col Up;
      go start_row (start_col - 1) Left;
      go start_row (start_col + 1) Right;
    ]
  in
  let length = List.filter_opt paths |> List.hd_exn in
  length / 2

let part_2 _g = 0

(*
   let part_2 grid =
     let n = Array.length grid in
     let m = String.length (Array.last grid) in
     let start_row, start_col =
       Array.find_mapi_exn grid ~f:(fun r row ->
           String.find_mapi row ~f:(fun c value ->
               match value with 'S' -> Some (r, c) | _ -> None))
     in
     let rec go r c dir =
       if r < 0 || r = n || c < 0 || c = m then None
       else
         let s= grid.(r).[c] in
         let res =
           match (s, dir) with
           | '|', Down -> go (r + 1) c Down
           | '|', Up -> go (r - 1) c Up
           | '-', Left -> go r (c - 1) Left
           | '-', Right -> go r (c + 1) Right
           | 'L', Down -> go r (c + 1) Right
           | 'L', Left -> go (r - 1) c Up
           | 'J', Down -> go r (c - 1) Left
           | 'J', Right -> go (r - 1) c Up
           | '7', Up -> go r (c - 1) Left
           | '7', Right -> go (r + 1) c Down
           | 'F', Up -> go r (c + 1) Right
           | 'F', Left -> go (r + 1) c Down
           | 'S', _ -> Some []
           | _ -> None
         in
         Option.map res ~f:(fun cells -> (r, c) :: cells)
     in
     let paths =
         [
           go (start_row + 1) start_col Down;
           go (start_row - 1) start_col Up;
           go start_row (start_col - 1) Left;
           go start_row (start_col + 1) Right;
         ]
     in
     match (List.find)
     let cells =
       List.cartesian_product
         (List.init (n - 1) ~f:Fn.id)
         (List.init (m - 1) ~f:Fn.id)
     in
     List.count cells ~f:(fun (r, c) -> escape r c PairSet.empty) *)
