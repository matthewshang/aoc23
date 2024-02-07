open Core

type input = (string * int list) list

module IntPair = struct
  type t = int * int [@@deriving compare, sexp, hash]
end

let input_of_lines lines =
  List.map lines ~f:(fun line ->
      let springs, groups = String.lsplit2_exn line ~on:' ' in
      (springs, String.split groups ~on:',' |> List.map ~f:Int.of_string))

let part_1 input =
  let rec is_consistent springs groups counter =
    match (springs, counter) with
    | [], c -> c <= 0 && List.is_empty groups
    | '#' :: _, 0 -> false
    | '#' :: rest, -1 -> (
        match groups with
        | [] -> false
        | size :: groups' -> is_consistent rest groups' (size - 1))
    | '#' :: rest, c -> is_consistent rest groups (c - 1)
    | '.' :: rest, c -> if c <= 0 then is_consistent rest groups (-1) else false
    | _ -> false
  in
  let counts =
    List.map input ~f:(fun (springs, groups) ->
        let rec count_consistent acc = function
          | [] -> if is_consistent (List.rev acc) groups (-1) then 1 else 0
          | '?' :: rest ->
              count_consistent ('.' :: acc) rest
              + count_consistent ('#' :: acc) rest
          | c :: rest -> count_consistent (c :: acc) rest
        in
        count_consistent [] (String.to_list springs))
  in
  List.fold counts ~init:0 ~f:( + )

let memo_rec f =
  let h = Hashtbl.create (module IntPair) in
  let rec g args pair =
    match Hashtbl.find h pair with
    | Some v -> v
    | None ->
        let v = f g args pair in
        Hashtbl.set h ~key:pair ~data:v;
        v
  in
  g
(*
   let part_2 input =
     let rec f self (springs, groups) (springs_left, groups_left) =
       match (springs, groups) with
       | [], _ -> if groups_left = 0 then 1 else 0
       | '.' :: rest, _ -> self (rest, groups) (springs_left - 1, groups_left)
       | '#' :: rest, _ ->
     in
     let memo_f = memo_rec f in
     List.map input ~f:(fun (springs, groups) ->
         memo_f
           (String.to_list springs, groups)
           (String.length springs, List.length groups))
     |> List.fold ~init:0 ~f:( + ) *)
