open Core

type input = string list

let input_of_lines = Fn.id

let part_1 input =
  let value line =
    let first =
      String.find line ~f:Char.is_digit
      |> Option.value_exn |> Char.get_digit_exn
    in
    let last =
      String.find (String.rev line) ~f:Char.is_digit
      |> Option.value_exn |> Char.get_digit_exn
    in
    (10 * first) + last
  in
  List.fold ~init:0 ~f:( + ) (List.map input ~f:value)

let patterns =
  [
    ("0", 0);
    ("1", 1);
    ("2", 2);
    ("3", 3);
    ("4", 4);
    ("5", 5);
    ("6", 6);
    ("7", 7);
    ("8", 8);
    ("9", 9);
    ("zero", 0);
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
  ]

let part_2 input =
  let value_first line ~patterns =
    String.find_mapi line ~f:(fun i _ ->
        List.find_map patterns ~f:(fun (pattern, value) ->
            if String.is_substring_at line ~pos:i ~substring:pattern then
              Some value
            else None))
    |> Option.value_exn
  in
  let value line =
    let left = value_first line ~patterns in
    let rev_patterns =
      List.map patterns ~f:(fun (pattern, value) -> (String.rev pattern, value))
    in
    let right = value_first (String.rev line) ~patterns:rev_patterns in
    (10 * left) + right
  in
  List.fold ~init:0 ~f:( + ) (List.map input ~f:value)

let%expect_test "part 1" =
  let input = [ "1abc2"; "pqr3stu8vwx"; "a1b2c3d4e5f"; "treb7uchet" ] in
  let answer = part_1 input in
  print_s [%message (answer : int)];
  [%expect {| (answer 142) |}]
