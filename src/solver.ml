module type Solution = sig
  type input

  val input_of_lines : string list -> input
  val part_1 : input -> int
  val part_2 : input -> int
end

module type T = sig
  val part_1 : string list -> string
  val part_2 : string list -> string
  val solve : string list -> string * string
end

module Make (S : Solution) : T = struct
  let part_1 input_lines =
    let input = S.input_of_lines input_lines in
    let answer = S.part_1 input in
    Int.to_string answer

  let part_2 input_lines =
    let input = S.input_of_lines input_lines in
    let answer = S.part_2 input in
    Int.to_string answer

  let solve input_lines = (part_1 input_lines, part_2 input_lines)
end
