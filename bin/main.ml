open Core
open Advent_2023

module type Foo = sig
  val foo : string -> string
end

let command =
  Command.basic ~summary:"Run solution for day and input"
    (let%map_open.Command day = flag "-d" (required int) ~doc:"int Day"
     and part = flag "-p" (optional int) ~doc:"int Part"
     and input_file = flag "-i" no_arg ~doc:"string Input file" in
     fun () ->
       let input_lines =
         if input_file then
           let filename = sprintf "input/day%02d" day in
           In_channel.with_file filename ~f:(fun file ->
               In_channel.input_lines file)
         else In_channel.(input_lines stdin)
       in
       let run (module M : Solver) =
         match part with
         | Some 1 -> print_endline (M.part_1 input_lines)
         | Some 2 -> print_endline (M.part_2 input_lines)
         | _ ->
             let part_1, part_2 = M.solve input_lines in
             print_endline part_1;
             print_endline part_2
       in
       match day with
       | 1 -> run (module Day01)
       | 2 -> run (module Day02)
       | 3 -> run (module Day03)
       | 4 -> run (module Day04)
       | 5 -> run (module Day05)
       | 6 -> run (module Day06)
       | 7 -> run (module Day07)
       | 8 -> run (module Day08)
       | _ -> failwith "Day not implemented")

let () = Command_unix.run command
