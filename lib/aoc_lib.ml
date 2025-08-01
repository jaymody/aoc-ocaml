open! Core

let solve = function
  | "y2015_day1_part1" -> Y2015_day1.Part1.solve
  | "y2015_day1_part2" -> Y2015_day1.Part2.solve
  | "y2015_day2_part1" -> Y2015_day2.Part1.solve
  | "y2015_day2_part2" -> Y2015_day2.Part2.solve
  | "y2015_day3_part1" -> Y2015_day3.Part1.solve
  | "y2015_day3_part2" -> Y2015_day3.Part2.solve
  | "y2015_day4_part1" -> Y2015_day4.Part1.solve
  | "y2015_day4_part2" -> Y2015_day4.Part2.solve
  | "y2015_day5_part1" -> Y2015_day5.Part1.solve
  | "y2015_day5_part2" -> Y2015_day5.Part2.solve
  | "y2015_day6_part1" -> Y2015_day6.Part1.solve
  | "y2015_day6_part2" -> Y2015_day6.Part2.solve
  | "y2015_day7_part1" -> Y2015_day7.Part1.solve
  | "y2015_day7_part2" -> Y2015_day7.Part2.solve
  | "y2015_day8_part1" -> Y2015_day8.Part1.solve
  | "y2015_day8_part2" -> Y2015_day8.Part2.solve
  | "y2015_day9_part1" -> Y2015_day9.Part1.solve
  | "y2015_day9_part2" -> Y2015_day9.Part2.solve
  | "y2015_day10_part1" -> Y2015_day10.Part1.solve
  | "y2015_day10_part2" -> Y2015_day10.Part2.solve
  | "y2015_day11_part1" -> Y2015_day11.Part1.solve
  | "y2015_day11_part2" -> Y2015_day11.Part2.solve
  | "y2015_day12_part1" -> Y2015_day12.Part1.solve
  | "y2015_day12_part2" -> Y2015_day12.Part2.solve
  | "y2015_day13_part1" -> Y2015_day13.Part1.solve
  | "y2015_day13_part2" -> Y2015_day13.Part2.solve
  | "y2015_day14_part1" -> Y2015_day14.Part1.solve
  | "y2015_day14_part2" -> Y2015_day14.Part2.solve
  | "y2015_day15_part1" -> Y2015_day15.Part1.solve
  | "y2015_day15_part2" -> Y2015_day15.Part2.solve
  | problem -> Error.raise_s [%message "no solution found" (problem : string)]
;;

let run problem data_dir =
  let solve = solve problem in
  let data_file =
    String.chop_suffix_if_exists data_dir ~suffix:"/"
    ^ "/"
    ^ (problem
       |> String.chop_suffix_if_exists ~suffix:"_part1"
       |> String.chop_suffix_if_exists ~suffix:"_part2")
    ^ ".txt"
  in
  Stdio.In_channel.with_file data_file ~f:(fun in_channel ->
    solve in_channel Stdio.Out_channel.stdout)
;;

let main () =
  match Sys.get_argv () with
  | [| _; problem |] -> run problem "data/"
  | [| _; problem; "stdin" |] -> solve problem In_channel.stdin Out_channel.stdout
  | [| _; problem; data_dir |] -> run problem data_dir
  | _ ->
    failwith
      "ERORR: must provide at least one argument: [problem_name] [data_dir (default: \
       \"data/\")]"
;;
