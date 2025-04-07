open! Base

module Part1 = struct
  let rec num_in_mem_chars = function
    | [] -> 0
    | '\\' :: '\\' :: tl | '\\' :: '"' :: tl | '\\' :: 'x' :: _ :: _ :: tl | _ :: tl ->
      1 + num_in_mem_chars tl
  ;;

  let solve =
    List.fold ~init:0 ~f:(fun acc line ->
      acc + (String.length line - (num_in_mem_chars (String.to_list line) - 2)))
  ;;

  let solve = Utils.read_lines_and_print_int solve
end

module Part2 = struct
  let rec num_chars_to_repr = function
    | [] -> 0
    | '\\' :: tl | '"' :: tl -> 2 + num_chars_to_repr tl
    | _ :: tl -> 1 + num_chars_to_repr tl
  ;;

  let solve =
    List.fold ~init:0 ~f:(fun acc line ->
      acc + num_chars_to_repr (String.to_list line) + 2 - String.length line)
  ;;

  let solve = Utils.read_lines_and_print_int solve
end
