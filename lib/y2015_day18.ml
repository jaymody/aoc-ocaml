open! Core

let is_on arr (row, col) ~n =
  let is_in_bounds = Int.between ~low:0 ~high:(n - 1) in
  match is_in_bounds row && is_in_bounds col with
  | false -> false
  | true -> arr.((row * n) + col)
;;

let num_on_neighbors arr row col ~n =
  let neighbors =
    [ row - 1, col - 1
    ; row - 1, col
    ; row - 1, col + 1
    ; row, col - 1
    ; row, col + 1
    ; row + 1, col - 1
    ; row + 1, col
    ; row + 1, col + 1
    ]
  in
  List.count neighbors ~f:(is_on arr ~n)
;;

let turn_on_corners arr ~n =
  arr.(0) <- true;
  arr.(n - 1) <- true;
  arr.(n * (n - 1)) <- true;
  arr.((n * n) - 1) <- true;
  arr
;;

let handle_corners arr ~n ~corners_on =
  match corners_on with
  | false -> arr
  | true -> turn_on_corners arr ~n
;;

let step arr ~n ~corners_on =
  let new_arr = Array.create false ~len:(n * n) in
  for row = 0 to n - 1 do
    for col = 0 to n - 1 do
      new_arr.((row * n) + col)
      <- (match arr.((row * n) + col), num_on_neighbors arr row col ~n with
          | true, 2 | true, 3 | false, 3 -> true
          | _ -> false)
    done
  done;
  handle_corners new_arr ~n ~corners_on
;;

let parse_arr lines ~n =
  let arr = Array.create false ~len:(n * n) in
  List.iteri lines ~f:(fun row rowx ->
    String.iteri rowx ~f:(fun col x ->
      arr.((row * n) + col)
      <- (match x with
          | '#' -> true
          | '.' -> false
          | c -> raise_s [%message "invalid char" (c : char)])));
  arr
;;

let solve ?(num_steps = 100) ?(n = 100) lines ~corners_on =
  lines
  |> parse_arr ~n
  |> handle_corners ~n ~corners_on
  |> Fn.apply_n_times ~n:num_steps (step ~n ~corners_on)
  |> Array.count ~f:Fn.id
;;

module Part1 = struct
  let solve = Utils.read_lines_and_print_int (solve ~corners_on:false)
end

module Part2 = struct
  let solve = Utils.read_lines_and_print_int (solve ~corners_on:true)
end
