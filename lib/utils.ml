open! Core

(* TODO: This feels super hacky, but it's the quickest thing I came up with
   to construct an [in_channel] from a [string]. *)
let solve_from_string solve text =
  let filename, out_channel = Stdlib.Filename.open_temp_file "" "" in
  Out_channel.output_string out_channel text;
  Out_channel.flush out_channel;
  In_channel.with_file filename ~f:(fun in_channel -> solve in_channel Out_channel.stdout)
;;

let read_all_and_print_int solve in_channel out_channel =
  in_channel
  |> In_channel.input_all
  |> String.strip
  |> solve
  |> Int.to_string
  |> Out_channel.output_string out_channel
;;

let read_lines_and_print_int solve in_channel out_channel =
  in_channel
  |> In_channel.input_lines
  |> List.map ~f:String.strip
  |> solve
  |> Int.to_string
  |> Out_channel.output_string out_channel
;;

let read_all_and_print_string solve in_channel out_channel =
  in_channel
  |> In_channel.input_all
  |> String.strip
  |> solve
  |> Out_channel.output_string out_channel
;;

let permutations =
  let rec insert x = function
    | [] -> [ [ x ] ]
    | hd :: tl as lst -> (x :: lst) :: List.map (insert x tl) ~f:(List.cons hd)
  in
  let rec permutations' = function
    | [] -> [ [] ]
    | hd :: tl -> List.concat_map (permutations' tl) ~f:(insert hd)
  in
  permutations'
;;

let rec apply_n_timesi ~n f x = if n <= 0 then x else apply_n_timesi ~n:(n - 1) f (f n x)
