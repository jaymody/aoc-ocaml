open! Base

(* TODO: This feels super hacky, but it's the quickest thing I came up with
   to construct an [in_channel] from a [string]. *)
let solve_from_string solve text =
  let filename, out_channel = Stdlib.Filename.open_temp_file "" "" in
  Out_channel.output_string out_channel text;
  Out_channel.flush out_channel;
  In_channel.with_open_text filename (fun in_channel ->
    solve in_channel Out_channel.stdout)
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
