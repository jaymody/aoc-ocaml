val solve_from_string : (in_channel -> out_channel -> unit) -> string -> unit
val read_all_and_print_int : (string -> int) -> in_channel -> out_channel -> unit
val read_lines_and_print_int : (string list -> int) -> in_channel -> out_channel -> unit
val read_all_and_print_string : (string -> string) -> in_channel -> out_channel -> unit
val permutations : 'a list -> 'a list list
