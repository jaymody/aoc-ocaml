open! Core

type t =
  { digit : int
  ; count : int
  }

let look_to_say =
  let[@tail_mod_cons] rec look_to_say' t = function
    | [] -> [ t ]
    | hd :: tl ->
      if hd = t.digit
      then look_to_say' { t with count = t.count + 1 } tl
      else t :: look_to_say' { digit = hd; count = 1 } tl
  in
  function
  | [] -> []
  | hd :: tl -> look_to_say' { digit = hd; count = 1 } tl
;;

let say_to_look = List.concat_map ~f:(fun { digit; count } -> [ count; digit ])

let solve s ~n =
  String.to_list s
  |> List.map ~f:(fun c -> c |> String.of_char |> Int.of_string)
  |> Fn.apply_n_times ~n (fun x -> x |> look_to_say |> say_to_look)
  |> List.length
;;

module Part1 = struct
  let solve = Utils.read_all_and_print_int (solve ~n:40)
end

module Part2 = struct
  let solve = Utils.read_all_and_print_int (solve ~n:50)
end
