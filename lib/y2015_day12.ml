open! Core

(* module Part1_solution_without_yojson = struct
  let to_digit_or_x c =
    if Char.equal '-' c || Char.between c ~low:'0' ~high:'9' then c else 'x'
  ;;

  let solve s =
    s
    |> String.to_list
    |> List.map ~f:to_digit_or_x
    |> List.group ~break:(fun a b -> Char.equal 'x' a || Char.equal 'x' b)
    |> List.fold ~init:0 ~f:(fun acc digits ->
      acc
      +
      match digits with
      | [] | [ 'x' ] -> 0
      | digits -> digits |> String.of_list |> Int.of_string)
  ;;

  let solve = Utils.read_all_and_print_int solve
end *)

let solve s ~exclude_assoc =
  let rec solve' = function
    | `Assoc xs ->
      if exclude_assoc xs
      then 0
      else List.fold xs ~init:0 ~f:(fun acc (_, x) -> acc + solve' x)
    | `List xs -> List.fold xs ~init:0 ~f:(fun acc x -> acc + solve' x)
    | `Int x -> x
    | `Bool _ | `Float _ | `Null | `String _ -> 0
  in
  s |> Yojson.Basic.from_string |> solve'
;;

module Part1 = struct
  let solve = solve ~exclude_assoc:(Fn.const false)
  let solve = Utils.read_all_and_print_int solve
end

module Part2 = struct
  let solve =
    solve
      ~exclude_assoc:
        (List.exists ~f:(fun (_, x) ->
           match x with
           | `String "red" -> true
           | _ -> false))
  ;;

  let solve = Utils.read_all_and_print_int solve
end
