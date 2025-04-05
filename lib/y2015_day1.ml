open! Base

let step = function
  | '(' -> 1
  | ')' -> -1
  | c -> Error.raise_s [%message "invalid input" (c : char)]
;;

module Part1 = struct
  let solve =
    Utils.read_all_and_print_int (String.fold ~init:0 ~f:(fun acc c -> acc + step c))
  ;;
end

module Part2 = struct
  let solve =
    String.fold_until
      ~init:(0, 0)
      ~f:(fun (idx, acc) c ->
        let acc = acc + step c in
        if acc = -1 then Continue_or_stop.Stop (idx + 1) else Continue (idx + 1, acc))
      ~finish:(fun _ -> raise_s [%message "santa never reaches the basement!"])
  ;;

  let solve = Utils.read_all_and_print_int solve
end
