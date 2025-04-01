open! Base

let step = function
  | '(' -> 1
  | ')' -> -1
  | c -> Error.raise_s [%message "invalid input" (c : char)]
;;

module Part1 = struct
  let solve reader writer =
    let rec solve' acc =
      match In_channel.input_char reader with
      | None | Some '\n' -> acc
      | Some c -> solve' (acc + step c)
    in
    solve' 0 |> Int.to_string |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve "";
      [%expect {| 0 |}];
      solve "(";
      [%expect {| 1 |}];
      solve ")";
      [%expect {| -1 |}];
      solve "()";
      [%expect {| 0 |}];
      solve ")(";
      [%expect {| 0 |}];
      solve "(())))";
      [%expect {| -2 |}];
      solve "(())))";
      [%expect {| -2 |}];
      ()
    ;;
  end
end

module Part2 = struct
  let solve reader writer =
    let rec solve' i acc =
      if acc = -1
      then i
      else (
        match In_channel.input_char reader with
        | None | Some '\n' -> failwith "santa never reaches the basement!"
        | Some c -> solve' (i + 1) (acc + step c))
    in
    solve' 0 0 |> Int.to_string |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve ")";
      [%expect {| 1 |}];
      solve "()())";
      [%expect {| 5 |}];
      solve "())(()";
      [%expect {| 3 |}];
      ()
    ;;
  end
end
