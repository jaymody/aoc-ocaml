open! Base

let solve secret_key n =
  let prefix = String.init n ~f:(fun _ -> '0') in
  let rec solve' i =
    let digest =
      Stdlib.Digest.MD5.string (secret_key ^ Int.to_string i) |> Stdlib.Digest.to_hex
    in
    if String.is_prefix digest ~prefix then i else solve' (i + 1)
  in
  solve' 1
;;

module Part1 = struct
  let solve reader writer =
    let secret_key = In_channel.input_all reader |> String.strip in
    solve secret_key 5 |> Int.to_string |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve "abcdef";
      [%expect {| 609043 |}];
      solve "pqrstuv";
      [%expect {| 1048970 |}];
      ()
    ;;
  end
end

module Part2 = struct
  let solve reader writer =
    let secret_key = In_channel.input_all reader |> String.strip in
    solve secret_key 6 |> Int.to_string |> Out_channel.output_string writer
  ;;
end
