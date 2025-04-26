open! Core

let solve secret_key ~n =
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
  let solve = Utils.read_all_and_print_int (solve ~n:5)
end

module Part2 = struct
  let solve = Utils.read_all_and_print_int (solve ~n:6)
end
