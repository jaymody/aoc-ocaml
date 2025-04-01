open! Base

let parse line =
  match String.split line ~on:'x' |> List.map ~f:Int.of_string with
  | [ l; w; h ] -> l, w, h
  | _ -> failwith "line must be in format <length>x<width>x<height>"
;;

module Part1 = struct
  let solve reader writer =
    Stdio.In_channel.fold_lines reader ~init:0 ~f:(fun acc line ->
      let l, w, h = parse line in
      let areas = [ l * w; w * h; h * l ] in
      let smallest_side = List.min_elt areas ~compare:Int.compare |> Option.value_exn in
      acc + smallest_side + List.fold areas ~init:0 ~f:(fun acc x -> acc + (2 * x)))
    |> Int.to_string
    |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve
    let test lines = solve (String.concat ~sep:"\n" lines)

    let%expect_test "a test" =
      test [ "2x3x4"; "1x1x10" ];
      [%expect {| 101 |}];
      ()
    ;;
  end
end

module Part2 = struct
  let solve reader writer =
    Stdio.In_channel.fold_lines reader ~init:0 ~f:(fun acc line ->
      let l, w, h = parse line in
      let min1, min2 = if l > w && l > h then w, h else l, if w < h then w else h in
      acc + (2 * (min1 + min2)) + (l * w * h))
    |> Int.to_string
    |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve
    let test lines = solve (String.concat ~sep:"\n" lines)

    let%expect_test "a test" =
      test [ "2x3x4"; "1x1x10" ];
      [%expect {| 48 |}];
      ()
    ;;
  end
end
