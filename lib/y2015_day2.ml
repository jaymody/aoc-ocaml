open! Core

let parse line =
  match String.split line ~on:'x' |> List.map ~f:Int.of_string with
  | [ l; w; h ] -> l, w, h
  | _ -> failwith "line must be in format <length>x<width>x<height>"
;;

module Part1 = struct
  let solve =
    List.fold ~init:0 ~f:(fun acc line ->
      let l, w, h = parse line in
      let areas = [ l * w; w * h; h * l ] in
      let smallest_side = List.min_elt areas ~compare:Int.compare |> Option.value_exn in
      acc + smallest_side + List.fold areas ~init:0 ~f:(fun acc x -> acc + (2 * x)))
  ;;

  let solve = Utils.read_lines_and_print_int solve
end

module Part2 = struct
  let solve =
    List.fold ~init:0 ~f:(fun acc line ->
      let l, w, h = parse line in
      let min1, min2 = if l > w && l > h then w, h else l, if w < h then w else h in
      acc + (2 * (min1 + min2)) + (l * w * h))
  ;;

  let solve = Utils.read_lines_and_print_int solve
end
