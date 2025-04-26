open! Core

module Pair = struct
  type t = string * string [@@deriving compare, sexp_of, hash]
end

let parse lines =
  let dists = Hashtbl.create (module Pair) in
  let cities = Hash_set.create (module String) in
  List.iter lines ~f:(fun line ->
    match String.split line ~on:' ' with
    | [ src; "to"; dst; "="; distance ] ->
      let distance = Int.of_string distance in
      Hashtbl.set dists ~key:(src, dst) ~data:distance;
      Hashtbl.set dists ~key:(dst, src) ~data:distance;
      Hash_set.add cities src;
      Hash_set.add cities dst
    | _ -> raise_s [%message "could not parse line" (line : string)]);
  dists, Hash_set.to_list cities
;;

let route_distance tbl =
  let rec route_distance' = function
    | [] | [ _ ] -> 0
    | a :: (b :: _ as rest) -> Hashtbl.find_exn tbl (a, b) + route_distance' rest
  in
  route_distance'
;;

let solve lines ~init ~select =
  let dists, cities = parse lines in
  cities
  |> Utils.permutations
  |> List.fold ~init ~f:(fun best route -> select best (route_distance dists route))
;;

module Part1 = struct
  let solve = Utils.read_lines_and_print_int (solve ~init:Int.max_value ~select:Int.min)
end

module Part2 = struct
  let solve = Utils.read_lines_and_print_int (solve ~init:Int.min_value ~select:Int.max)
end
