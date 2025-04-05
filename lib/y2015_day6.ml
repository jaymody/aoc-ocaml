open! Base

module Coord = struct
  type t =
    { x : int
    ; y : int
    }

  let of_string s =
    match String.split s ~on:',' with
    | [ x; y ] -> { x = Int.of_string x; y = Int.of_string y }
    | _ -> raise_s [%message "failed to parse string to coordinate" (s : string)]
  ;;
end

module Rect = struct
  type t =
    { topleft : Coord.t
    ; botright : Coord.t
    }

  let of_strings ~topleft ~botright =
    { topleft = Coord.of_string topleft; botright = Coord.of_string botright }
  ;;

  let to_seq { topleft; botright } =
    Sequence.cartesian_product
      (Sequence.range ~stop:`inclusive topleft.x botright.x)
      (Sequence.range ~stop:`inclusive topleft.y botright.y)
  ;;
end

module Grid = struct
  type t =
    { mutable lights : int array
    ; width : int
    ; height : int
    }

  let empty ?(width = 1000) ?(height = 1000) () =
    { lights = Array.create 0 ~len:(width * height); width; height }
  ;;

  let set_rect t rect ~f =
    Rect.to_seq rect
    |> Sequence.iter ~f:(fun (x, y) ->
      let idx = (x * t.width) + y in
      t.lights.(idx) <- f t.lights.(idx))
  ;;

  let num_lights_on t = Array.sum (module Int) t.lights ~f:Fn.id
end

let solve lines ~on ~off ~toggle =
  let grid = Grid.empty () in
  List.iter lines ~f:(fun line ->
    let f, topleft, botright =
      match String.split line ~on:' ' with
      | [ "turn"; "off"; topleft; "through"; botright ] -> off, topleft, botright
      | [ "turn"; "on"; topleft; "through"; botright ] -> on, topleft, botright
      | [ "toggle"; topleft; "through"; botright ] -> toggle, topleft, botright
      | _ -> raise_s [%message "could not parse line" (line : string)]
    in
    let rect = Rect.of_strings ~topleft ~botright in
    Grid.set_rect grid rect ~f);
  Grid.num_lights_on grid
;;

module Part1 = struct
  let solve =
    solve ~on:(fun _ -> 1) ~off:(fun _ -> 0) ~toggle:(fun x -> if x = 0 then 1 else 0)
  ;;

  let solve = Utils.read_lines_and_print_int solve
end

module Part2 = struct
  let solve =
    solve ~on:(fun x -> x + 1) ~off:(fun x -> Int.max 0 (x - 1)) ~toggle:(fun x -> x + 2)
  ;;

  let solve = Utils.read_lines_and_print_int solve
end
