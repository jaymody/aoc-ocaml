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

  let idx_seq_of_rectangle t (rect : Rect.t) =
    Sequence.cartesian_product
      (Sequence.range ~stop:`inclusive rect.topleft.x rect.botright.x)
      (Sequence.range ~stop:`inclusive rect.topleft.y rect.botright.y)
    |> Sequence.map ~f:(fun (x, y) -> (x * t.width) + y)
  ;;

  let on t rect =
    idx_seq_of_rectangle t rect |> Sequence.iter ~f:(fun idx -> t.lights.(idx) <- 1)
  ;;

  let off t rect =
    idx_seq_of_rectangle t rect |> Sequence.iter ~f:(fun idx -> t.lights.(idx) <- 0)
  ;;

  let toggle t rect =
    idx_seq_of_rectangle t rect
    |> Sequence.iter ~f:(fun idx ->
      t.lights.(idx) <- (if t.lights.(idx) = 0 then 1 else 0))
  ;;

  let num_lights_on t = Array.sum (module Int) t.lights ~f:Fn.id
end

module Part1 = struct
  let solve reader writer =
    let grid = Grid.empty () in
    let rec solve' () =
      match In_channel.input_line reader with
      | None -> ()
      | Some line ->
        (match String.split line ~on:' ' with
         | [ "turn"; "off"; topleft; "through"; botright ] ->
           Grid.off grid (Rect.of_strings ~topleft ~botright)
         | [ "turn"; "on"; topleft; "through"; botright ] ->
           Grid.on grid (Rect.of_strings ~topleft ~botright)
         | [ "toggle"; topleft; "through"; botright ] ->
           Grid.toggle grid (Rect.of_strings ~topleft ~botright)
         | _ -> raise_s [%message "could not parse line" (line : string)]);
        solve' ()
    in
    solve' ();
    Grid.num_lights_on grid |> Int.to_string |> Out_channel.output_string writer
  ;;
end

module Part2 = struct
  module Grid = struct
    include Grid

    let on t rect =
      idx_seq_of_rectangle t rect
      |> Sequence.iter ~f:(fun idx -> t.lights.(idx) <- t.lights.(idx) + 1)
    ;;

    let off t rect =
      idx_seq_of_rectangle t rect
      |> Sequence.iter ~f:(fun idx -> t.lights.(idx) <- Int.max 0 (t.lights.(idx) - 1))
    ;;

    let toggle t rect =
      idx_seq_of_rectangle t rect
      |> Sequence.iter ~f:(fun idx -> t.lights.(idx) <- t.lights.(idx) + 2)
    ;;
  end

  let solve reader writer =
    let grid = Grid.empty () in
    let rec solve' () =
      match In_channel.input_line reader with
      | None -> ()
      | Some line ->
        (match String.split line ~on:' ' with
         | [ "turn"; "off"; topleft; "through"; botright ] ->
           Grid.off grid (Rect.of_strings ~topleft ~botright)
         | [ "turn"; "on"; topleft; "through"; botright ] ->
           Grid.on grid (Rect.of_strings ~topleft ~botright)
         | [ "toggle"; topleft; "through"; botright ] ->
           Grid.toggle grid (Rect.of_strings ~topleft ~botright)
         | _ -> raise_s [%message "could not parse line" (line : string)]);
        solve' ()
    in
    solve' ();
    Grid.num_lights_on grid |> Int.to_string |> Out_channel.output_string writer
  ;;
end
