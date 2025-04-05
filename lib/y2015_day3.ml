open! Base

module Pos = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare]
  end

  include T
  include Comparator.Make (T)
end

module Santa = struct
  type t =
    { pos : Pos.t
    ; seen : (Pos.t, Pos.comparator_witness) Base.Set.t
    }

  let create () =
    let pos = 0, 0 in
    let seen = Set.empty (module Pos) in
    let seen = Set.add seen pos in
    { pos; seen }
  ;;

  let move t c =
    let x, y = t.pos in
    let new_pos =
      match c with
      | '^' -> x, y + 1
      | '>' -> x + 1, y
      | 'v' -> x, y - 1
      | '<' -> x - 1, y
      | c -> Error.raise_s [%message "invalid input" (c : char)]
    in
    let seen = Set.add t.seen new_pos in
    { pos = new_pos; seen }
  ;;
end

module Part1 = struct
  let solve s =
    s
    |> String.fold ~init:(Santa.create ()) ~f:(fun acc c -> Santa.move acc c)
    |> fun santa -> Set.length santa.Santa.seen
  ;;

  let solve = Utils.read_all_and_print_int solve
end

module Part2 = struct
  let solve s =
    s
    |> String.fold
         ~init:(`Real, Santa.create (), Santa.create ())
         ~f:(fun (turn, real, robo) c ->
           match turn with
           | `Real -> `Robo, Santa.move real c, robo
           | `Robo -> `Real, real, Santa.move robo c)
    |> fun (_, real, robo) -> Set.union real.Santa.seen robo.Santa.seen |> Set.length
  ;;

  let solve = Utils.read_all_and_print_int solve
end
