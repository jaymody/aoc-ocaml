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
  let solve reader writer =
    let rec solve' santa =
      match In_channel.input_char reader with
      | None | Some '\n' -> Set.length santa.Santa.seen
      | Some c -> solve' (Santa.move santa c)
    in
    solve' (Santa.create ()) |> Int.to_string |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve ">";
      [%expect {| 2 |}];
      solve "^>v<";
      [%expect {| 4 |}];
      solve "^v^v^v^v^v";
      [%expect {| 2 |}];
      ()
    ;;
  end
end

module Part2 = struct
  let solve reader writer =
    let rec solve' turn real robo =
      match In_channel.input_char reader with
      | None | Some '\n' -> Set.union real.Santa.seen robo.Santa.seen |> Set.length
      | Some c ->
        (match turn with
         | `Real -> solve' `Robo (Santa.move real c) robo
         | `Robo -> solve' `Real real (Santa.move robo c))
    in
    solve' `Real (Santa.create ()) (Santa.create ())
    |> Int.to_string
    |> Out_channel.output_string writer
  ;;

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve ">";
      [%expect {| 2 |}];
      solve "^>v<";
      [%expect {| 3 |}];
      solve "^v^v^v^v^v";
      [%expect {| 11 |}];
      ()
    ;;
  end
end
