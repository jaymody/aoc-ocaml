open! Core

let char_to_num c = Char.to_int c - Char.to_int 'a'
let num_to_char n = Char.of_int_exn (Char.to_int 'a' + n)
let string_to_nums s = s |> String.to_list |> List.map ~f:char_to_num
let nums_to_string nums = nums |> List.map ~f:num_to_char |> String.of_list

let is_iol c =
  let is_char c' = Int.equal (char_to_num c') c in
  is_char 'i' || is_char 'o' || is_char 'l'
;;

let has_iol = List.exists ~f:is_iol

let rec has_three_in_a_row = function
  | x1 :: (x2 :: x3 :: _ as tl) ->
    if x2 = x1 + 1 && x3 = x1 + 2 then true else has_three_in_a_row tl
  | _ -> false
;;

let has_different_non_overlapping_pairs xs =
  let test_condition prev a b =
    a = b
    &&
    match prev with
    | None -> false
    | Some x -> a <> x
  in
  let rec has_different_non_overlapping_pairs' prev = function
    | a :: (b :: c :: d :: _ as tl) ->
      let prev = if a = b then Some a else prev in
      if test_condition prev c d
      then true
      else has_different_non_overlapping_pairs' prev tl
    | [ _; a; b ] -> test_condition prev a b
    | _ -> false
  in
  has_different_non_overlapping_pairs' None xs
;;

let is_good xs =
  has_three_in_a_row xs && (not (has_iol xs)) && has_different_non_overlapping_pairs xs
;;

let bump xs =
  xs
  |> List.rev
  |> List.fold_map ~init:1 ~f:(fun acc x -> if x + acc = 26 then 1, 0 else 0, x + acc)
  |> snd
  |> List.rev
;;

let rec solve xs = if is_good xs then xs else solve (bump xs)

module Part1 = struct
  let solve s = s |> string_to_nums |> solve |> nums_to_string
  let solve = Utils.read_all_and_print_string solve
end

module Part2 = struct
  let solve s = s |> string_to_nums |> solve |> bump |> solve |> nums_to_string
  let solve = Utils.read_all_and_print_string solve
end
