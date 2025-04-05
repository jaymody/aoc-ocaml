open! Base

let solve_common reader writer ~is_nice_string_fn =
  let rec solve' acc =
    match In_channel.input_line reader with
    | None -> acc
    | Some line -> solve' (acc + Bool.to_int (is_nice_string_fn line))
  in
  solve' 0 |> Int.to_string |> Out_channel.output_string writer
;;

module Part1 = struct
  let num_vowels =
    String.fold ~init:0 ~f:(fun acc c ->
      match c with
      | 'a' | 'e' | 'i' | 'o' | 'u' -> acc + 1
      | _ -> acc)
  ;;

  let has_repeating_char =
    String.fold_until
      ~init:'\n' (* a bit hacky, but we know \n is not one of the possible chars *)
      ~f:(fun prev c ->
        if Char.equal c prev
        then Continue_or_stop.Stop true
        else Continue_or_stop.Continue c)
      ~finish:(fun _ -> false)
  ;;

  let has_bad_strings s =
    let output = ref false in
    for pos = 0 to String.length s - 2 do
      match String.sub s ~pos ~len:2 with
      | "ab" | "cd" | "pq" | "xy" -> output := true
      | _ -> ()
    done;
    !output
  ;;

  let is_nice_string s =
    num_vowels s >= 3 && has_repeating_char s && not (has_bad_strings s)
  ;;

  let solve = solve_common ~is_nice_string_fn:is_nice_string

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve "ugknbfddgicrmopn";
      [%expect {| 1 |}];
      solve "aaa";
      [%expect {| 1 |}];
      solve "jchzalrnumimnmhp";
      [%expect {| 0 |}];
      solve "haegwjzuvuyypxyu";
      [%expect {| 0 |}];
      solve "dvszwmarrgswjxmb";
      [%expect {| 0 |}];
      ()
    ;;
  end
end

module Part2 = struct
  (* let group_char_list l ~n =
    l |> List.groupi ~break:(fun i _ _ -> i % n = 0) |> List.map ~f:String.of_list
  ;;

  let has_unique_pairs l =
    let pairs = group_char_list l ~n:2 in
    List.length l <> Set.length (Set.of_list (module String) pairs)
  ;;

  let contains_non_consecutive_pairs s =
    s
    |> String.to_list
    |> function
    | [] -> false
    | _ :: l1 as l2 -> has_unique_pairs l1 || has_unique_pairs l2
  ;; *)

  let contains_non_consecutive_pairs s =
    let contains_pair pair =
      let rec contains_pair' = function
        | a :: (b :: _ as tl) -> [%equal: char * char] (a, b) pair || contains_pair' tl
        | _ -> false
      in
      contains_pair'
    in
    let rec contains_non_consecutive_pairs' = function
      | a :: (b :: tl as rest) ->
        contains_pair (a, b) tl || contains_non_consecutive_pairs' rest
      | _ -> false
    in
    s |> String.to_list |> contains_non_consecutive_pairs'
  ;;

  let contains_sandwich s =
    let rec contains_sandwich' = function
      | a :: (_ :: b :: _ as rest) ->
        if Char.equal a b then true else contains_sandwich' rest
      | _ -> false
    in
    s |> String.to_list |> contains_sandwich'
  ;;

  let is_nice_string s = contains_non_consecutive_pairs s && contains_sandwich s
  let solve = solve_common ~is_nice_string_fn:is_nice_string

  module Test = struct
    let solve = Utils.solve_from_string solve

    let%expect_test "a test" =
      solve "qjhvhtzxzqqjkmpb";
      [%expect {| 1 |}];
      solve "xxyxx";
      [%expect {| 1 |}];
      solve "uurcxstgmygtbstg";
      [%expect {| 0 |}];
      solve "ieodomkazucvgmuy";
      [%expect {| 0 |}];
      ()
    ;;
  end
end
