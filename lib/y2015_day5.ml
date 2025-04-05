open! Base

let solve ~is_nice_string =
  Utils.read_lines_and_print_int
    (List.fold ~init:0 ~f:(fun acc line -> acc + Bool.to_int (is_nice_string line)))
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

  let solve = solve ~is_nice_string
end

module Part2 = struct
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
  let solve = solve ~is_nice_string
end
