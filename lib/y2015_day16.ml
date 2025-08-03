open! Core

let aunt_sue_facts =
  [ "children", 3
  ; "cats", 7
  ; "samoyeds", 2
  ; "pomeranians", 3
  ; "akitas", 0
  ; "vizslas", 0
  ; "goldfish", 5
  ; "trees", 3
  ; "cars", 2
  ; "perfumes", 1
  ]
;;

let solve lines ~verify_fact =
  let matches =
    List.fold lines ~init:[] ~f:(fun acc line ->
      let line =
        String.filter line ~f:(fun c -> not (Char.equal ':' c || Char.equal ',' c))
      in
      match String.split line ~on:' ' with
      | "Sue" :: n :: facts ->
        let keys, values =
          facts
          |> List.mapi ~f:(fun i x ->
            match i % 2 = 0 with
            | true -> Either.First x
            | false -> Either.Second (Int.of_string x))
          |> List.partition_map ~f:Fn.id
        in
        let facts = List.zip_exn keys values in
        (match List.for_all facts ~f:verify_fact with
         | false -> acc
         | true -> n :: acc)
      | _ -> raise_s [%message "failed to parse line" (line : string)])
  in
  match matches with
  | [] -> raise_s [%message "no matches"]
  | [ n ] -> n
  | matches -> raise_s [%message "multiple matches" (matches : string list)]
;;

module Part1 = struct
  let verify_fact = List.mem aunt_sue_facts ~equal:[%equal: string * int]
  let solve = Utils.read_lines_and_print_string (solve ~verify_fact)
end

module Part2 = struct
  let verify_fact (key, value) =
    let sue_value = List.Assoc.find_exn aunt_sue_facts key ~equal:[%equal: string] in
    match key with
    | "cats" | "trees" -> value > sue_value
    | "pomeranians" | "goldfish" -> value < sue_value
    | _ -> value = sue_value
  ;;

  let solve = Utils.read_lines_and_print_string (solve ~verify_fact)
end
