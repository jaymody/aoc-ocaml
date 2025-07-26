open! Core

let parse =
  List.fold
    ~init:([], [], [], [], [])
    ~f:(fun (capacity, durability, flavor, texture, calories) line ->
      let line =
        String.filter line ~f:(fun c -> not (Char.equal c ':' || Char.equal c ','))
      in
      match String.split ~on:' ' line with
      | [ _name
        ; "capacity"
        ; capacity'
        ; "durability"
        ; durability'
        ; "flavor"
        ; flavor'
        ; "texture"
        ; texture'
        ; "calories"
        ; calories'
        ] ->
        ( Int.of_string capacity' :: capacity
        , Int.of_string durability' :: durability
        , Int.of_string flavor' :: flavor
        , Int.of_string texture' :: texture
        , Int.of_string calories' :: calories )
      | _ -> raise_s [%message "failed to parse line" (line : string)])
;;

let solve ?(total_amount = 100) ?calorie_target lines =
  let capacity, durability, flavor, texture, calories = parse lines in
  let rec solve' acc n amount_left =
    match n, amount_left with
    | 0, amount_left ->
      let acc = acc @ [ amount_left ] in
      let sumprod property =
        List.zip_exn acc property
        |> List.sum (module Int) ~f:(fun (amt, score) -> amt * score)
        |> Int.max 0
      in
      let score =
        sumprod capacity * sumprod durability * sumprod flavor * sumprod texture
      in
      (match calorie_target with
       | None -> score
       | Some target -> if sumprod calories = target then score else 0)
    | n, amount_left ->
      Sequence.range 0 (amount_left + 1)
      |> Sequence.map ~f:(fun amount ->
        solve' (acc @ [ amount ]) (n - 1) (amount_left - amount))
      |> Sequence.max_elt ~compare:Int.compare
      |> Option.value_exn
  in
  solve' [] (List.length capacity - 1) total_amount
;;

module Part1 = struct
  let solve = Utils.read_lines_and_print_int solve
end

module Part2 = struct
  let solve = Utils.read_lines_and_print_int (solve ~calorie_target:500)
end
