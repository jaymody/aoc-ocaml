open! Core

let parse_line line =
  match String.split ~on:' ' line with
  | [ _name
    ; "can"
    ; "fly"
    ; speed
    ; "km/s"
    ; "for"
    ; fly_time
    ; "seconds,"
    ; "but"
    ; "then"
    ; "must"
    ; "rest"
    ; "for"
    ; rest_time
    ; "seconds."
    ] -> Tuple3.map (speed, fly_time, rest_time) ~f:Int.of_string
  | _ -> raise_s [%message "failed to parse line" (line : string)]
;;

let distance_traveled (speed, fly_time, rest_time) ~travel_time =
  let num_cycles = travel_time / (fly_time + rest_time) in
  let leftover_travel_time = travel_time % (fly_time + rest_time) in
  let leftover_fly_time = Int.min fly_time leftover_travel_time in
  let leftover_distance_traveled = speed * leftover_fly_time in
  let cycles_distance_traveled = num_cycles * speed * fly_time in
  leftover_distance_traveled + cycles_distance_traveled
;;

module Part1 = struct
  let best_distance deers ~travel_time =
    deers
    |> List.map ~f:(distance_traveled ~travel_time)
    |> List.max_elt ~compare:[%compare: int]
    |> Option.value_exn
  ;;

  let solve lines ~travel_time =
    lines |> List.map ~f:parse_line |> best_distance ~travel_time
  ;;

  let solve = Utils.read_lines_and_print_int (solve ~travel_time:2503)
end

module Part2 = struct
  let solve lines ~travel_time =
    let deers = List.map lines ~f:parse_line in
    deers
    |> List.length
    |> List.init ~f:(Fn.const 0)
    |> Utils.apply_n_timesi ~n:travel_time (fun n scores ->
      let distances = deers |> List.map ~f:(distance_traveled ~travel_time:n) in
      let best_distance =
        distances |> List.max_elt ~compare:[%compare: int] |> Option.value_exn
      in
      List.map2_exn distances scores ~f:(fun distance score ->
        if distance = best_distance then score + 1 else score))
    |> List.max_elt ~compare:[%compare: int]
    |> Option.value_exn
  ;;

  let solve = Utils.read_lines_and_print_int (solve ~travel_time:2503)
end
