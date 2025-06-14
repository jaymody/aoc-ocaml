open! Core

module Pair = struct
  module T = struct
    type t = string * string [@@deriving compare, hash, sexp]
  end

  include Hashable.Make (T)
end

let parse_line line =
  match String.split ~on:' ' line with
  | [ a; "would"; "gain"; n; "happiness"; "units"; "by"; "sitting"; "next"; "to"; b ] ->
    let b = String.chop_suffix_exn b ~suffix:"." in
    let n = Int.of_string n in
    (a, b), n
  | [ a; "would"; "lose"; n; "happiness"; "units"; "by"; "sitting"; "next"; "to"; b ] ->
    let b = String.chop_suffix_exn b ~suffix:"." in
    let n = Int.of_string n in
    (a, b), -n
  | _ -> raise_s [%message "failed to parse line" (line : string)]
;;

let neighbor_happiness a b ~happiness_tbl =
  Hashtbl.find_exn happiness_tbl (a, b) + Hashtbl.find_exn happiness_tbl (b, a)
;;

let seating_happiness seating ~happiness_tbl =
  let rec aux acc = function
    | a :: (b :: _ as tl) -> aux (acc + neighbor_happiness a b ~happiness_tbl) tl
    | [ last_guest ] -> acc, last_guest
    | _ -> raise_s [%message "seating must not be empty"]
  in
  let total, last_guest = aux 0 seating in
  let first_guest = List.hd_exn seating in
  total + neighbor_happiness first_guest last_guest ~happiness_tbl
;;

let solve lines ~including_me =
  let data = List.map lines ~f:parse_line in
  let guests =
    data
    |> List.map ~f:(fun ((x, _), _) -> x)
    |> List.dedup_and_sort ~compare:[%compare: string]
  in
  let happiness_tbl = Pair.Table.of_alist_exn data in
  let guests =
    match including_me with
    | false -> guests
    | true ->
      let me = "me" in
      List.iter guests ~f:(fun guest ->
        Hashtbl.add_exn happiness_tbl ~key:(guest, me) ~data:0;
        Hashtbl.add_exn happiness_tbl ~key:(me, guest) ~data:0);
      me :: guests
  in
  guests
  |> Utils.permutations
  |> List.map ~f:(seating_happiness ~happiness_tbl)
  |> List.max_elt ~compare:[%compare: int]
  |> Option.value_exn
;;

module Part1 = struct
  let solve = Utils.read_lines_and_print_int (solve ~including_me:false)
end

module Part2 = struct
  let solve = Utils.read_lines_and_print_int (solve ~including_me:true)
end
