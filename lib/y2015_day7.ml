open! Base

let last_16bits x = x land (Int.pow 2 16 - 1)
let lshift_16 x n = last_16bits (x lsl n)
let rshift_16 x n = last_16bits x lsr n

module Rule = struct
  type t =
    | And of string * string
    | Identity of string
    | Lshift of string * int
    | Not of string
    | Or of string * string
    | Rshift of string * int
  [@@deriving sexp_of]
end

let create_rules_table lines =
  let tbl = Hashtbl.create (module String) in
  List.iter lines ~f:(fun line ->
    match String.split line ~on:' ' with
    | [ in_; "->"; out ] -> Hashtbl.set tbl ~key:out ~data:(Rule.Identity in_)
    | [ "NOT"; in_; "->"; out ] -> Hashtbl.set tbl ~key:out ~data:(Not in_)
    | [ in1; op; in2; "->"; out ] ->
      Hashtbl.set
        tbl
        ~key:out
        ~data:
          (match op with
           | "AND" -> And (in1, in2)
           | "OR" -> Or (in1, in2)
           | "LSHIFT" -> Lshift (in1, Int.of_string in2)
           | "RSHIFT" -> Rshift (in1, Int.of_string in2)
           | _ -> raise_s [%message "unsupported op" (op : string)])
    | _ -> raise_s [%message "could not parse line" (line : string)]);
  tbl
;;

let evaluate tbl =
  let memo = Hashtbl.create (module String) in
  let rec evaluate' out =
    match Hashtbl.find memo out with
    | Some c -> c
    | None ->
      (match Hashtbl.find tbl out with
       | None -> Int.of_string out
       | Some (rule : Rule.t) ->
         let data =
           match rule with
           | Identity in_ -> evaluate' in_
           | Not in_ -> lnot (evaluate' in_)
           | Or (in1, in2) -> evaluate' in1 lor evaluate' in2
           | And (in1, in2) -> evaluate' in1 land evaluate' in2
           | Lshift (in_, n) -> lshift_16 (evaluate' in_) n
           | Rshift (in_, n) -> rshift_16 (evaluate' in_) n
         in
         Hashtbl.set memo ~key:out ~data;
         data)
  in
  evaluate'
;;

module Part1 = struct
  let solve lines =
    let tbl = create_rules_table lines in
    evaluate tbl "a"
  ;;

  let solve = Utils.read_lines_and_print_int solve
end

module Part2 = struct
  let solve lines =
    let tbl = create_rules_table lines in
    Hashtbl.set tbl ~key:"b" ~data:(Identity (Int.to_string (evaluate tbl "a")));
    evaluate tbl "a"
  ;;

  let solve = Utils.read_lines_and_print_int solve
end
