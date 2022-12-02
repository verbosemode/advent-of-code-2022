(*
day2-1: 13565
day2-2: 12424
*)

exception Parser_error

let test1_input = {|A Y
B X
C Z|}

type items =
  | Rock
  | Paper
  | Scissors

let item_to_score = function
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let draw = 3
let won = 6

module Part_1 = struct
  let item_of_char_exn = function
    | 'A' | 'X' -> Rock
    | 'B' | 'Y' -> Paper
    | 'C' | 'Z' -> Scissors
    | _ -> raise Parser_error

  let items_of_line s =
    try
      item_of_char_exn (String.get s 0), item_of_char_exn (String.get s 2)
    with
    | Invalid_argument _ | Parser_error as e ->
        print_endline s;
        raise e

  let calculate_my_score (other, ours) =
    match other, ours with
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> draw + item_to_score ours
    | Rock, Scissors | Scissors, Paper | Paper, Rock -> item_to_score ours
    | Scissors, Rock | Paper, Scissors | Rock, Paper -> won + item_to_score ours
end

module Part_2 = struct
  type outcome =
    | Win
    | Lose
    | Draw

  let item_of_char_exn = function
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> raise Parser_error

  let items_of_line s =
    try
      Part_1.item_of_char_exn (String.get s 0), item_of_char_exn (String.get s 2)
    with
    | Invalid_argument _ | Parser_error as e ->
        print_endline s;
        raise e

  let our_item_by_intended_outcome (other, outcome) =
    match other, outcome with
    | Rock, Win | Scissors, Lose -> Paper
    | Paper, Win | Rock, Lose -> Scissors
    | Scissors, Win | Paper, Lose -> Rock
    | _, Draw -> other
end

let part1 filename =
  Utils.read_lines filename
  (* FIXME dafuq. just to drop the empty string at the end? *)
  |> List.filter (fun e -> String.length e <> 0)
  |> List.map Part_1.items_of_line
  |> List.fold_left (fun a e -> a + Part_1.calculate_my_score e) 0

let part2 filename =
  Utils.read_lines filename
  (* FIXME dafuq. just to drop the empty string at the end? *)
  |> List.filter (fun e -> String.length e <> 0)
  |> List.map Part_2.items_of_line
  |> List.fold_left (fun a (other, outcome as move) ->
      let our_item = Part_2.our_item_by_intended_outcome move in
      let our_score = Part_1.calculate_my_score (other, our_item) in
      a + our_score) 0

let () = part1 Sys.argv.(1) |> Printf.printf "day2-1: %i\n"
let () = part2 Sys.argv.(1) |> Printf.printf "day2-2: %i\n"
