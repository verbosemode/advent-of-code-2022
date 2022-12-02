(*
Day1-1: 69289
Day1-2: 205615
*)

let part1_test_input =
  {|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|}

let part1_test_output =
  [ [ 2000; 3000 ]; [ 4000 ]; [ 5000; 6000 ]; [ 7000; 8000; 9000 ]; [ 10000 ] ]

let sum = List.fold_left (+) 0

let find_most_callories l =
  List.fold_left
    (fun a e ->
      let callories = sum e in
      if callories > a then callories else a)
    0 l

let parse_elves l =
  let rec aux l a elves =
    match l with
    | [] -> a :: elves
    | h :: t ->
        if String.length h = 0 then aux t [] (a :: elves)
        else aux t (int_of_string h :: a) elves
  in
  aux l [] []

let part1 filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> parse_elves
  |> find_most_callories

let part2 filename =
  In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> parse_elves
  |> List.map sum
  |> List.sort compare
  |> List.rev
  |> List.to_seq
  |> Seq.take 3
  |> Seq.fold_left (fun a e -> e + a) 0

let () = part1 Sys.argv.(1) |> Printf.printf "day1-1: %i\n"
let () = part2 Sys.argv.(1) |> Printf.printf "day1-2: %i\n"
