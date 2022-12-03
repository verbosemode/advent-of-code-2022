(*
day3-1: 8018
day3-2: 2518
*)

exception Parser_error

module CharSet = Set.Make (Char)

let cut_string_in_half s =
  let l = String.length s in
  assert (l > 0);
  (String.sub s 0 (l / 2), String.sub s (l / 2) (l / 2))

let string_explode = String.fold_left (fun a c -> c :: a) []

let char_priority c =
  match c with
  | 'a' .. 'z' -> Char.code c - 96
  | 'A' .. 'Z' -> Char.code c - 38
  | _ ->
      raise
        (Invalid_argument (Printf.sprintf "char_score: Invalid character: %c" c))

let item_priority s =
  let comp1, comp2 = cut_string_in_half s in
  let comp1_set = CharSet.of_list (string_explode comp1) in
  let comp2_set = CharSet.of_list (string_explode comp2) in
  let shared_item = CharSet.inter comp1_set comp2_set in
  assert (CharSet.cardinal shared_item = 1);
  List.nth (CharSet.elements shared_item) 0 |> char_priority

let group_by_3 l =
  let rec aux l a =
    match l with
    | [] -> a
    | e1 :: e2 :: e3 :: t ->
        let e1_set = CharSet.of_list (string_explode e1) in
        let e2_set = CharSet.of_list (string_explode e2) in
        let e3_set = CharSet.of_list (string_explode e3) in
        let shared_item = CharSet.inter (CharSet.inter e1_set e2_set) e3_set in
        assert (CharSet.cardinal shared_item = 1);
        let item = List.nth (CharSet.elements shared_item) 0 in
        aux t (a + char_priority item)
    | [ _; _ ] | _ :: [] -> raise Parser_error
  in
  aux l 0

let part1 filename =
  Utils.read_lines filename |> List.fold_left (fun a e -> a + item_priority e) 0

let part2 filename = Utils.read_lines filename |> group_by_3
let () = part1 Sys.argv.(1) |> Printf.printf "day3-1: %i\n"
let () = part2 Sys.argv.(1) |> Printf.printf "day3-2: %i\n"
