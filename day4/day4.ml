(*
day4-1: 605
day4-2: 914
*)

module IntSet = Set.Make (Int)

let list_of_range x y =
  let rec aux a i = if i = y then i :: a else aux (i :: a) (i + 1) in
  aux [] x

let get_elve_sections s =
  let sections = String.split_on_char '-' s in
  assert (List.length sections = 2);
  let x = List.nth sections 0 |> int_of_string in
  let y = List.nth sections 1 |> int_of_string in
  list_of_range x y |> IntSet.of_list

let part1 s =
  let pairs = String.split_on_char ',' s in
  assert (List.length pairs = 2);
  let elve_1_sections = List.nth pairs 0 |> get_elve_sections in
  let elve_2_sections = List.nth pairs 1 |> get_elve_sections in
  let overlap = IntSet.inter elve_1_sections elve_2_sections in
  if
    IntSet.cardinal overlap = IntSet.cardinal elve_1_sections
    && IntSet.cardinal elve_2_sections >= IntSet.cardinal overlap
    || IntSet.cardinal overlap = IntSet.cardinal elve_2_sections
       && IntSet.cardinal elve_1_sections >= IntSet.cardinal overlap
  then 1
  else 0

let part2 s =
  let pairs = String.split_on_char ',' s in
  assert (List.length pairs = 2);
  let elve_1_sections = List.nth pairs 0 |> get_elve_sections in
  let elve_2_sections = List.nth pairs 1 |> get_elve_sections in
  let overlap = IntSet.inter elve_1_sections elve_2_sections in
  if IntSet.cardinal overlap <> 0 then 1 else 0

let part1 filename =
  Utils.read_lines filename |> List.fold_left (fun a e -> a + part1 e) 0

let part2 filename =
  Utils.read_lines filename |> List.fold_left (fun a e -> a + part2 e) 0

let () = part1 Sys.argv.(1) |> Printf.printf "day4-1: %i\n"
let () = part2 Sys.argv.(1) |> Printf.printf "day4-2: %i\n"
