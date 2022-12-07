(*
day5-1: SPFMVDTZT
day5-2: ZFSJBPRFP
*)

exception Parse_error

let int_of_list_index n l = int_of_string @@ List.nth l n

type move = {
  items : int;
  src : int;
  dst : int;
}

let pp_move : move Fmt.t =
  let open Fmt in
  record
    [
      field "items" (fun e -> e.items) int;
      field "src" (fun e -> e.src) int;
      field "dst" (fun e -> e.dst) int;
    ]

let parse_move s =
  let l = String.split_on_char ' ' s in
  {
    items = int_of_list_index 1 l;
    src = int_of_list_index 3 l;
    dst = int_of_list_index 5 l;
  }

let is_stack_line s =
  let s = String.trim s in
  String.starts_with ~prefix:"[" s && String.ends_with ~suffix:"]" s

let get_stack_number l =
  let count_stacks s =
    String.fold_left (fun a c -> if c = '[' then a + 1 else a) 0 s
  in
  List.fold_left
    (fun a e ->
      let stacks = count_stacks e in
      if stacks > a then stacks else a)
    0 l

let prepare_stacks num_stacks =
  assert (num_stacks > 0);
  let h = Hashtbl.create 20 in
  for i = 0 to num_stacks - 1 do
    Hashtbl.add h i (Stack.create ())
  done;
  h

let get_stack_offset n = if n = 0 then 1 else (n * 4) + 1

let parse_stack_crate i s =
  try
    match String.get s (get_stack_offset i) with
    | ' ' -> None
    | 'A' .. 'Z' as c -> Some c
    | _ -> raise Parse_error
  with (* Out of bounds *)
  | Invalid_argument _ -> None

let parse_stack_line s num_stacks =
  assert (num_stacks > 0);
  let rec aux a i =
    if i = num_stacks then a else aux (parse_stack_crate i s :: a) (i + 1)
  in
  aux [] 0 |> List.rev

let fill_stacks stacks stack_lines =
  let num_stacks = Hashtbl.length stacks in
  List.iter
    (fun e ->
      List.iteri
        (fun i e' ->
          match e' with
          | None -> ()
          | Some c -> Stack.push c (Hashtbl.find stacks i))
        (parse_stack_line e num_stacks))
    stack_lines

let populate_move stacks move =
  let src = Hashtbl.find stacks (move.src - 1) in
  let dst = Hashtbl.find stacks (move.dst - 1) in
  for i = 1 to move.items do
    Stack.push (Stack.pop src) dst
  done

let populate_move_multiple stacks move =
  let temp_stack = Stack.create () in
  let src = Hashtbl.find stacks (move.src - 1) in
  let dst = Hashtbl.find stacks (move.dst - 1) in
  for i = 1 to move.items do
    Stack.push (Stack.pop src) temp_stack;
  done;
  for i = 1 to move.items do
    Stack.push (Stack.pop temp_stack) dst;
  done

let string_top_of_the_stacks stacks =
  let num_stacks = Hashtbl.length stacks in
  let rec aux a i =
    if i = num_stacks then a
    else
      aux (a ^ Printf.sprintf "%c" (Stack.top (Hashtbl.find stacks i))) (i + 1)
  in
  aux "" 0

let part1 filename =
  let moves =
    Utils.read_lines filename
    |> List.filter (fun e -> String.starts_with ~prefix:"move " e)
    |> List.fold_left (fun a e -> parse_move e :: a) []
    |> List.rev
  in
  let stack_lines = Utils.read_lines filename |> List.filter is_stack_line in
  let num_stacks = get_stack_number stack_lines in
  let stacks = prepare_stacks num_stacks in
  fill_stacks stacks (List.rev stack_lines);
  List.iter (fun e -> populate_move stacks e) moves;
  string_top_of_the_stacks stacks

let part2 filename =
  let moves =
    Utils.read_lines filename
    |> List.filter (fun e -> String.starts_with ~prefix:"move " e)
    |> List.fold_left (fun a e -> parse_move e :: a) []
    |> List.rev
  in
  let stack_lines = Utils.read_lines filename |> List.filter is_stack_line in
  let num_stacks = get_stack_number stack_lines in
  let stacks = prepare_stacks num_stacks in
  fill_stacks stacks (List.rev stack_lines);
  List.iter (fun e -> populate_move_multiple stacks e) moves;
  string_top_of_the_stacks stacks

let () = part1 Sys.argv.(1) |> Printf.printf "day5-1: %s\n"
let () = part2 Sys.argv.(1) |> Printf.printf "day5-2: %s\n"
