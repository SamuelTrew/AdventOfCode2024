let direction (c : char) =
  match c with
  | '^' -> (-1, 0)
  | '>' -> (0, 1)
  | 'v' -> (1, 0)
  | '<' -> (0, -1)
  | _ -> exit 1

let rotate (c : char) =
  match c with
  | '^' -> '>'
  | '>' -> 'v'
  | 'v' -> '<'
  | '<' -> '^'
  | _ -> exit 1

(* ################################################################## *)

let rec progress (matrix: char list list) ((y, x): (int * int)) (c: char) (count : int) =
  let height = List.length matrix in
  let width = List.length (List.nth matrix 0) in
  let (dy, dx) = direction c in
  let new_pos = (y + dy, x + dx) in
  match new_pos with
  | (-1, _) -> count
  | (_, -1) -> count
  | pos when (fst pos) == height -> count
  | pos when (snd pos) == width -> count
  | pos when (List.nth (List.nth matrix (fst pos))) (snd pos) == '#' -> progress matrix new_pos (rotate c) count
  | _ -> progress matrix new_pos c (count + 1)

let part1 () =
  let matrix = Utils.read_lines "inputs/6.txt" |> List.map Utils.string_to_char_list in
  let start = List.nth (Utils.starting_points '^' matrix) 0 in
  progress matrix start '^' 1
;;

(* ################################################################## *)


let part2 () =
  0
;;
