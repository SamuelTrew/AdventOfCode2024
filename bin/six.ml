let direction (c : char) =
  match c with
  | '^' -> -1, 0
  | '>' -> 0, 1
  | 'v' -> 1, 0
  | '<' -> 0, -1
  | _ -> exit 1
;;

let rotate (c : char) =
  match c with
  | '^' -> '>'
  | '>' -> 'v'
  | 'v' -> '<'
  | '<' -> '^'
  | _ -> exit 1
;;

(* ################################################################## *)

(* Define a comparison function for tuples of (int * int) *)
let compare_tuple (a1, b1) (a2, b2) =
  let cmp1 = compare a1 a2 in
  if cmp1 = 0 then compare b1 b2 else cmp1
;;

(* Create a set of (int * int) tuples *)
module IntPairSet = Set.Make (struct
    type t = int * int

    let compare = compare_tuple
  end)

let progress
  (matrix : char list list)
  ((y, x) : int * int)
  (c : char)
  (visited : IntPairSet.t)
  =
  let height = List.length matrix in
  let width = List.length (List.nth matrix 0) in
  let rec prog_help
    (matrix : char list list)
    ((y, x) : int * int)
    (c : char)
    (visited : IntPairSet.t)
    =
    let dy, dx = direction c in
    let new_pos = y + dy, x + dx in
    match new_pos with
    | -1, _ -> visited
    | _, -1 -> visited
    | pos when fst pos == height -> visited
    | pos when snd pos == width -> visited
    | pos when List.nth (List.nth matrix (fst pos)) (snd pos) == '#' ->
      prog_help matrix (y, x) (rotate c) visited
    | _ -> prog_help matrix new_pos c (IntPairSet.add new_pos visited)
  in
  prog_help matrix (y, x) c visited
;;

let part1 () =
  let matrix = Utils.read_lines "inputs/6.txt" |> List.map Utils.string_to_char_list in
  let start = List.nth (Utils.starting_points '^' matrix) 0 in
  progress matrix start '^' (IntPairSet.singleton start) |> IntPairSet.cardinal
;;

(* ################################################################## *)

(* Create a Map of (int * int) tuples -> char *)
module IntPairMap = Map.Make (struct
    type t = int * int

    let compare = compare_tuple
  end)

module CharSet = Set.Make (Char)

let print_set s =
  let elements = CharSet.elements s in
  List.iter (fun x -> Printf.printf "%c " x) elements;
  print_newline ()
;;

let update_set (curr_pos : int * int) (visited : CharSet.t IntPairMap.t) (c : char) =
  let curr_state = IntPairMap.find_opt curr_pos visited in
  match curr_state with
  (* Visited before, so add to state *)
  | Some state -> IntPairMap.add curr_pos (CharSet.add c state) visited
  (* Haven't visited before, so create state*)
  | None -> IntPairMap.add curr_pos (CharSet.singleton c) visited
;;

let rec find_progress
  (matrix : char list list)
  (curr_pos : int * int)
  (new_pos : int * int)
  (c : char)
  (visited : CharSet.t IntPairMap.t)
  =
  let height = List.length matrix in
  let width = List.length (List.nth matrix 0) in
  match new_pos with
  (* If we go out of bounds, our addition hasn't worked *)
  | -1, _ -> false
  | _, -1 -> false
  | pos when fst pos == height -> false
  | pos when snd pos == width -> false
  | pos when List.nth (List.nth matrix (fst pos)) (snd pos) == '#' ->
    find_loop matrix curr_pos (rotate c) (update_set curr_pos visited c)
  | _ -> find_loop matrix new_pos c (update_set curr_pos visited c)

and (* Needed for these 2 functions to be able to call each other*)
  find_loop
  (matrix : char list list)
  ((y, x) : int * int)
  (c : char)
  (visited : CharSet.t IntPairMap.t)
  =
  let dy, dx = direction c in
  let new_pos = y + dy, x + dx in
  let curr_state = IntPairMap.find_opt (y, x) visited in
  match curr_state with
  (* Visited before *)
  | Some state ->
    (match CharSet.mem c state with
     (* Visited before, going in that direction *)
     | true -> true
     | false -> find_progress matrix (y, x) new_pos c visited)
  (* Haven't visited before, so do checks like normal *)
  | None -> find_progress matrix (y, x) new_pos c visited
;;

let update_2d_list matrix (y : int) (x : int) =
  List.mapi
    (fun i row ->
      if y == i
      then
        List.mapi (fun j _ -> if x == j then '#' else List.nth (List.nth matrix i) j) row
      else List.nth matrix i)
    matrix
;;

(* Generates list of matrices with '#' in each possible coord *)
let generate_matrices (matrix : char list list) =
  List.mapi (fun i row -> List.mapi (fun j _ -> update_2d_list matrix i j) row) matrix
  |> List.flatten
;;

let part2 () =
  let matrix = Utils.read_lines "inputs/6.txt" |> List.map Utils.string_to_char_list in
  let start = List.nth (Utils.starting_points '^' matrix) 0 in
  let found =
    List.map
      (fun m -> find_loop m start '^' IntPairMap.empty |> Bool.to_int)
      (generate_matrices matrix)
  in
  Utils.sum_list found
;;
