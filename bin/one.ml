let get_pairs (line : string) =
  let l = Str.split (Str.regexp_string "   ") line in
  match l with
  | [ a; b ] -> int_of_string a, int_of_string b
  | _ -> Int.max_int, Int.max_int
;;

let rec sum_list (list : int list) =
  match list with
  | a :: b :: rest -> Int.abs a + Int.abs b + sum_list rest
  | a :: [] -> Int.abs a
  | [] -> 0
;;

let part1 () =
  let lines = Utils.read_lines "inputs/1.txt" in
  let pairs = List.map get_pairs lines in
  let sorted1 = List.fast_sort Int.sub (List.map fst pairs) in
  let sorted2 = List.fast_sort Int.sub (List.map snd pairs) in
  let diff = List.map2 Int.sub sorted1 sorted2 in
  sum_list diff
;;

(* ################################################################## *)

let similarity (counter : int Utils.IntMap.t) (item : int) : int =
  item
  *
  try Utils.IntMap.find item counter with
  | Not_found -> 0
;;

let part2 () =
  let lines = Utils.read_lines "inputs/1.txt" in
  let pairs = List.map get_pairs lines in
  let counter = Utils.count_frequencies (List.map snd pairs) in
  sum_list (List.map (similarity counter) (List.map fst pairs))
;;
