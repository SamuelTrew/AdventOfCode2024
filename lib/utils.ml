let read_lines (file_name : string) : string list =
  let chan = open_in file_name in
  let rec read_lines (chan : in_channel) : string list =
    try
      let line = input_line chan in
      line :: read_lines chan
    with
    | End_of_file -> []
  in
  let lines = read_lines chan in
  close_in chan;
  lines
;;

module IntMap = Map.Make (Int)

let count_frequencies lst =
  List.fold_left
    (fun map x ->
      let current_count =
        try IntMap.find x map with
        | Not_found -> 0
      in
      IntMap.add x (current_count + 1) map)
    IntMap.empty
    lst
;;

let sum_list = List.fold_left ( + ) 0
let string_to_char_list (s : string) = s |> String.to_seq |> List.of_seq

(* Returns all coords of characters matching chr *)
let starting_points (chr : char) (input : char list list) : (int * int) list =
  List.mapi
    (fun y row -> List.mapi (fun x curr -> if curr == chr then Some (y, x) else None) row)
    input
  |> List.flatten
  |> List.filter_map (fun x -> x)
;;

let get_coord (matrix : char list list) (y : int) (x : int) =
  List.nth (List.nth matrix y) x
;;

(* Define a comparison function for tuples of (int * int) *)
let compare_tuple (a1, b1) (a2, b2) =
  let cmp1 = compare a1 a2 in
  if cmp1 = 0 then compare b1 b2 else cmp1
;;

(* Create a set of (int * int) tuples *)
module CoordSet = Set.Make (struct
    type t = int * int

    let compare = compare_tuple
  end)

let set_of_list (li : (int * int) list) =
  List.fold_left (fun set elem -> CoordSet.add elem set) CoordSet.empty li
;;
