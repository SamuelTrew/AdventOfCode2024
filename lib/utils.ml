(* File Handling *)

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

(* reading bytes directly would be more efficient *)
let read_file (file_name : string) : string list = read_lines file_name

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
