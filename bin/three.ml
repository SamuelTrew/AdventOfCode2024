let find_all_matches str =
  (* Matches mul(int, int) where int is a group *)
  let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let rec loop start_idx acc =
    try
      (* Find the next match *)
      let _ = Str.search_forward regex str start_idx in
      (* Get the numbers from the regex group *)
      let fst = Str.matched_group 1 str |> int_of_string in
      let snd = Str.matched_group 2 str |> int_of_string in
      (* Continue to next match *)
      loop (Str.match_end ()) ((fst, snd) :: acc)
    with
    | Not_found -> List.rev acc  (* Return the collected matches in the correct order *)
  in
  loop 0 []
;;

let count input =
  let matches = find_all_matches input in
  (* Print matches with newlines for better readability *)
  Utils.sum_list (List.map (fun (a,b) -> a*b) matches)
;;

let part1 () =
  Utils.read_file "inputs/3.txt" |> String.concat "" |> count
;;

(* ################################################################## *)

let filter_dont str =
  let splits = Str.split (Str.regexp "do()") str in
  let good = List.map (fun split -> List.nth (Str.split (Str.regexp "don't()") split) 0) splits in
  String.concat "" good
;;

let part2 () =
  Utils.read_file "inputs/3.txt" |> String.concat "" |> filter_dont |> count
;;
