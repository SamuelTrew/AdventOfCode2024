let up_pass (prev : int) (curr : int) = prev < curr && curr - prev <= 3
let down_pass (prev : int) (curr : int) = prev > curr && prev - curr <= 3

let same_transition (f : int -> int -> bool) (diff : int) (report : string list) =
  let nums = List.map int_of_string report in
  let res =
    List.fold_left
      (fun (is_up, prev) curr -> is_up && f prev curr, curr)
      (true, List.hd nums + diff)
      nums
  in
  fst res
;;

let all_increasing = same_transition up_pass (-1)
let all_decreasing = same_transition down_pass 1

let is_valid_change (report : string list) =
  Bool.to_int (all_increasing report || all_decreasing report)
;;

let part1 () =
  let lines = Utils.read_file "inputs/2.txt" in
  let reports = List.map (String.split_on_char ' ') lines in
  Utils.sum_list (List.map is_valid_change reports)
;;

(* ################################################################## *)

let up_fail (prev : int) (curr : int) = prev >= curr || curr - prev > 3
let down_fail (prev : int) (curr : int) = prev <= curr || prev - curr > 3

let first_failure (f : int -> int -> bool) (diff : int) (report : string list) =
  let nums = List.map int_of_string report in
  let failed, index, _ =
    List.fold_left
      (fun (unsafe, index, prev) curr ->
        let new_unsafe = unsafe || f prev curr in
        new_unsafe, (if new_unsafe then index else index + 1), curr)
      (false, 0, List.hd nums + diff)
      nums
  in
  failed, index
;;

let first_up_failure = first_failure up_fail (-1)
let first_down_failure = first_failure down_fail 1

let list_without_i (report : string list) (i : int) =
  List.filteri (fun index _ -> i != index) report
;;

let is_valid_count (report : string list) =
  let up_failed, up_i = first_up_failure report in
  let down_failed, down_i = first_down_failure report in
  match up_failed && down_failed with
  | false -> 1
  | true ->
    Bool.to_int
      (all_increasing (list_without_i report up_i)
       || all_increasing (list_without_i report (up_i - 1))
       || all_decreasing (list_without_i report down_i)
       || all_decreasing (list_without_i report (down_i - 1)))
;;

let part2 () =
  let lines = Utils.read_file "inputs/2.txt" in
  let reports = List.map (String.split_on_char ' ') lines in
  Utils.sum_list (List.map is_valid_count reports)
;;
