let parse_input (equation : string) =
  let pair = Str.split (Str.regexp ": ") equation in
  match pair with
  | [ result; inputs ] ->
    int_of_string result, String.split_on_char ' ' inputs |> List.map int_of_string
  | _ -> exit 1
;;

(* ################################################################## *)

let rec apply_operation ((result, inputs) : int * int list) =
  match inputs with
  | fst :: snd :: rest ->
    (* Attempt addition and if that doesn't work attempt multiplication *)
    let add = apply_operation (result, (fst + snd) :: rest) in
    if add then true else apply_operation (result, (fst * snd) :: rest)
  | fst :: [] -> result == fst
  | [] -> false
;;

let part1 () =
  let equations = List.map parse_input (Utils.read_lines "inputs/7.txt") in
  List.filter apply_operation equations |> List.map fst |> Utils.sum_list
;;

(* ################################################################## *)

let rec int_pow (power : int) =
  match power with
  | 0 -> 1
  | 1 -> 10
  | pow -> 10 * int_pow (pow - 1)
;;

let rec apply_operation_concat ((result, inputs) : int * int list) =
  match inputs with
  | fst :: snd :: rest ->
    (* Attempt addition and if that doesn't work attempt multiplication *)
    if apply_operation_concat (result, (fst + snd) :: rest)
    then true (* Attempt multiplication and if that doesn't work attempt concatention *)
    else if apply_operation_concat (result, (fst * snd) :: rest)
    then true
    else (
      let power = string_of_int snd |> String.length in
      let fst_part = fst * int_pow power in
      apply_operation_concat (result, (fst_part + snd) :: rest))
  | fst :: [] -> result == fst
  | [] -> false
;;

let part2 () =
  let equations = List.map parse_input (Utils.read_lines "inputs/7.txt") in
  List.filter apply_operation_concat equations |> List.map fst |> Utils.sum_list
;;
