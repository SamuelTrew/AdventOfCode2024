let parse_rules (ruleStr : string) =
  String.split_on_char '\n' ruleStr
  |> List.map (fun rule ->
    match String.split_on_char '|' rule with
    | [ a; b ] -> int_of_string a, int_of_string b
    | _ -> exit 1)
;;

let parse_manuals (manualStr : string) =
  String.split_on_char '\n' manualStr
  |> List.map (fun manual -> String.split_on_char ',' manual |> List.map int_of_string)
;;

let parse_data (input : string) =
  let ruleStr, manualStr =
    match Str.split (Str.regexp "\n\n") input with
    | [ rules; manuals ] -> rules, manuals
    | _ -> exit 1
  in
  parse_rules ruleStr, parse_manuals manualStr
;;

module IntSet = Set.Make (Int)
module RuleMap = Map.Make (Int)

let build_rule_map (rules : (int * int) list) =
  List.fold_left
    (fun acc (before, after) ->
      match RuleMap.find_opt before acc with
      | Some set -> RuleMap.add before (IntSet.add after set) acc
      | None -> RuleMap.add before (IntSet.add after IntSet.empty) acc)
    RuleMap.empty
    rules
;;

let rec is_valid_manual (rule_map : IntSet.t RuleMap.t) (manual : int list) =
  match manual with
  | [] -> false
  | _ :: [] -> true
  | a :: b :: rest ->
    let b_set_opt = RuleMap.find_opt a rule_map in
    (match b_set_opt with
     | Some b_set ->
       (match IntSet.find_opt b b_set with
        | Some _ -> is_valid_manual rule_map (b :: rest)
        | None -> false)
     | None -> false)
;;

(* ################################################################## *)

let part1 () =
  let rules, manuals =
    Utils.read_lines "inputs/5.txt" |> String.concat "\n" |> parse_data
  in
  let rule_map = build_rule_map rules in
  let valid_manuals = List.filter (is_valid_manual rule_map) manuals in
  List.map (fun man -> List.nth man (List.length man / 2)) valid_manuals |> Utils.sum_list
;;

(* ################################################################## *)

let sort_invalid (rule_map : IntSet.t RuleMap.t) (manuals : int list list) =
  List.map
    (List.sort (fun a b ->
       let b_set_opt = RuleMap.find_opt a rule_map in
       match b_set_opt with
       | Some b_set ->
         IntSet.find_opt b b_set |> Option.is_none |> Bool.to_int |> fun i -> (i * 2) - 1
       | None -> 1))
    manuals
;;

let part2 () =
  let rules, manuals =
    Utils.read_lines "inputs/5.txt" |> String.concat "\n" |> parse_data
  in
  let rule_map = build_rule_map rules in
  let invalid_manuals =
    List.filter (fun man -> is_valid_manual rule_map man |> not) manuals
  in
  let sorted = sort_invalid rule_map invalid_manuals in
  List.map (fun man -> List.nth man (List.length man / 2)) sorted |> Utils.sum_list
;;
