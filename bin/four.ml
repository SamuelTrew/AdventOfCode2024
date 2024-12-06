let directions = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ]

let get_coord (matrix : char list list) (y : int) (x : int) =
  List.nth (List.nth matrix y) x
;;

(* Checks that the given points around (y,x) are valid at given mult *)
let in_bound_points
  (matrix : char list list)
  ((y, x) : int * int)
  (points : (int * int) list)
  (mult : int)
  =
  let height = List.length matrix - 1 in
  let width = List.length (List.nth matrix 0) - 1 in
  List.filter
    (fun (ud, lr) ->
      let end_y = y + (ud * mult) in
      let end_x = x + (lr * mult) in
      end_y >= 0 && end_y <= height && end_x >= 0 && end_x <= width)
    points
;;

let count_for_coord (matrix : char list list) ((y, x) : int * int) =
  let get_m_coord = get_coord matrix in
  let viable_dirs = in_bound_points matrix (y, x) directions 3 in
  List.map
    (fun (ud, lr) ->
      let my, mx = ud + y, lr + x in
      let ay, ax = (2 * ud) + y, (2 * lr) + x in
      let sy, sx = (3 * ud) + y, (3 * lr) + x in
      Bool.to_int
        (get_m_coord my mx == 'M' && get_m_coord ay ax = 'A' && get_m_coord sy sx = 'S'))
    viable_dirs
  |> Utils.sum_list
;;

let part1 () =
  let lines = Utils.read_lines "inputs/4.txt" in
  let matrix = List.map Utils.string_to_char_list lines in
  let coords = Utils.starting_points 'X' matrix in
  List.map (count_for_coord matrix) coords |> Utils.sum_list
;;

(* ################################################################## *)

let xmas = [ 'M'; 'M'; 'S'; 'S' ]
let corners = [ -1, -1; -1, 1; 1, 1; 1, -1 ] (* tl tr br bl *)

let rec rotate (lst : char list) (count : int) =
  match lst, count with
  | l, 0 -> l
  | [ a; b; c; d ], count -> rotate [ b; c; d; a ] (count - 1)
  | _ -> exit 1
;;

let rotations = [ xmas; rotate xmas 1; rotate xmas 2; rotate xmas 3 ]

(* For each rotation find if any are valid *)
let valid_corners (matrix : char list list) ((y, x) : int * int) =
  let get_m_coord = get_coord matrix in
  List.find_opt
    (fun order ->
      List.mapi
        (fun i (cy, cx) -> get_m_coord (cy + y) (cx + x) == List.nth order i)
        corners
      |> List.fold_left ( && ) true)
    rotations
  |> Option.is_some
  |> Bool.to_int
;;

let part2 () =
  let lines = Utils.read_lines "inputs/4.txt" in
  let matrix = List.map Utils.string_to_char_list lines in
  let coords = Utils.starting_points 'A' matrix in
  let viable_coords =
    List.filter
      (fun (y, x) ->
        let in_bound_corners = in_bound_points matrix (y, x) corners 1 in
        List.length in_bound_corners == List.length corners)
      coords
  in
  List.map (valid_corners matrix) viable_coords |> Utils.sum_list
;;
