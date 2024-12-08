module CharMap = Map.Make (Char)

let char_coords (matrix : char list list) =
  List.mapi (fun y row -> List.mapi (fun x c -> c, (y, x)) row) matrix
  |> List.flatten
  |> List.filter (fun (c, _) -> c != '.')
;;

let update_map (c : char) (coord : int * int) (map : (int * int) list CharMap.t) =
  let curr_state = CharMap.find_opt c map in
  match curr_state with
  | Some state -> CharMap.add c (coord :: state) map
  | None -> CharMap.add c [ coord ] map
;;

let get_coord_groups (coord_pairs : (char * (int * int)) list) =
  List.fold_left (fun map (c, coord) -> update_map c coord map) CharMap.empty coord_pairs
  |> CharMap.bindings
  |> List.map snd
;;

let is_oob (y : int) (x : int) (height : int) (width : int) =
  y < 0 || x < 0 || y >= height || x >= width
;;

(* ################################################################## *)

let anti_nodes ((ay, ax) : int * int) ((by, bx) : int * int) =
  let dy, dx = ay - by, ax - bx in
  [ ay + dy, ax + dx; by - dy, bx - dx ]
;;

let rec collect_anti_nodes (coords : (int * int) list) =
  match coords with
  | fst :: snd :: rest ->
    List.concat
      [ anti_nodes fst snd
      ; collect_anti_nodes (fst :: rest)
      ; collect_anti_nodes (snd :: rest)
      ]
  | _ :: [] -> []
  | [] -> []
;;

let remove_oob (matrix : char list list) (anti : (int * int) list) =
  let height = List.length matrix in
  let width = List.length (List.nth matrix 0) in
  List.filter (fun (y, x) -> not (is_oob y x height width)) anti
;;

let part1 () =
  let matrix = Utils.read_lines "inputs/8.txt" |> List.map Utils.string_to_char_list in
  let coord_groups = char_coords matrix |> get_coord_groups in
  let anti_groups = List.map collect_anti_nodes coord_groups in
  let anti = List.flatten anti_groups |> remove_oob matrix |> Utils.set_of_list in
  Utils.CoordSet.cardinal anti
;;

(* ################################################################## *)

let rec hop_forever (y : int) (x : int) (dy : int) (dx : int) (height : int) (width : int)
  =
  if is_oob y x height width
  then []
  else (y, x) :: hop_forever (y + dy) (x + dx) dy dx height width
;;

let anti_nodes_forever
  (matrix : char list list)
  ((ay, ax) : int * int)
  ((by, bx) : int * int)
  =
  let height = List.length matrix in
  let width = List.length (List.nth matrix 0) in
  let dy, dx = ay - by, ax - bx in
  let up = hop_forever ay ax dy dx height width in
  let down = hop_forever by bx (-dy) (-dx) height width in
  List.concat [ up; down ]
;;

let rec collect_forever_anti_nodes (matrix : char list list) (coords : (int * int) list) =
  match coords with
  | fst :: snd :: rest ->
    List.concat
      [ anti_nodes_forever matrix fst snd
      ; collect_forever_anti_nodes matrix (fst :: rest)
      ; collect_forever_anti_nodes matrix (snd :: rest)
      ]
  | _ :: [] -> []
  | [] -> []
;;

let part2 () =
  let matrix = Utils.read_lines "inputs/8.txt" |> List.map Utils.string_to_char_list in
  let coord_groups = char_coords matrix |> get_coord_groups in
  let anti_groups = List.map (collect_forever_anti_nodes matrix) coord_groups in
  let anti = List.flatten anti_groups |> Utils.set_of_list in
  Utils.CoordSet.cardinal anti
;;
