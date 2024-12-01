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
let read_file (file_name : string) : string = read_lines file_name |> String.concat "\n"
