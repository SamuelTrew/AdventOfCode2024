let time f =
  let t = Sys.time () in
  let fx = f () in
  Printf.printf "\nExecution time: %fms\n" ((Sys.time () -. t) *. float_of_int 1000);
  Printf.printf "%i\n" fx
;;

time Six.part1;
time Six.part2
