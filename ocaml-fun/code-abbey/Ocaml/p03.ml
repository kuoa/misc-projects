let file = "p3.txt"
;;

let solve l =
  let i_list = List.map (int_of_string) l in
  Printf.printf "%d " (List.fold_left (+) 0 i_list)
;;

let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in 
  List.iter solve sList;
  Printf.printf "\n"
