  
let file = "p16.txt" (* input file *)
  
let solve sList =

  (* string List -> float List *)
  let fList = List.map (float_of_string) sList in
  let size = float_of_int (List.length fList) in

  (* using List.fold_left to compute the sum *)
  let sum = (List.fold_left (+.) 0. fList) in

  (* computing average *)
  let rez =  sum /. (size -. 1.0)
  in Printf.printf "%d " (int_of_float (rez +. 0.5))		   		   
  
let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  
  List.iter solve sList;
  Printf.printf "\n"
		
