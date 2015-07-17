let file = "p5.txt"
;;

let solve l =
  let i_list = List.map (int_of_string) l in

  match i_list with
  | h :: [] -> Printf.printf "%d " h
  | h :: t ->  Printf.printf "%d " (List.fold_left min h t)
  | _ -> failwith "wrong format"
;;

let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in 
  List.iter solve sList;
  Printf.printf "\n"
