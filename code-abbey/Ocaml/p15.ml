let file = "p15.txt"
;;



let solve l =
  let i_list = List.map (int_of_string) l in

  match i_list with
  | h :: [] -> Printf.printf "%d %d" h h
  | h :: t -> (* not optimal since we use 2 folds, but i'm lazy *)
     Printf.printf "%d %d"
		   (List.fold_left max h t) (List.fold_left min h t)

  | _ -> failwith "wrong format"
;;


let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_line file in 
  solve sList;
  Printf.printf "\n"
