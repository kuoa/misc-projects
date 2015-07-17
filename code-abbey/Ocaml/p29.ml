let file = "p29.txt" (* input file *) 

let original_index o_list =

  (* sort the list *)
  let sort_list = List.sort compare o_list in

  let find_index x o_list =

    (* find the index of x in the orignal list *)
    (* not optimal since List.iteri traverses all the elements *)
    (* but i'm lazy to create own function *)
  
    List.iteri (fun i el ->
		if (el = x) then (Printf.printf "%d " (i+1))
	       )
	       o_list in

  (* find the index of all elements in the orignal list *)
  List.iter (fun x -> find_index x o_list) sort_list
	     
let solve s_list =
  let i_list = List.map (int_of_string) s_list in

  original_index i_list
    	     	        
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input file in 
  List.iter solve sList;
  Printf.printf "\n"
   
