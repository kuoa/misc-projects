let file = "p21.txt" (* input file *)
  
let solve s_list =

  (* string list -> int list *)
  let i_list = List.map (int_of_string) s_list in

  match i_list with
  | [] -> ()

  | h::t ->
     (* max value defines the size of the array *)
     let max_value = List.fold_left max h t in
     let freq = Array.make (max_value + 1) 0 in

     (* side effect on freq array *)
     List.iter (fun x -> (freq.(x) <- freq.(x) + 1) ) i_list;
     (* printing the array *)
     Array.iter (fun x -> if (x <> 0) then
			    Printf.printf "%d " x) freq
;;
  	        
		   		     
let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  List.iter solve sList;
  Printf.printf "\n"
   
