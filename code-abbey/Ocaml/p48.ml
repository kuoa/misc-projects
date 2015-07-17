let file = "p48.txt" (* input file *)

let colatz_f x =

  (* counting the number of steps *)
  let rec loop x acc =
    if (x = 1) then acc
    else if (x mod 2 = 0) then
      loop (x / 2) (acc + 1)
    else
      loop (3 * x + 1) (acc + 1)
  in loop x 0
	   

let solve s_list =

  (* string list -> int list *)
  let i_list = List.map (int_of_string) s_list in
  List.iter (fun x -> Printf.printf "%d " (colatz_f x) ) i_list	      
  	        
		   		     
let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  List.iter solve sList;
  Printf.printf "\n"
   
