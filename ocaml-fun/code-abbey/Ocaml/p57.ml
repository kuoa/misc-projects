let file = "p57.txt" (* input file *) 

let average a b c =
  (a +. b +. c) /. 3.0
;;
	     
let solve s_array =
  
  let size = Array.length s_array in
  let old_array = Array.map (float_of_string) s_array in
  let new_array = Array.copy old_array in
   
  for i = 1 to size - 2 do
    new_array.(i) <-
      (average old_array.(i-1) old_array.(i) old_array.(i+1) )
  done;

  (* before *)
  (*Array.iter (Printf.printf "%.7f ") old_array;  *)
  Printf.printf "\n";
  (* after *)
  Array.iter (Printf.printf "%.8f ") new_array;

  
;;
    	     	        
let () =

  (* reading the input, string list list *)
  let s_array = Ca.read_line_a file in 
  solve s_array;
  Printf.printf "\n"
   
