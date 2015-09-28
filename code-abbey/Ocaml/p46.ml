let file = "p46.txt" (* input file *)

exception Game_won
exception Game_not_won

let size = 3
     	
let check a =
  let r1 = ref 0 in (* first rezult *)
  let r2 = ref 0 in (* second rezult *)

  try
    for i = 0 to size - 1 do
      for j = 0 to size - 1 do
	(* check w | h *)
	r1 := !r1 + a.(i).(j);
	r2 := !r2 + a.(j).(i);
      done;

      if ( abs !r1 = size || abs !r2 = size) then
	raise Game_won
      else (
	r1 := 0;
	r2 := 0;
      )
    done;

    
    for i = 0 to size - 1 do
      (* diagonal || antidiagonal *)
      r1 := !r1 + a.(i).(i);
      r2 := !r2 + a.(i).(size - 1 - i);
    done;

    if ( abs !r1 = size || abs !r2 = size) then
      raise Game_won
    else
      raise Game_not_won
    
  with
  | Game_won -> true
  | Game_not_won -> false


		      
let add a nb turn =

  let i = (nb - 1) / size in
  let j = (nb - 1) mod size  in

  if (turn mod 2 = 0) then
    a.(i).(j) <- 1
  else
    a.(i).(j) <- (-1)
  

		   
let solve s_list =
  let grid = Array.make_matrix size size 0 in
  let i_list = List.map (int_of_string) s_list in

  try
    let rec loop i = function
      | [] -> Printf.printf "0 "
      | h::t ->      
	 add grid h i;
	 
	 if (check grid = true) then (
	   Printf.printf "%d " i;
	   raise Game_won
	 )
				       
	 else
	   loop (i+1) t
    in loop 1 i_list
  with
  | Game_won -> ()
      		         	 		 				    

                  
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

		
let () = main ()	         


	      
let print_array a =

  for i = 0 to Array.length a - 1 do
    for j = 0 to Array.length a - 1 do
      Printf.printf "%d " a.(i).(j)
    done;
   Printf.printf "\n"
  done;

  Printf.printf "\n"
