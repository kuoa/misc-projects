open Big_int

let file = "p67.txt" (* input file *) 

let fibonnaci_count x =
  let x = big_int_of_string x in

  let rec loop i prev acc =
    if (ge_big_int acc x) then
      Printf.printf "%d " (i+1)
		    
    else loop (i+1) (acc) (add_big_int acc prev)
  in loop 1 (unit_big_int) (unit_big_int)


let solve s_list =

  match s_list with
  | h::[] ->
     fibonnaci_count h

  | _ -> failwith "wrong format"

    	     	        
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  
  List.iter solve sList;
  Printf.printf "\n"
   
