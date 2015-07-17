let file = "p59.txt" (* input file *)

let bulls_and_cows base guess =
  let rez  = ref (0, 0) in 
  let l = String.length base in

  for i = 0 to (l - 1) do
    for j = 0 to (l - 1) do     
      let (r, w) = !rez in

      if (base.[i] = guess.[j]) then
	if (i = j) then 
	  rez := (r+1, w)	
	else 
	  rez := (r, w+1)
		   
    done;	 
  done;

  !rez  	     
  
let solve s_list =
  let base = "6921" in

  match s_list with
  | [guess] ->
     let (r, w) = bulls_and_cows base guess in
     Printf.printf "%d-%d " r w

  | _ -> failwith "Wrong format in solve" 
		  
			
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()  
