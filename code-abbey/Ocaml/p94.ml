let file = "p94.txt" (* input file *)
	       
let solve s_list =

  s_list
  |> List.map int_of_string
  |> List.fold_left (fun base x -> base + x * x) 0
  |> Printf.printf "%d "

			
let main () =
  
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()  
