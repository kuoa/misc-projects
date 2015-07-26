let file = "p37.txt" (* input file *)

let mortage_calc p r m =
  let rate = r /. 1200. in
  let sum = p *. (rate *. ((1. +. rate) ** m)) /.
	      ((1. +. rate) ** m -. 1.) in 
  print_float (ceil sum);
  print_newline ()
	
    
let solve p r m = mortage_calc p r m

			       
let main () =  
  (* reading the input, string list list *)
  let input = Ca.read_line_l file in
  let f_input = List.map float_of_string input in
  match f_input with
  | p::r::m::[] -> solve p r m

  | _ -> failwith "Wrong format"
    
  

let () = main ()	         
