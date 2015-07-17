let file = "p38.txt" (* input file *)

let solve_eq a b c =
  (* list of string because i don't like the idea of printing ints (and or) strings *)

  let delta = b * b - 4 * a * c in
  let sqrt_delta = int_of_float (sqrt (abs_float(float_of_int delta))) in

  if (delta >= 0) then
    let x1 = (-b - sqrt_delta) / (2 * a) in
    let x2 = (-b + sqrt_delta) / (2 * a) in
    Printf.printf "%d %d; " (max x1 x2) (min x1 x2)
		  
  else 
    let x1 = (string_of_int (-b / (2*a))) ^ "+" ^ (string_of_int (sqrt_delta / (2*a))) ^ "i" in
    let x2 = (string_of_int (-b / (2*a))) ^ "-" ^ (string_of_int (sqrt_delta / (2*a))) ^ "i" in
    Printf.printf "%s %s; " x1 x2 

    (* [x1; x2] *)
          

let solve s_list =
  let open Printf in  

  let i_list = List.map int_of_string  s_list in   

  match i_list with    
  | a::b::c::[] -> solve_eq a b c    
		   
  | _ -> failwith "wrong format"
                         
let main () =
  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

