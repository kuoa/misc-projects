let file = "p18.txt" (* input file *)

let square_root x times =
  (* finds the square root of x, after times iteration *)

  let rec loop x rez i =
    if (i = times) then
      Printf.printf "%.8f " rez
    else (
      let d = x /. rez in (* must have d ~~ rez since rez * rez ~~ x *)
      let new_rez = (d +. rez) /. 2.0 in
      loop  x new_rez  (i+1)
    )
  in loop x 1.0 0
        
  	      
let solve s_list =
  let open Printf in  
   
  match s_list with
  | x::times::[] ->
     square_root (float_of_string x) (int_of_string times)
     
  | _ -> failwith "wrong format in input file"

		  
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

