let file = "p44.txt" (* input file *)

let sum_dice x y = (1 + x mod 6) + (1 + y mod 6)
		     	     
let solve s_list =

  match s_list with
  | x::y::[] ->
     let res = sum_dice (int_of_string x) (int_of_string y) in
     Printf.printf "%d " res
     
  | _ -> failwith "wrong format in input file"
                         
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

