let file = "p69.txt" (* input file *)

let find_index n =
  let open Big_int in
  let zero = zero_big_int in
  let one = unit_big_int in
  let n = big_int_of_int n in

  let rec loop i n1 n2 =    
    if ( eq_big_int (mod_big_int n1 n) zero) then
     Printf.printf "%d " i
    else
      loop (i+1) n2 (add_big_int n1 n2)
	   
  in loop 1 one one


let solve slist =
  slist
  |> List.map int_of_string
  |> List.iter find_index

    
                                  
let main () =

  (* reading the input, string list list *)
  Ca.read_line_l file
  |> solve;  
  print_newline ()

		
let () = main ()	         
