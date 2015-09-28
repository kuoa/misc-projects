let file = "p26.txt" (* input file *) 

let rec gcd a b =
  if (a = 0) then
    b
  else gcd (b mod a) a
;;

let lcm a b =
  a * b / (gcd a b)
;;

let solve s_list =
  let i_list = List.map (int_of_string) s_list in

  match i_list with
  | a::b::[] ->
     Printf.printf "(%d %d) " (gcd a b) (lcm a b)

  | _ -> failwith "wrong format"

    	     	        
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input file in 
  List.iter solve sList;
  Printf.printf "\n"
   
