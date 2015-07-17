let file = "p17.txt" (* input file *)

let check_sum_f value curr_sum =
  let mod_value = 10000007 in
  let prod_value = 113 in
  let new_sum = ( (value + curr_sum) * prod_value ) mod mod_value in
  new_sum

let check_sum list =

  (* storing the sum in a ref *)
  let rez = ref 0 in

  (* x is the current element of the list *)
  List.iter (fun x -> rez := (check_sum_f x !rez) ) list;

  !rez

  
let solve s_list =

  (* string list -> int list *)
  let i_list = List.map (int_of_string) s_list in
  let value = (check_sum i_list) in
  Printf.printf "%d\n" value
  	        
		   		     
let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  List.iter solve sList;
  Printf.printf "\n"
   
