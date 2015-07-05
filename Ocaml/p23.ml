let file = "p23.txt"
	     
let check_sum_f value curr_sum =

  (* check_sum intermediate function *)  
  let mod_value = 10000007 in
  let prod_value = 113 in
  let new_sum = ( (value + curr_sum) * prod_value ) mod mod_value in
  new_sum

let check_sum array =

  (* storing the sum in a ref *)
  let rez = ref 0 in

  (* x is the current element of the array *)
  Array.iter (fun x -> rez := (check_sum_f x !rez) ) array;

  !rez

let swap array i j =

  (*swap function *)  
  let temp = array.(i) in
  array.(i) <- array.(j);
  array.(j) <- temp
	     
let solve s_list =

  (* copying the list into the array *)
  let size = List.length s_list in
  let a = Array.make size (-1) in
  List.iteri (fun i x -> a.(i) <- int_of_string x) s_list;

  let swaps = ref 0 in (* nb of swaps *)

  for i = 0 to size - 2 do
    if (a.(i) > a.(i+1)) then (
      incr swaps;
      swap a i (i+1)
    )
  done;
       
  Printf.printf "\n%d %d" !swaps (check_sum a)	     	     


let () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

