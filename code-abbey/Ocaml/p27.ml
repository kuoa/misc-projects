let file = "p27.txt" (* input file *)

let swap array i j =

  (*swap function *)  
  let temp = array.(i) in
  array.(i) <- array.(j);
  array.(j) <- temp
;;
	     
let solve s_list =

  (* copying the list into the array *)
  let size = List.length s_list in
  let a = Array.make size (-1) in
  List.iteri (fun i x -> a.(i) <- int_of_string x) s_list;

  let swapped = ref true in (* has a swap been made? *)
  let times = ref 0 in (* nb of passes *)
  let swaps = ref 0 in (* nb of swaps *)

  while (!swapped) do
    swapped := false;

    for i = 0 to size - 2 do
      if (a.(i) > a.(i+1)) then (
	swapped := true;
	incr swaps;
	swap a i (i+1)
      )
    done;
    
    if (!swapped) then
      incr times
    
  done;    

  Array.iter (Printf.printf "%d " ) a; 
  Printf.printf "\n%d %d" (!times + 1) !swaps	     	     
;;
  

  
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  
  List.iter solve sList;
  Printf.printf "\n"
   
