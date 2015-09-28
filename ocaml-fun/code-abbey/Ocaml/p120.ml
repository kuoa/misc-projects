let file = "p120.txt" (* input file *)

let swap array i j =
  let temp = array.(i) in
  array.(i) <- array.(j);
  array.(j) <- temp


let selection_sort a =
  let l = ref (Array.length a - 1) in
  let max = ref (a.(0)) in
  let ind = ref 0 in

  for i = 0 to (Array.length a - 1) do
    for j = 0 to !l do

      if (a.(j) > !max) then (

	(* update max and position *)	
	max := a.(j);
	ind := j
	)
    done;

    (* research done, update data *)
    swap a !ind !l;
    decr l;
    Printf.printf "%d " !ind;
    ind := 0;
    max := a.(0)
      
  done
    
           
let main () =
  (* reading the input, string list list *)
  Ca.read_line_a file
  |> Array.map (int_of_string)
  |> selection_sort;
  
  print_newline ()

let () = main ()
  
