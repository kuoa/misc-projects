(* what did we learn *)

(* bad idea to use lists, because you need to keep reversing them and it takes too long *)
(* when using arrays, do use some while loops *)

(* kinda sloopy code *)
(* Execution time: 4.644000s with (primes.(!i) * primes.(!i) < !curr_nb) *)
(* Execution time: 3.864000s with ((primes.(!i) < int_of_float (sqrt (float_of_int !curr_nb))) *)

let file = "p61.txt" (* input file *)	    

let give_primes nb =
  let primes = Array.make nb 2 in

  let curr_nb = ref 3 in (* current number beeing checked *)
  let i = ref 0 in       (* iterate over array *)
  let size = ref 1 in    (* number of primes in the array *)

  while (!size < nb) do    

    while ((primes.(!i) < int_of_float (sqrt (float_of_int !curr_nb))) &&
	     (!curr_nb mod primes.(!i) <> 0)) do
      incr i;

    done;
        
    if (!curr_nb mod primes.(!i) <> 0) then (
      primes.(!size) <- !curr_nb;
      incr size;
      curr_nb := !curr_nb + 2;
      i:= 0
    )
    else (
      curr_nb := !curr_nb + 2;
      i:= 0
    )	
  done;

  primes
    	             			
let main () =

  let primes = give_primes 200000 in
  
  Ca.read_line_l file
  |> List.iter (fun x ->  Printf.printf "%d " primes.( int_of_string x - 1));
  print_newline ()
		
let () = main ()  
