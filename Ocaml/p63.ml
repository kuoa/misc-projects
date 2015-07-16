let file = "p63.txt" (* input file *)


let prime_factors n =

  let rec loop n d flist =
    if (n = 1) then List.rev flist else
      if (n mod d = 0) then

	match flist with
	| (f, nb) :: t  when (f = d) ->
	   loop (n / d) d ( (f, nb+1) :: t)

	(* includes [] and when f is not in the list *)
	| l -> loop (n / d) d ( (d, 1) :: l) 
    else
      loop n (d+1) flist
  in loop n 2 []

let print_factors flist =
  List.iter (fun (f, nb) -> for i = 0 to (nb-1) do Printf.printf "%d*" f done;) flist;
  print_char ' '

	     
let solve slist =
  slist
  |> List.map int_of_string
  |> List.iter (fun x -> print_factors (prime_factors x) )
    
                                  
let main () =
  (* reading the input, string list list *)
  Ca.read_input_l file

  |> List.iter solve;
  
  print_newline ()

let () = main ()	         
