let file = "p81.txt" (* input file *)

let bit_count n =
 
  let rec loop n shift acc =
    if (shift < 0) then
      acc
    else (      
      let x = (n lsr shift) in (* left shifting *)
      if (x land 1 = 1) then   (* bitwise and *)
	(* i want to see the representation, so i'm using a list *)
	loop n (shift - 1) (1 :: acc) (* if it's 1 *)
      else
	loop n (shift - 1) (0 :: acc) (* if it's 0 *)
    )
  in (loop n 31 [])
     |> List.fold_left (+) 0
     |> Printf.printf "%d "
;;
  	      
let solve s_list =
  let open Printf in  

  let i_list = List.map int_of_string  s_list in
   
  match i_list with
  | h::[] ->
     bit_count h
     
  | _ -> failwith "wrong format in input file"
                         
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

