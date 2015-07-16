let file = "p33.txt" (* input file *)


let int_to_bin n =
  (* returns a list containing the bynary notation of n *)

  let rec loop n shift acc =
    if (shift < 0) then
      List.rev acc
    else (      
      let x = (n lsr shift) in (* left shifting *)
      if (x land 1 = 1) then   (* bitwise and *)
	loop n (shift - 1) (1 :: acc) (* if it's 1 *)
      else
	loop n (shift - 1) (0 :: acc) (* if it's 0 *)
    )
  in (loop n 7 [])



let bin_to_int l =
  (* transforms a list containing a binary notation to a int *)
  (* we ignore the highest bit *)

  let len = List.length l - 1 in
  let l = List.map (float_of_int) l in

  let rec loop l i acc =
    match l with
    | h :: t ->
       loop t (i - 1) (acc +. h *. (2. ** (float_of_int i)))

    | [] -> int_of_float acc

  in loop l len 0.


	  
let solve n =
  let bin_list = int_to_bin n in (* binary notation *)
  let sum = List.fold_left (+) 0 bin_list in (* sum of bits *)

  if (sum mod 2 = 0) then (* if the sum of bits is pair, print the coresponding char *)
    print_char (char_of_int (bin_to_int (List.tl bin_list)))

    
                                  
let main () =
  (* reading the input, string list list *)
  Ca.read_line_l file
  |> List.map int_of_string
  |> List.iter solve;
  
  print_newline ()

let () = main ()
  
