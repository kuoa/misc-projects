let file = "p128.txt" (* input file *)
       
let chose n k =
  (* using BigInt, as max_int in ocaml is 2^63 *)
  
  let open Big_int in
  let n = big_int_of_int n in
  let k = big_int_of_int k in
  let one = unit_big_int in

  let rec loop d r n =    
    if (ge_big_int d (succ_big_int k)) then
      r
    else loop (succ_big_int d)
	      ( div_big_int (mult_big_int r n) d ) (pred_big_int n)
  in loop one one n
  	      
let solve s_list =
  let open Printf in  

  let i_list = List.map int_of_string  s_list in
   
  match i_list with
  | n::k::[] ->
     Printf.printf "%s " (Big_int.string_of_big_int (chose n k))
     
  | _ -> failwith "wrong format"
                         
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

