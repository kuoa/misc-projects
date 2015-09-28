let file = "p53.txt" (* input file *)
	     
let q_t_k p1 p2 =

  (* actual coordonates in chars *)
  let kx = p1.[0] in
  let ky = p1.[1] in
  let qx = p2.[0] in
  let qy = p2.[1] in

  (* same row or same column *)
  if ( (kx = qx) || (ky = qy) ) then
    true
  else
    (*  diagonals *)
    let d1 = abs ((int_of_char kx) - (int_of_char qx)) in
    let d2 = abs ((int_of_char ky) - (int_of_char qy)) in
    if ( d1 = d2 ) then
      true
    else
      false
    

	
let solve slist =

  match slist with
  | p1::p2::[] ->
     let quenTakesKing = q_t_k p1 p2 in
     if (quenTakesKing) then
       Printf.printf "Y "
     else
       Printf.printf "N "
    
  | _ -> failwith "Wrong reading format in solve"
  

                                      
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;  
  print_newline ()

		
let () = main ()	         
