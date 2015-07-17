let file = "p31.txt" (* input file *)

let rotate_string x s =
  let open Printf in  

  let bs = Bytes.of_string s in
  let l = Bytes.length bs in    	      

  if (x > 0) then (
    let s1 = Bytes.sub bs 0  x in
    let s2 = Bytes.sub bs x (l-x) in
    
    let rez =  Bytes.cat s2 s1 in
    printf "%s " (Bytes.to_string rez)
  )

  else (
    let x = abs x in
    let s1 = Bytes.sub bs (l-x)  x in
    let s2 = Bytes.sub bs 0 (l-x)  in
    
    let rez =  Bytes.cat s1 s2 in
    printf "%s " (Bytes.to_string rez)
  );;
  
  
	     
  
let solve s_list =
  let open Printf in
 
  match s_list with
  | i::s::[] ->     
     rotate_string (int_of_string (i)) s
     
  | _ -> failwith "wrong format"
                         
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input_l file in 
  List.iter solve sList;
  Printf.printf "\n"
