open Big_int
(* using Big_int, can also use Int64, never used Big_int though *)      
let file = "p14.txt" (* input file *)

let f op value acc =
  (* value and acc are Big_int *)
  
  match op with
  | "+" -> acc := (add_big_int !acc value)
  | "*" -> acc := (mult_big_int !acc value)
  | "%" -> acc := (mod_big_int !acc value)
  | _ -> failwith "non defined operator"
    		  
;;
      	   
let solve s_list rez =  
  (* Printf.printf "%s\n" (string_of_big_int !rez); *)
  
  match s_list with
  | op::value::[] ->
     
     let v = (big_int_of_string value) in
     f op v rez
       
  | _ -> failwith "Wrong format in solve" ;
              	        	 ;;
  
let () =
  
  let rez = ref (big_int_of_int 8) in

  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  
  List.iter (fun x -> solve x rez) sList;
  Printf.printf "%s\n" (string_of_big_int !rez);
   
