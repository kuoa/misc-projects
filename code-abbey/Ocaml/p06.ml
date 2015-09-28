#load "str.cma";;
  
open Printf
open Str       
let file = "p6.txt"

let () =
  let ic = open_in file in
  printf "\n";
  try
    while true do
      let line = input_line ic in
      (* tokenizing the line *)
         
      let sList = Str.split (Str.regexp_string " ") line in
      (* List.iter (printf "%s~") sList *)
 
      match sList with
      | n1::n2::t ->
	 let n1 = float_of_string n1 in
	 let n2 = float_of_string n2 in
	 let q = n1 /. n2 in
	 let natQ = int_of_float ( ceil (q -. 0.5)) in
	 printf "%d " natQ
	   
			
      | _ -> failwith "Empty or too much arguments"
  
       
    done
  with End_of_file ->
    close_in ic
;;
    
