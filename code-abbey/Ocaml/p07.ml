#load "str.cma";;
  
open Printf
open Str       
let file = "p7.txt"

let round x =
  int_of_float ( ceil (x -. 0.5))
;;

let toCelsius x =
  (x -. 32.0) /. 1.8
;;
  
let () =
  let ic = open_in file in
  printf "\n";
  
  try
    let line = input_line ic in

    (* tokenizing the line *)
    
    let sList = Str.split (Str.regexp_string " ") line in
    
    (* List.iter (printf "%s~") sList *)
    
    let rec loop sList =
      match sList with
      | [] -> printf ""

      | n::t ->
	 let n = float_of_string n in
	 let rez = round (toCelsius n) in
	 printf "%d " rez;
	 loop t
	      
    in loop sList;
       
  with e ->
    close_in ic;
    raise e
		     
;;
    
