  
let file = "p28.txt" (* input file *)

let printCategory x =
  (* print the acording message *)
  
  if (x < 18.5) then
    Printf.printf "under "
  else if (x < 25.0) then
    Printf.printf "normal "
  else if (x < 30.0) then
    Printf.printf "over "
  else
    Printf.printf "obese "

  
(* the BMI function *)
let bmi weight height  = weight /. height ** 2.0
		       
let solve sList =

  (* string list -> float list *)
  let fList = List.map (float_of_string) sList in

  match fList with
  | weight::height::[] ->

     let x = bmi weight height in
     printCategory x

  | _ -> () (* we can raise an exception if needed *)
    
	  
    

  
let () =

  (* string list list *)
  let sList = Ca.read_input file in
  List.iter solve sList;
  Printf.printf "\n"

