  
let file = "p11.txt" (* input file *)

let sum_digit x =

  let rec loop x acc =
    if (x = 0) then
      acc
    else loop (x / 10) (acc + (x mod 10) )
  in loop x 0

 
let f a b c =
  let rez = a * b + c in
  sum_digit rez

  
let solve sList =

  (* string List -> int List *)
  let iList = List.map (int_of_string) sList in

  match iList with
  |a::b::c::[] ->
    
    let rez = f a b c in
    Printf.printf "%d " rez

  |_ -> ()

     
let () =
  
  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  
  List.iter solve sList;
  Printf.printf "\n"
		
