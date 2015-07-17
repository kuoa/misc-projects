
let file = "p9.txt" (* input file *)

let sum a b c =
  (a + b) >= c
;;

let solve s_list =

  let i_list = List.map (int_of_string) s_list in
  match i_list with
  | a::b::c::[] ->
     if ( (sum a b c) && (sum a c b) && (sum b c a) ) then
       Printf.printf "%d " 1
     else
       Printf.printf "%d " 0

  | _ -> failwith "Wrong format"
;;
  
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  
  List.iter solve sList;
  Printf.printf "\n"
   
