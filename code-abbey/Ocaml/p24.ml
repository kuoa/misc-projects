let file = "p24.txt" (* input file *) 

let neum_f x =
  (* bad idea to use a list, but we don't have many values *)
  (* should compare with a Hashtbl *)

  let list_val = ref [] in

  let rec loop i x =
    if (List.mem x !list_val) then
      Printf.printf "%d " i
    else (
      list_val := x::!list_val;
      let xs = x * x in
      let rez = (xs / 100) mod 10000 in
      loop (i+1) rez
    )
  in loop 0 x

;;
  
let solve s_list =
  let i_list = List.map (int_of_string) s_list in

  match i_list with
  | h::[] -> (neum_f h)

  | _ -> failwith "wrong format"
    	     	        
let () =

  (* reading the input, string list list *)
  let sList = Ca.read_input file in 
  List.iter solve sList;
  Printf.printf "\n"
   
