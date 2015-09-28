let file = "p30.txt" (* input file *)

let rev_string s =
  let s = Bytes.of_string s in
  (* reversing the string in place *)
  let size = Bytes.length s in
  
  for i = 0 to ((size-1) / 2)  do
    let temp = s.[i] in
    
    (* exchange symetrical chars *)
    Bytes.set s i (s.[size-i-1]);
    Bytes.set s (size-i-1) temp
	      
  done;

  Printf.printf "%s\n" s 
;;
    
  
	   
let solve s = rev_string s;;
        	        		   		     
let () =
  let s = "where supper job off stove yield why end interrogative set"
  in solve s;;


  (*
  (* reading the input, string list list *)
  let sList = Ca.read_input file in
  List.iter solve sList;
  Printf.printf "\n"
   *)
  
 
   
