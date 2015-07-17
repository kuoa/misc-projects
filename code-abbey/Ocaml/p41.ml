  
let file = "p41.txt" (* input file *)
		       
let solve sList =

  (* string list -> float list *)
  let fList = List.map float_of_string sList in

  (* sorting the float list *)
  let sortList = List.sort compare fList in

  (* list size *)
  let size = List.length sortList in

  (* empty list *)
  if (size = 0) then
    ()

  (* size is even <=> the median is the middle element *)
  else if (size mod 2 <> 0) then
    Printf.printf "%d " (int_of_float (List.nth sortList (size/2) ) )
  
  (* if size is odd then take the average \ *)
  (* between the middle 2 elements *)
  else (
    let n1 =  (List.nth sortList (size/2 - 1) ) in
    let n2 =  (List.nth sortList (size/2) ) in
    let rez = ( ( n1 +. n2) /. 2.0 ) in
    Printf.printf "%d " (int_of_float rez )
  )
	 
let () =

  (* string list list *)
  let sList = Ca.read_input file in
  List.iter solve sList;
  Printf.printf "\n"

