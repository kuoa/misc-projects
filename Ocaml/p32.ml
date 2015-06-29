(* i took this not optimal approach because i wanted to see all the steps *)

let make_list x =
  (* create a list with the first x numbers *)
  let rec loop i acc =
    if (i = 0) then
      acc
    else
      loop (i-1) (i::acc)
  in loop x []
;;

let print_list l =
  List.iter (Printf.printf "%d ") l;
  Printf.printf "\n"
;;
 
let iteri_filter list f i =
  (* f works on the index of element *)
  (* i is a pointer since we never stop counting *)
  
  let rec loop o_list n_list i =
  match o_list with
  | [] -> List.rev n_list
  | h::t ->
     if f !i then(
       incr i;
       loop t (h:: n_list) i
     )
     else (
       incr i;
       loop t n_list i
     )
    in loop list [] i
;;

let solve_v1 size step =
  let old_list = make_list size in
  let i = ref 1 in
  let f x = (x mod step <> 0) in
  let new_list = ref [] in 

  (* first cycle *)
  new_list := iteri_filter old_list f i;
  
  while (List.length !new_list > 1) do
    print_list !new_list;
    new_list := (iteri_filter !new_list f i)
  done;

  print_list !new_list
      				     
;;


  
  
      
    
   
