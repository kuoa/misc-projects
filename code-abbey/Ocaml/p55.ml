let file = "p55.txt" (* input file *)

(* it's really late, this program might be stupid, but it works *)
	     
let rec add_dublicates list htbl =
  (* using a Hastable to add the dublicates *)
  
  match list with
  | x::y::t -> 
    if (x = y) then (
      if (not (Hashtbl.mem htbl x)) then (
	Hashtbl.add htbl x x;
	add_dublicates t htbl
      )
      else
	add_dublicates t htbl
    )
    else
      add_dublicates (y::t) htbl

  | _ -> ()
				
	          		                             
let solve s_list =
  let open Printf in

  (* sort the list *)
  let sort_list = List.sort compare s_list in
  let htbl = Hashtbl.create (List.length sort_list) in
  add_dublicates sort_list htbl;

  (* transfer dublicates from hstbl to list | sort list | print list *)  
  let rez_list = ref [] in
  Hashtbl.iter (fun key x -> rez_list := x :: !rez_list ) htbl;

  List.sort compare !rez_list
  |> List.iter (printf "%s ")
	    		  
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  
(* 
fun fact time measurement for the current program and the following bash script

 cat "$1" | xargs -n1 | sort | uniq -d | xargs 
 ->> real	0m0.181s
     user	0m0.000s
     sys        0m0.032s

 ocaml version
--> real	0m0.006s
    user	0m0.000s
    sys	        0m0.004s
 *)




