let read_input file =
  (** 'a list list of input *)

  let rez = ref [] in
  let ic = open_in file in
  Printf.printf "\n";
  
  try
    while true do
      let line = input_line ic in
      (* tokenizing the line *)
      
      let sList = Str.split (Str.regexp_string " ") line in
      rez := sList :: !rez
    done;

    List.rev !rez (* necessary *)
     
  with End_of_file ->
    close_in ic;

    List.rev !rez
;;


let print_list f list =
  List.iter f list;
  Printf.printf "\n";
;;
  
let print_2xlist f list =
   List.iter (print_list f) list
;;

