let read_input_l file =
  (** string list list of input *)
  (*old versian was read_input*)

  let rez = ref [] in
  let ic = open_in file in
  
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

let read_line_l file =
  (** string list of input *)

  let rez = ref [] in
  let ic = open_in file in

  try
    let line = input_line ic in
    (* tokenizing the line *)
    
    let sList = Str.split (Str.regexp_string " ") line in
    rez := sList;
    
    !rez
   
  with End_of_file ->
    close_in ic;

    !rez
;;

let read_line_a file =
  (** string array of input *)
  let s_list = read_line_l file in
  let size = List.length s_list in
  let s_array = Array.make size "" in
  List.iteri (fun i x -> s_array.(i) <- x) s_list;

  s_array
;;
  
let read_input_brut file =
  (** string list of input *)
 
  let rez = ref [] in
  let ic = open_in file in
  
  try
    while true do
      let line = input_line ic in
      rez := line :: !rez
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

