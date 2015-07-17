let file = "p47.txt" (* input file *)

let is_letter x = ('A' <= x ) && ( x <= 'Z')

let decipher string k =
  let open Printf in

  let new_letter c =
    let c = int_of_char c in
    let nc = c - k in
    let diff = (int_of_char 'Z)' - (int_of_char 'A') + 1 in

    if (nc < int_of_char 'A') then
      char_of_int (nc + diff)
    else
      char_of_int nc in
  
  let print_new_letter c =
    if (is_letter c) then
      printf "%c" (new_letter c)
    else
      printf "%c" c in

  String.iter (fun c -> print_new_letter c) string;
  print_char ' '
    	     

let solve string =
  let k = 24 in decipher string k 
                             

let main () =

  (* reading the input, string list list *)
  Ca.read_input_brut file
  |> List.iter solve;
  print_newline ()

let () = main ()  
