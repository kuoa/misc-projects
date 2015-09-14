(* prints $diez_number of '#' starting from $i *)
let rec print_diez i diez_number =
  if i < diez_number then
    (print_char '#'; print_diez (i + 1) diez_number)

(* prints $space_number of '#' starting from $i *)
let rec print_space i space_number =
  if i < space_number then
    (print_char ' '; print_space (i + 1) space_number)
  else
    print_char ']'
    
(* prints the current progres bar based on $percent *)
let print_bar percent =  
  let full_bar = 40 in (* 40 chars size bar *)
  let diez_number = int_of_float (float_of_int percent *. float_of_int full_bar /. 100.) in
  let space_number = full_bar - diez_number in

  print_diez 0 diez_number;
  print_space 0 space_number
