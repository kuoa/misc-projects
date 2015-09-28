open Batteries

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let sum_digit string =
  let char_list = String.explode string in
  List.fold_left (fun acc ch -> acc + (int_of_char ch - int_of_char '0')) 0 char_list

let solve () =
  let pwr_string = Big_int.power_int_positive_int 2 1000 in
  let rez = sum_digit (Big_int.to_string pwr_string) in
  Printf.printf "%d " rez

let main () = time solve ()		 
