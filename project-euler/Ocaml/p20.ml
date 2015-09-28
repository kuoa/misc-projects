open Batteries


let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let sum_digit string =
  let char_list = String.explode string in
  List.fold_left (fun acc ch -> acc + (int_of_char ch - int_of_char '0')) 0 char_list

let fact n =
  let rec loop acc n =
    if n = 1 then acc
    else let x = Big_int.big_int_of_int n in
	 loop (Big_int.mul x acc) (n - 1)
  in loop Big_int.one n

let solve () =
  let n = fact 100 in
  sum_digit (Big_int.to_string n)
	  
let main () = time solve ()
    


