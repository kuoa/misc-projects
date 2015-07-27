let file = "p8.txt"

let string_to_array () =
  let ic = open_in file in
  let line = input_line ic in
  let size = String.length line in
  let array = Array.make size (-1) in

  for i = 0 to size - 1 do
    let digit = int_of_char line.[i] - int_of_char '0' in
    array.(i) <- digit
  done;
  array

let solve () =
  let max = ref 0 in
  let produs = ref 1 in
  let array = string_to_array() in

  for i = 0 to Array.length array - 14 do
    for j = 0 to 12 do
      produs := !produs * array.(i + j)
    done;
    if (!produs > !max) then
      max := !produs;
    produs := 1;
  done;

  !max

let () = Printf.printf  "\n%d\n"  (solve())
    
  

    

