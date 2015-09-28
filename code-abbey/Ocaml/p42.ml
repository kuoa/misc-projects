let file = "p42.txt" (* input file *)
	     
let value c ace =
  match c with
  | "2"| "3"| "4"| "5"| "6"| "7"| "8"| "9" -> int_of_string c
  | "T"| "J"| "Q"| "K" -> 10
  | "A" -> (ace := true; 1)      (* default 1, may be + 10 if ace*)
  | _ -> failwith "Non-valid input"


    		  
let solve s_list =
  let open Printf in
  let ace = ref false in
  let rez =
    s_list
    |> List.map (fun x -> value x ace)
    |> List.fold_left (+) 0 in

  let rez_p_10  = rez + 10 in
  if (rez > 21) then
    printf "Bust "
  else if ((!ace) && (rez_p_10 <= 21)) then
    printf "%d " rez_p_10
  else
    printf "%d " rez


           
let main () =
  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

