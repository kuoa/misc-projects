let file = "p58.txt" (* input file *)

let suits = ["Clubs"; "Spades"; "Diamonds"; "Hearts"]
let ranks = ["2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "Jack"; "Queen"; "King"; "Ace"]

let print_card n =
  let k = 13 in
  let suit = n / k in
  let rank = n mod k in
  Printf.printf "%s " ( (List.nth ranks rank) ^ "-of-" ^ (List.nth suits suit) )
  
let solve s_list =

  match s_list with
  | [n] -> print_card (int_of_string n)

  | _ -> failwith "Wrong format in solve" 
		  
			
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()  
