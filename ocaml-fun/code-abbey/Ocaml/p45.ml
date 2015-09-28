let file = "p45.txt" (* input file *)

let ranks = [| "A"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "T"; "J"; "Q"; "K" |]
let suits = [| "C"; "D"; "H"; "S" |]


let swap array i j =
  let temp = array.(i) in
  array.(i) <- array.(j);
  array.(j) <- temp

	      
let create_deck suits ranks =
  
  let ls = Array.length suits in
  let lr =  Array.length ranks in
  let l =  ls * lr in (* combined size *)
  let deck = Array.make l "null" in
  let c = ref 0 in

  for i = 0 to ls - 1 do
    for j = 0 to lr - 1 do
      deck.(!c) <- suits.(i) ^ ranks.(j);
      incr c
    done;
  done;

  deck


    
let solve s_rand =
  let deck = create_deck suits ranks in
  let l = Array.length deck in
  
  for i = 0 to (l - 1) do
    let rand = (int_of_string s_rand.(i)) mod l in
    swap deck i rand
  done;

  Array.iter (Printf.printf "%s ") deck
				
    
                                        
let main () =

  (* reading the input, string list list *)
  Ca.read_line_a file
  |> solve;
  print_newline ()

		
let () = main ()	         
