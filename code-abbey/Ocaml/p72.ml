let file = "p72.txt" (* input file *)

let consonants = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'j'; 'k'; 'l'; 'm';
		  'n'; 'p'; 'r'; 's'; 't'; 'v'; 'w'; 'x'; 'z']

let vowels =  ['a'; 'e'; 'i'; 'o'; 'u']

let xnext a c m x0 n =
  let rec loop value i =
    if (i >= n) then
      value
    else loop ((a * value + c) mod m) (i + 1)
  in loop x0 0


let solve length last_i=
  let a = 445 in
  let c = 700001 in
  let m = 2097152 in
  let x0 = 1667002 in

  let rec loop i =
    if (i < length) then
      let rand = xnext a c m x0 (i + last_i) in
      if (i mod 2 = 0) then 
	print_char (List.nth consonants (rand mod 19))	 
      else 
	print_char (List.nth vowels (rand mod 5));
	loop (i + 1)
  in loop 0;
     print_char ' ' 
	
                  
let main () =
  
  (* reading the input, string list list *)
  let last_i = ref 1 in
  let s_list = Ca.read_line_l file in
  let i_list = List.map (int_of_string) s_list in

  List.iter (fun x -> solve  x !last_i;
		      last_i := !last_i + x) i_list
	    
  		
let () = main ()	         
