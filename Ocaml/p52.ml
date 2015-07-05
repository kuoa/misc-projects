let file = "p52.txt" (* input file *)

let print_type a b c =
    let open Printf in
    let s1 = a * a + b * b in
    let s2 = c * c in

    if (s1 = s2) then
      printf "R "
    else if (s1 < s2) then
      printf "O "
    else printf "A "
;;
    
  	      
let solve s_list =
  let open Printf in  
  let i_list =
    s_list
    |> List.map int_of_string  
    |> List.sort compare in (* sort to make it general *)
  
  match i_list with
  | a::b::c::[] ->
     print_type a b c     
     
  | _ -> failwith "wrong format"
                         
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

