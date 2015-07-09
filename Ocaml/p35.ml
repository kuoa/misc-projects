let file = "p35.txt" (* input file *)

let get_years s r p =

  let rec loop new_sum years=
    if (new_sum >= r) then
      years
    else loop (new_sum +. new_sum *. p /. 100.0) (years+1)
  in loop s 0


let solve s_list =
  let f_list = List.map (float_of_string) s_list in

  match f_list with
  | s::r::p::[] ->
     Printf.printf "%d " (get_years s r p)

  | _ -> failwith "Wrong format in solve" 
                               

let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()  
