let file = "p25.txt" (* input file *)

let xnext a c m x0 n =

  let rec loop value i =
    if (i >= n) then
      value
    else loop ((a * value + c) mod m) (i + 1)
  in loop x0 0

let solve s_list =
  let i_list = List.map (int_of_string) s_list in

  match i_list with
  | a::c::m::x0::n::[] ->
     Printf.printf "%d " (xnext a c m x0 n)

  | _ -> failwith "Wrong format in solve" 
                               

let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()  
