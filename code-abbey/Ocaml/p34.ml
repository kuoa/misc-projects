let file = "p34.txt" (* input file *)

let e = 2.718281828459045  (* euler's number *)
	  
let f1 a b c d x =
  (* will be used in partial aplication *)
  a *. x +.  b *. (sqrt (x *. x *. x)) -. c *. (e ** (  (-.x) /. 50.)) -. d
  
let find_solution a b c d =
  let f = f1 a b c d in (* partial application *)
  let eps = 0.0000001 in (* precisition *)

  let rec loop low high =
    let mid = ( (low +. high) /. 2.) in
    let fmid = f mid in
    let flow = abs_float (f low) in (* take abs! *)
    let fhigh = abs_float (f high) in (* take abs! *)

    if (abs_float fmid <= eps) then
      Printf.printf "%.8f " mid
    else if (flow < fhigh) then
      loop low mid
    else
      loop mid high

  in loop 0. 100.
          	      

let solve s_list =
  let open Printf in  

  let i_list = List.map float_of_string  s_list in
   
  match i_list with
  | a::b::c::d::[] ->
     find_solution a b c d
		   
  | _ -> failwith "wrong format"
                         
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;
  print_newline ()

let () = main ()
  

