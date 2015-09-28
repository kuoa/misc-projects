let file = "p22.txt" (* input file *)

let get_time x y n  =

  let t = ( (x *.y *. n) /. (x +. y)) in (* exact time, but nb pages might <> int *)
  let p1 = t /. x in (* nb of pages for the first printer *)
  let p2 = t /. y in (* nb of pages for the second printer *)

  let try1 = max ((ceil p1) *. x) ((floor p2) *. y) in (* total time when c p1 / f p2 *)
  let try2 = max ((floor p1) *. x) ((ceil p2) *. y) in (* total time when f p1 / c p2 *)

  int_of_float (min try1 try2) (* minimum time between try1 try2 *)

  

let solve slist =

  let ilist = List.map float_of_string slist in
  match ilist with
  | x::y::n::[] ->
     Printf.printf "%d " (get_time x y n)


  | _ -> failwith "Wrong reading format in solve"
  

    
                                  
let main () =

  (* reading the input, string list list *)
  Ca.read_input_l file
  |> List.iter solve;  
  print_newline ()

		
let () = main ()	         
