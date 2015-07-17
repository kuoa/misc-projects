exception Stop
exception Empty

let file = "p19.txt" (* input file *)
	    
let check stack s=
  (* There must be a cleaner way to do this, redo when you have time *)  
  (* raises Stop *)
  
  match s with
  | '(' | '[' | '{' | '<' ->  Stack.push s stack

  | ']' | '}' | '>' -> (      (* ascii difference of 2 *)

    let s_stack = Stack.pop stack in
    if ( (int_of_char s) - (int_of_char s_stack) <> 2) then
      raise Stop
  )
  
  | ')' -> (                  (* ascii difference of 1 *)

    let s_stack = Stack.pop stack in
    if ( (int_of_char s) - (int_of_char s_stack) <> 1) then
      raise Stop
  )

  | _ -> ()                   (* any other char*)

    
let solve s_list =
  
  let stack = Stack.create () in
  
  try
    String.iter (fun c -> check stack c) s_list;

    if (Stack.is_empty stack) then      
      Printf.printf "1 "
    else
      Printf.printf "0 "

  with 
  | Stop | Empty -> Printf.printf "0 "
    			
let main () =
  
  Ca.read_input_brut file
  |> List.iter solve;
  print_newline ()

let () = main ()  
