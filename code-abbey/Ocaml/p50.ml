let file = "p50.txt" (* input file *) 

let is_letter c =
  (c >= 'a') && (c <= 'z')


let string_filter f s =
  (*Using buffers that automatically 
    expand as necessary.*)
  
  let len = String.length s in
  let sc = Buffer.create len in

  for i = 0 to len - 1 do
    let c = s.[i] in

    if (f c) then
      Buffer.add_char sc c
  done;

  Buffer.contents sc


let string_rev s =
  let s = Bytes.of_string s in
  (* reversing the string in place *)
  let size = Bytes.length s in
  
  for i = 0 to ((size-1) / 2)  do
    let temp = s.[i] in
    
    (* exchange symetrical chars *)
    Bytes.set s i (s.[size-i-1]);
    Bytes.set s (size-i-1) temp
	      
  done;

  Bytes.to_string s


let () =
  let printf = Printf.printf in
  let ic = open_in file in
  printf "\n";

  try    
    while true do
      let line = input_line ic in
      let s = string_filter is_letter (String.lowercase line) in
      let s_rev = string_rev s in
      
      if (s = s_rev) then
	printf "Y "
      else
	printf "N "           
    done;
         
  with End_of_file ->
    close_in ic;
    

