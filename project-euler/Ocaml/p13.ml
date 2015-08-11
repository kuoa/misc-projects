open Batteries
open Big_int

let file = "p13.txt"

let solve =
  let result = ref zero in 
  let filelines = File.lines_of file in (* reading lines *)
  Enum.iter (fun line -> result := add !result (of_string line)) filelines;
  Printf.printf "%s\n" (to_string !result)	
