open Batteries

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx
       
let file = "p18.txt"

let read_triangle file =
  let list = List.of_enum (File.lines_of file) in
  let size = List.length list in
  let rec tokenize = function
  | [] -> []
  | h :: t -> String.nsplit h " " @ tokenize t in

  let tokened_list =  tokenize list in
  			             			  
