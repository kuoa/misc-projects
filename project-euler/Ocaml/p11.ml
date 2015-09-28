open Batteries

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx
       
let file = "p11.txt"
let read file = List.of_enum (File.lines_of file)
let rec tokenize = function
  | [] -> []
  | h :: t -> String.nsplit h " " @ tokenize t

let solve () = 
  let a = Array.of_list (tokenize (read file)) in
  let t = Array.map Int.of_string a in
  let size = Int.of_float (Float.sqrt (Int.to_float (Array.length t))) in
  let get x y = t.(x * size + y) in  
  let max = ref 0 in

  for i = 0 to size - 1 do
    for j = 0 to size - 5 do     
      let prod1 = get i j * get i (j + 1) * get i (j + 2) * get i (j + 3) in
      let prod2 = get j i * get (j + 1) i * get (j + 2) i * get (j + 3) i in

      if prod1 > !max then max := prod1;
      if prod2 > !max then max := prod2;
    done;
  done;
  
  for i = 0 to size - 5 do
    for j = 0 to size - 5 do
      let prod1  = get i j * get (i + 1) (j + 1) * get (i + 2) (j + 2) * get (i + 3) (j + 3) in

      if prod1 > !max then max := prod1;
    done;
  done;
  
  for i = 3 to size - 1 do
    for j = 0 to size - 5 do
      let prod2 = get i j * get (i - 1) (j + 1) * get (i - 2) (j + 2) * get (i - 3) (j + 3) in

      if prod2 > !max then max := prod2;
    done;
  done;
  
  !max
      			  
let main () = time solve ()
