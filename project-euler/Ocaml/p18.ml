open Batteries

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx
       
let file = "p18.txt"

(* split string list on space *)
let rec tokenize = function
  | [] -> []
  | h :: t -> String.nsplit h " " @ tokenize t

(* fill a triangle array *)
let fill_array list_enum =
  let size = List.length list_enum in
  let t_list =  tokenize list_enum in
  let tokened_list = List.map (String.to_int) t_list in
  let triangle_array = Array.make_matrix size size 0 in

  let rec loop i j = function
    | [] -> triangle_array
    | h :: t ->
       triangle_array.(i).(j) <- h;
       if i = j then loop (i + 1) 0 t else loop i (j + 1) t
  in loop 0 0 tokened_list
	                     
let read_triangle file =
  let list = List.of_enum (File.lines_of file) in
  fill_array list

let solve () =
  let t_array = read_triangle file in
  let size = Array.length t_array in

  let rec loop i j sum =
    if i < size - 1 then
      if t_array.(i + 1).(j) > t_array.(i + 1).(j + 1) then
	  loop (i + 1) j (sum + t_array.(i + 1).(j))
      else
	  loop (i + 1) (j + 1) (sum + t_array.(i + 1).(j + 1))
    else sum
  in loop (-1) 0 0

let main () = time solve ()
