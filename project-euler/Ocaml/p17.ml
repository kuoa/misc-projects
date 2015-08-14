open Batteries

let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
  fx

let unitati = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
let zecimi_mici = [| "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen"|]
let zecimi_mari = [| ""; "ten"; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]

let rec n_to_s n =
  (* raises Not_found *)
  if n >= 0 && n < 10 then
    unitati.(n)
  else if n < 20 then
    zecimi_mici.(n - 10)
  else if n < 100  then
    zecimi_mari.(n / 10) ^ " " ^ (n_to_s (n mod 10))
  else if n < 1000 && n mod 100 = 0 then    
    unitati.(n / 100) ^ " hundred"
  else if n < 1000 && n mod 100 <> 0 then
      unitati.(n / 100) ^ " hundred and " ^ n_to_s (n mod 100)
  else if n = 1000 then "one thousand" 
  else raise Not_found

let no_whitespace s =
  String.replace_chars (fun c -> if c = ' ' then "" else String.of_char c) s

let solve () =
  (1--1000)
  |> Enum.map (fun n -> no_whitespace (n_to_s n))
  |> Enum.fold (fun acc s -> acc + String.length s) 0

let main () = time solve ()
