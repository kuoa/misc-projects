open Batteries

let triangle x =
  x * (x + 1) / 2


let count_factors n =
  let rec loop i rez =
    if (i * i > n) then
      rez
    else if (n mod i = 0) then
      loop (i + 1) (rez + 2)
    else
      loop (i + 1) rez
  in loop 1 0
    

let rec solve n =
  if (count_factors (triangle n) < 500) then
    solve (n + 1)
  else
    Int.print stdout (triangle n)

	      
let () = solve 7  								  
