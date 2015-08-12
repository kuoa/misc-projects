open Batteries

let collatz x =
  
  let rec loop i n =
    if n = 1 then i
    else if n mod 2 = 0 then loop (i + 1) (n / 2)
    else loop (i + 1) (3 * n + 1)	      
  in loop 1 x	  



let solve () =
  
  let rec loop n n_max size_max = 
    if n > 500000 then

      let chain_size = collatz n in
      if chain_size > size_max then
	loop (n - 2) n chain_size
      else loop (n - 2) n_max size_max

    else Printf.printf "%d %d\n" n_max size_max      
  in loop 999999 0 0
