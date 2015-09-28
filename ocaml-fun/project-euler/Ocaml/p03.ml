let n = 600851475143

let prime_factors n =
  let rec loop n i res = 
    if n <= 1 then
      res
    else
      if n mod i = 0 then
	loop (n / i) i (i :: res)
      else
	loop n (i + 1) res	     
  in loop n 2 []


let solve () =
   prime_factors n
  |> List.hd
  |> print_int		        

	  
let () = solve ()
 
