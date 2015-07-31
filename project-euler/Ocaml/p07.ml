let is_prime n =
  let rec not_divisor i =
    i * i > n || (n mod i <> 0 && not_divisor (i+1))
  in not_divisor 2

let first_x_primes x =
  let rec loop i list nb =
    if nb >= x then
      list
    else
      if (is_prime i) then
	loop (i + 1) (i :: list) (nb + 1)
      else
	loop (i + 1) list nb
  in loop 2 [] 0

let () = Printf.printf "\n%d\n" (List.hd (first_x_primes 10001))
		  
		 
  
