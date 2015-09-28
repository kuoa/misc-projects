let is_prime n =
  let rec not_divisor i =
    i * i > n || (n mod i <> 0 && not_divisor (i+1))
  in not_divisor 2



let prime_list_until limit =
  let rec add_to_list i list =
    if (i >= limit) then
      list
    else if (is_prime i) then
      add_to_list (i + 2) (i :: list)
    else
      add_to_list (i + 2) list

  in add_to_list 5 [2; 3]
		  
let solve () =
  let limit = 2000000 in
  prime_list_until limit
  |> List.fold_left (+) 0 

let main () = solve ()
