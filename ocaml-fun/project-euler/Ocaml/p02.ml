let make_fib n =
  
  let rec loop n1 n2 list =
    if (n2 >= n + 1) then
      List.rev list
    else
      let new_n = (n1 + n2) in
      loop n2 new_n (new_n :: list)
  in loop 0 1 []


	  
let solve () =
  let max = 4000000 in

  make_fib max
  |> List.filter (fun x ->  x mod 2 = 0)
  |> List.fold_left (+) 0
  |> print_int


let () = solve ()
		  
