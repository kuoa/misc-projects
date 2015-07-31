let rec make_list = function
  | 0 -> []
  | n -> n :: make_list (n-1)

let solve () =
  let max = 999 in

  make_list max
  |> List.filter (fun x -> x mod 3 = 0 || x mod 5 = 0)
  |> List.fold_left (+) 0
  |> print_int


let () = solve ()

  
