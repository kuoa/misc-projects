let limit = 20

let rec div_all_to x n =
  n <= 1 || x mod n = 0 && div_all_to x (n-1)


let rec get_number_to x =
  if (div_all_to x limit) then
    x
  else
    get_number_to (x+1)

let () = Printf.printf "\n%d\n" (get_number_to 9999999)
