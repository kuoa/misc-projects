
let rec int_to_list n =
  if n <= 0 then
    []
  else
    (n mod 10) :: int_to_list (n / 10)


let largest_palindrom a b =
  let max = ref 0 in
  for i = a downto 100 do
    for j = b downto 100 do

	let number = i * j in
	let list = int_to_list number in

	if (list = List.rev list && number > !max) then
	  max := number
		   
    done;    
  done;
 
  Printf.printf "\n%d\n" !max
      
let () = largest_palindrom 999 999 
			      
