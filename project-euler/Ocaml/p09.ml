exception Found

let sum = 1000
let isPTriplet a b c = a * a + b * b = c * c

let solve () =
  let k = ref 0 in

  try
    
    for i = 2 to sum / 2 do
      for j = 3 to sum / 3 do

	k := sum - j - i;
	
	if(isPTriplet i j !k) then
	  ( Printf.printf "\n%d %d %d %d\n" i j !k (i * j * !k);
	    raise Found )
				     
      done;
    done;

  with Found -> ()

let () = solve ()
      
	

					     
