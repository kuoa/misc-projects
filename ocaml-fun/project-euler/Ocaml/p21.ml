open Batteries


let time f x =
  let t = Sys.time() in
  let fx = f x in
  Printf.printf "Execution time: %.fs\n" (Sys.time() -. t);
  fx

	  
let sum_divisors n =
  let rec is_divisor i acc =
    if i * i <= n then
      if n mod i = 0 then
	if i * i = n || i = 1 then is_divisor (i + 1) (i + acc)	  
	else is_divisor (i + 1) (i + n / i + acc)
      else is_divisor (i + 1) acc
    else acc
  in is_divisor 1 0


let solve () =
  let sum = ref 0 in

  for n1 = 0 to 10000 do
    let n2 = sum_divisors n1 in
    let s = sum_divisors n2 in

    if n2 > n1 && n1 = s then
      sum := !sum + n1 + n2
  done;

  Printf.printf "%d " !sum
