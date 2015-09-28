let sum_to n = n * (n + 1) / 2

let sum_squares_to n = n * (n + 1) * (2 * n + 1) / 6

let solve n =
  let s1 = sum_to n in
  let s2 = sum_squares_to n in
  s1 * s1 - s2

let () = Printf.printf "\n%d\n" (solve 100)
