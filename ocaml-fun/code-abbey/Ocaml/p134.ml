let file = "p62.txt" (* input file *)

let draw w h txt_size =
  let dx = ref 1 in
  let dy = ref 1 in

  let rec loop i x y =
    if i < 100 then (
      if (x <= 0 || x + txt_size >= w) then
	dx := -(!dx);
      if (y <= 0 || y >= h - 1) then
	dy := -(!dy);

     
      let new_x = x + !dx in    
      let new_y = y + !dy in
       Printf.printf "%d %d " x  y;
     
      loop (i + 1) new_x new_y
    )
  in loop 0 1 1
