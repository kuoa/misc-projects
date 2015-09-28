type blend = string
type tea ={ name: blend; min: int; sec: int};;

let tea_list = [{name = "black"; min = 0; sec = 5}; {name = "fruit"; min = 7; sec = 0};
                {name = "green"; min = 3; sec = 0}]

(* throws exception Not_found *)
let find_tea name = List.find (fun tea -> tea.name = name) tea_list
    
let print_tea_list () = List.iter (fun tea -> Printf.printf " * %s\n" tea.name) tea_list    
		
let get_time tea = tea.min * 60 + tea.sec                                  

(* outputs a progress bar coresponding to the stewing time of #tea *)
let start_kettle tea = 
  let tea_time = float_of_int (get_time tea) in
  let start_time = Unix.gettimeofday () in

  let rec loop percent =
    if percent < 100 then

      let time_diff = Unix.gettimeofday () -. start_time in
      let new_percent = int_of_float (time_diff /. tea_time *. 100.) in
      
      Bar.print_bar new_percent;           
      Unix.sleep 1;

      loop new_percent

  in loop 0;
  Printf.printf "\nYour %s tea is ready! Enjoy!\n" tea.name
    

