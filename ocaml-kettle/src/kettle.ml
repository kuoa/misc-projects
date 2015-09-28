let start_kettle tea_name =
  let open Printf in
  
  try
    let found_tea = Tea.find_tea tea_name in
    printf "Starting the kettle with some %s tea\n" tea_name;
    Tea.start_kettle found_tea
  with
  | Not_found -> printf "Option: '%s' not found.\n-help to see available options.\n" tea_name

let print_tea_list () = Tea.print_tea_list ()

let () =
  let open Printf in
  
  let speclist = [("-list", Arg.Unit (print_tea_list), " Display the list of availabe types of tea");
                  ("-tea", Arg.String (start_kettle), " Start the steeping for a specific type of tea")]
  in let usage_msg = "Kettle is a tea steeping timer. Options available:"
  in Arg.parse speclist print_endline usage_msg
