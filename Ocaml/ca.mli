(** read each line and return a string list list *)
val read_input_l : string -> string list list

(** read first line and return a string list *)
val read_line_l : string -> string list

(** 'a array of input *)
val read_line_a : string -> string array

(** print 'a list using f as a printing function *)
val print_list : ('a -> unit) -> 'a list -> unit

(** print 'a list list using f as a printing function *)
val print_2xlist : ('a -> unit) -> 'a list list -> unit


