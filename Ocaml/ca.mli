(** read each line and return a float list list *)
val read_input : string -> string list list

(** print 'a list using f as a printing function *)
val print_list : ('a -> unit) -> 'a list -> unit

(** print 'a list list using f as a printing function *)
val print_2xlist : ('a -> unit) -> 'a list list -> unit
