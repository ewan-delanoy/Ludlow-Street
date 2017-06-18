type t 
val to_string_list : t -> string list 
val of_string_list : string list -> t 
val concat_two : t -> t -> t 
val concat : t list -> t 
val print : t -> string 
val itemize : ('a -> string) -> 'a list -> t 

val max_line_length_ref : int ref 
val make_aggregates_if_possible : Separator.t -> string list -> t

