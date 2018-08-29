

type filename = string

val cat_array: filename -> string array

val chop : string -> string

(* convert something printed using Format to print into a string *)
val format_to_string : (unit -> unit) (* printer *) -> string

val filesize : filename -> int

val hupdate_default : 
  'a -> update:('b -> 'b) -> default:(unit -> 'b) -> ('a, 'b) Hashtbl.t -> unit

val join_gen : 'a -> 'a list -> 'a list

val optionise : (unit -> 'a) -> 'a option

val pr2_xxxxxxxxxxxxxxxxx : unit -> unit

val some : 'a option -> 'a 












