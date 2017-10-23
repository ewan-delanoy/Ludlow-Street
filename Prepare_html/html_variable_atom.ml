(*

#use"Prepare_html/html_variable_atom.ml";;

*)

type t=
   Simple of Html_parameter_type.t*((string*string) list) 
   |Unregistered of ((Html_parameter_value.t list) -> string);;

