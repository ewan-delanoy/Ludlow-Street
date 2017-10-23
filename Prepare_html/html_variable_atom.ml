(*

#use"Prepare_html/html_variable_atom.ml";;

*)

type t=
   Simple of Html_parameter_type.t*((string*string) list) 
   |Unregistered of ((Html_parameter_value.t list) -> string);;

exception Missing_parameter of Html_parameter_type.t * (Html_parameter_value.t list);;  

let apply x l=
  match x with
   Simple(typ,l_assoc)->
         (
           match Option.seek (fun vaal->Html_parameter_value.get_type vaal=typ) l with
           None->raise(Missing_parameter(typ,l))
           |Some(vaal)->List.assoc (Html_parameter_value.get_content vaal) l_assoc
         )
  |Unregistered(f)->f l;;

