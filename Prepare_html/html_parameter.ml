(*

#use"Prepare_html/html_parameter_value.ml";;

*)

type t=V of Html_parameter_type.t*string;;

let get_type (V(typ,s))=typ;;
let get_content (V(typ,s))=s;;