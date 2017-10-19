(*

#use"Prepare_html/html_parameter_type.ml";;

*)

type t=T of string;;

let of_string s=T s;;
let to_string (T s)=s;;