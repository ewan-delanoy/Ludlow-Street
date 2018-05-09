(*

#use"Php_analizer/HRecognizer/avoider_label.ml";;

*)

type t=AL of string;;

let of_string s=AL s;;
let to_string (AL s)=s;;

