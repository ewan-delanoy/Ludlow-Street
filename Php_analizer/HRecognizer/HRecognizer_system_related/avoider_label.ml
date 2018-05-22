(*

#use"Php_analizer/HRecognizer/avoider_label.ml";;

*)

type t=AL of string;;

let of_string s=AL s;;
let to_string (AL s)=s;;

let prepare_ocaml_name (AL s)=
    Prepare_ocaml_name.for_labelled_elt
    Prepare_ocaml_name.for_string
    ("A"^"voider_label.AL")  s;;
    