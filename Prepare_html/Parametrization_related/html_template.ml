(*

#use"Prepare_html/Parametrization_related/html_template.ml";;

*)

type t=
    Constant of string
   |Nonconstant_atom of ( ((string*string) list )*string ) list
   |Concat of t list;;

let config_covered_by_another l1 l2=
   List.for_all (fun x->List.mem x l2) l1;;   

