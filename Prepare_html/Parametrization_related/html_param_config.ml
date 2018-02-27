(*

#use"Prepare_html/Parametrization_related/html_param_config.ml";;

*)

type t= (string*string) list;;

let test_for_inclusion (l1:t) (l2:t)=
  List.for_all (fun x->List.mem x l2) l1;;