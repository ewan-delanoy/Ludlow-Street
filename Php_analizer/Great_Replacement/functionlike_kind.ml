(*

#use"Php_analizer/Great_Replacement/functionlike_kind.ml";;


*)

type t=
  Non_function
 |Usual_function 
 |Private_method
 |Protected_method
 |Public_method 
 |Namespace_line
 |After_namespace_comments
 |Abstract_class
 |Interface;;

let non_function = Non_function;;
let usual_function = Usual_function;;
let private_method = Private_method;;
let protected_method = Protected_method;;
let public_method = Public_method;;
let namespace_line =  Namespace_line;;
let after_namespace_comments = After_namespace_comments;;
let abstract_class=Abstract_class;;
let interface=Interface;;