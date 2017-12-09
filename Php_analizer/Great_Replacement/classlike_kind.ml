(*

#use"Php_analizer/Great_Replacement/classlike_kind.ml";;


*)

type t=
      Abstract_class
     |Final_class
     |Usual_class 
     |Interface
     |Plain_text
     |Namespace_line
     |After_namespace_comments;;

let abstract_class=Abstract_class;;
let final_class=Final_class;;
let usual_class=Usual_class;;
let interface=Interface;;
let plain_text=Plain_text;;
let namespace_line=Namespace_line;;
let after_namespace_comments=After_namespace_comments;;

exception Unknown_class_qualification of string;;

let from_class_qualification qualification=
  if qualification="" then Usual_class else
  if qualification="abstract" then Abstract_class else
  if qualification="final" then Final_class else
  raise(Unknown_class_qualification(qualification));;
       