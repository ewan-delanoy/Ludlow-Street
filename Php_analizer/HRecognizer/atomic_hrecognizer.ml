(*

#use"Php_analizer/HRecognizer/atomic_hrecognizer.ml";;

*)


(*
#use"Php_analizer/HRecognizer/atomic_hrecognizer.ml";;
*)

type t=
  Constant of string
 |Later_constant of string
 |Constant_list of string list
 |Later_constant_list of string list
 |Exactly_one of char list
 |Star of char list
 |Star_outside of char list
 |Enclosed of char*char;;

let constant s=Constant(s);;
let later_constant s=Later_constant(s);;
let constant_list l= Constant_list(l);;
let later_constant_list l=Later_constant_list(l);;
let exactly_one l=Exactly_one(l);;
let star l=Star(l);;
let star_outside l=Star_outside(l);;
let enclosed (opener,closer)=Enclosed (opener,closer);;
