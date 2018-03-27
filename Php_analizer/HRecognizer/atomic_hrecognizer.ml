(*

#use"Php_analizer/HRecognizer/atomic_hrecognizer.ml";;

*)


(*
#use"Php_analizer/HRecognizer/atomic_hrecognizer.ml";;
*)

type t=
  Constant of string
 |Later_constant of string
 |Star of char list
 |Star_outside of char list
 |Enclosed of char*char
 |Simple_quoted
 |Double_quoted;;

let constant s=Constant(s);;
let later_constant s=Later_constant(s);;
let star l=Star(l);;
let star_outside l=Star_outside(l);;
let enclosed (opener,closer)=Enclosed (opener,closer);;
let simple_quoted=Simple_quoted;;
let double_quoted=Double_quoted;;