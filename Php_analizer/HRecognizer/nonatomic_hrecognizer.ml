(*
#use"Php_analizer/HRecognizer/nonatomic_hrecognizer.ml";;
*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(t list)
 |Ordered_disjunction of string*(t list)
 |Star of string*t;;

let name=function
  Leaf(s,_)->s
  |Chain(s,_)->s
  |Ordered_disjunction(s,_)->s
  |Star(s,_)->s;;

let leaf s x=Leaf(s,x);;
let chain s l=Chain(s,l);;
let ordered_disjunction s l= Ordered_disjunction(s,l);;
let star s l=Star(s,l);; 