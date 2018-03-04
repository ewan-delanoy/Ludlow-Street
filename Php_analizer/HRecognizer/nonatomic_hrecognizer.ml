(*
#use"Php_analizer/HRecognizer/nonatomic_hrecognizer.ml";;
*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(t list)
 |Ordered_disjunction of string*(t list)
 |Star of string*t
 |Maybe of string*t
 |Keyword_avoider of string*(t*(string list));;

let name=function
  Leaf(s,_)->s
  |Chain(s,_)->s
  |Ordered_disjunction(s,_)->s
  |Star(s,_)->s
  |Maybe(s,_)->s
  |Keyword_avoider(s,_)->s;;

let leaf s x=Leaf(s,x);;
let chain s l=Chain(s,l);;
let ordered_disjunction s l= Ordered_disjunction(s,l);;
let star s l=Star(s,l);; 
let maybe s l=Maybe(s,l);; 
let keyword_avoider s (atm,l)=Keyword_avoider(s,(atm,l));; 

let unveil =function
  Leaf(s,atm)->("leaf",[],Some atm,None)
 |Chain(_,l)->("chain",l,None,None)
 |Ordered_disjunction(_,l)->("ordered_disjunction",l,None,None)
 |Star(_,x)->("star",[x],None,None)
 |Maybe(_,x)->("maybe",[x],None,None)
 |Keyword_avoider(_,(x,l))->("avoid_keywords",[x],None,Some(l));;


let print (x:t)="rn \""^(name x)^"\"";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;