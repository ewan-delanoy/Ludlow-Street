(*

#use"Php_analizer/HRecognizer/nonatomic_jrecognizer.ml";;

We deliberately omit all operations that involve a possibly empty
result, such as "star".

*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(t list)
 |Ordered_disjunction of string*(t list)
 |Keyword_avoider of string*(t*(string list));;

let name=function
  Leaf(s,_)->s
  |Chain(s,_)->s
  |Ordered_disjunction(s,_)->s
  |Keyword_avoider(s,_)->s;;

let leaf s x=Leaf(s,x);;
let chain s l=Chain(s,l);;
let ordered_disjunction s l= Ordered_disjunction(s,l);;
let keyword_avoider s (atm,l)=Keyword_avoider(s,(atm,l));; 

let leaf_sconstructor="leaf";;
let chain_sconstructor="chain";;
let ordered_disjunction_sconstructor="ordered_disjunction";;
let keyword_avoider_sconstructor="keyword_avoider";;

let unveil =function
  Leaf(s,atm)->(leaf_sconstructor,[],Some atm,None)
 |Chain(_,l)->(chain_sconstructor,l,None,None)
 |Ordered_disjunction(_,l)->(ordered_disjunction_sconstructor,l,None,None)
 |Keyword_avoider(_,(x,l))->(keyword_avoider_sconstructor,[x],None,Some(l));;

let chained_version=function
Chain(_,l)->l
|x->[x];;

let rec flatten x=match x with
  Chain(_,l)->
      List.flatten (Image.image flatten l)
 |Ordered_disjunction(_,l)->
      if List.length l =1
      then flatten(List.hd l)
      else [x]
 |_->[x];;

let print (x:t)="rn \""^(name x)^"\"";;

let print_out (fmt:Format.formatter) (x:t)=
   Format.fprintf fmt "@[%s@]" (print x);;