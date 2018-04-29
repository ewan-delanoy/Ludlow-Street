(*

#use"Php_analizer/HRecognizer/abstractified_nonatomic_hrecognizer.ml";;

Unfortunate coupling with the type definition in nonatomic_hrecognizer module,
but I couldn't find anything better so far.

*)

type t=
  Leaf of string*Atomic_hrecognizer.t
 |Chain of string*(string list)
 |Ordered_disjunction of string*(string list)
 |Star of string*string
 |Maybe of string*string
 |Keyword_avoider of string*(string*(string list))
 |Motionless of string*(string list);;

 let name=function
 Leaf(s,_)->s
 |Chain(s,_)->s
 |Ordered_disjunction(s,_)->s
 |Star(s,_)->s
 |Maybe(s,_)->s
 |Keyword_avoider(s,_)->s
 |Motionless(s,_)->s;;